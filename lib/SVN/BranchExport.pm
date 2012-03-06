package SVN::BranchExport;

use warnings;
use strict;
use feature "switch";

use SVN::BranchExport::DirFlow;
use SVN::BranchExport::Reader;
use SVN::BranchExport::ActionBuilder;
use SVN::BranchExport::Writer;
use SVN::BranchExport::Makefile;

sub configure {

    my ( %args ) = @_;

    my ( $infile, $outfile, $ignored_patterns, $progress ) =
        @args{qw/ infile outfile ignored_patterns progress /};

    my $ignored_pattern =
        @$ignored_patterns
        ? join( "|", @$ignored_patterns )
        : "(?!)";
    $ignored_pattern = qr/$ignored_pattern/;

    my $reader =
        SVN::BranchExport::Reader->new(
            file => $infile // "-",
	);

    my %guessed_trunks;
    my $root_directory =
        SVN::BranchExport::DirFlow->new(
            path         => "",
            parent       => undef,
            copied_from  => undef,
            copyfrom_rev => undef,
            add_revision => 0,
            last_author  => "",
        );

    my %authors;
    my %svk_depots;
    my %tag_patterns;
    my $action_count = 0;

    while ( my $revision = $reader->get_revision_data ) {

        $authors{$revision->{author}} //=
            "$revision->{author} <$revision->{author}\@" . $reader->uuid . ">"
            if defined($revision->{author});

        $progress->( $revision->{revision} );

        $action_count += @{$revision->{nodes}} + 1;

        foreach my $node ( @{$revision->{nodes}} ) {

            foreach my $merge_line ( @{$node->{mergeinfo}} ) {
                if ( defined($merge_line->{uuid}) ) {

                    my $depot = $svk_depots{$merge_line->{uuid}} //= {
                        uuid             => $merge_line->{uuid},
                        author           => $revision->{author},
                        diverged_before  => $revision->{revision},
                        merge_candidates => [],
                    };

                    my $revision_no = $merge_line->{revisions};
                    $revision_no =~ s/^-//;

                    if ( $revision_no <= $depot->{diverged_before} ) {

                        my $merged_from = $root_directory->get(
                            path         => $merge_line->{path},
                            add_revision => $revision_no,
                            allow_create => 0,
                            author       => $revision->{author},
                        );

                        if ( $merged_from ) {
                            push( @{$depot->{merge_candidates}}, $revision_no );
                        } else {
                            $depot->{diverged_before} = $revision_no;
                        };
                    };

                };
            };

            next if $node->{path} =~ $ignored_pattern;

            given ( $node->{action} ) {

                when ( "add" ) {

                    if ( $node->{kind} eq "dir" ) {

                        my $copied_from;
                        if ( $node->{copyfrom_path} ) {

                            $copied_from = $root_directory->get(
                                %$node,
                                directory => [ split( "/", $node->{copyfrom_path} ) ],
                                add_revision => $node->{copyfrom_rev},
                                allow_create => 0,
                                author       => $revision->{author},
                            );

                            if ( $node->{path} =~ m{^tags/} ) {
                                $tag_patterns{"^tags/"} = "";
                            } elsif ( $node->{path} =~ m{^(.*?)/tags/} ) {
                                $tag_patterns{"^$1/tags/"} = "$1/";
                            };

                        };

                        $root_directory->add(
                            %$node,
                            copied_from  => $copied_from,
                            add_revision => $revision->{revision},
                            author       => $revision->{author},
                        );

                    } else {

                        my $trunk = $root_directory->get(
                            %$node,
                            add_revision => $revision->{revision},
                            allow_create => 1,
                            author       => $revision->{author},
                        )->guess_trunk( $node->{path}, $revision->{revision} );

                        $guessed_trunks{$trunk->id} //= $trunk;

                    };

                };

                when ( "delete" ) {

                    if ( $node->{kind} eq "(not specified)" ) {
                        # "kind" usually isn't specified for deletion,
                        # so we search for it instead:

                        my @directory = @{$node->{directory}};

                        $node->{kind} =
                            $root_directory->get(
                                %$node,
                                add_revision => $revision->{revision},
                                allow_create => 0,
                                author       => $revision->{author},
                            )
                            ? "dir"
                            : "file"
                            ;

                        # Reset the directory that was modified by the call to get:
                        $node->{directory} = \@directory;

                    };

                    if ( $node->{kind} eq "dir" ) {

                        # Find directory, delete
                        $root_directory->delete(
                            %$node,
                            rm_revision => $revision->{revision},
                        );

                    };

                };

            };

        };

    };


    # Return the guessed trunks:
    my @trunks = grep( { !$_->within_branch } values( %guessed_trunks ) );

    my @depots;
    foreach my $depot ( values %svk_depots ) {
        my $candidates = $depot->{merge_candidates};
        $depot->{merge_candidates} =
            grep( { $_ < $depot->{diverged_before} } @$candidates );
        push( @depots, $depot ) if @$candidates;
    };
    @depots = sort { $b->{merge_candidates} <=> $a->{merge_candidates} } @depots;

    SVN::BranchExport::Makefile::write(
        infile           => $infile,
        outfile          => $outfile,
        ignored_patterns => $ignored_patterns,
        trunks           => \@trunks,
        svk_depots       => \@depots,
        tag_patterns     => \%tag_patterns,
        authors          => \%authors,
        action_count     => $action_count,
    );

};


sub make {

    my ( %args ) = @_;

    my ( $infile, $progress, $start_time, $command_line ) =
        @args{qw/ infile progress start_time command_line /};

    my $make_vars = SVN::BranchExport::Makefile::read( $infile // "-" );

    my $total_actions = $make_vars->{action_count};
    my $actions_so_far     = 0;

    if ( defined($make_vars->{authors_file}) ) {
        open( my $fh, ">", $make_vars->{authors_file} )
            or die "$!: $make_vars->{authors_file}";
        print $fh map( { "$_\n" } @{$make_vars->{authors}} );
        close( $fh ) or die "$!: $make_vars->{authors_file}";
    };

    my $ignored_pattern =
        @{$make_vars->{ignored_patterns}}
        ? join( "|", @{$make_vars->{ignored_patterns}} )
        : "(?!)";

    my $reader =
        SVN::BranchExport::Reader->new(
            file => $make_vars->{dump_file} // "-",
    );

    my $root_directory =
        SVN::BranchExport::DirFlow->new(
            path         => "",
            parent       => undef,
            copied_from  => undef,
            copyfrom_rev => undef,
            add_revision => 0,
            last_author  => "",
        );

    my @commands = sort(
        {
            ( $a->{add_revision} // $a->{rm_revision} ) <=>
            ( $b->{add_revision} // $b->{rm_revision} )
        }
        @{$make_vars->{commands}}
    );

    my $action_builder = SVN::BranchExport::ActionBuilder->new(
        root_directory   => $root_directory,
        commands         => \@commands,
        ignored_pattern  => qr/$ignored_pattern/,
        svk_depots       => $make_vars->{svk_depots},
        highlight_merges => $make_vars->{highlight_merges},
    );

    my $writer = SVN::BranchExport::Writer->new(
        %args,
        %$make_vars
    );

    while ( my $revision = $reader->get_revision_data ) {

        $actions_so_far += @{$revision->{nodes}} + 1;

        $progress->( $revision->{revision}, $actions_so_far / $total_actions );
        my ( $actions, $summary ) = $action_builder->process_revision( $revision );

        foreach my $action ( @$actions ) {
            $writer->write( $revision, $action, $summary );
        };

    };

};

1;
