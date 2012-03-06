package SVN::BranchExport::Reader;

=head1 SVN::BranchExport::Reader

    my $reader = SVN::BranchExport::Reader->new(
        file => "my_svn_dump.dump",
    );

    while ( my $revision_data = $reader->get_revision_data ) {
        # process revision
    };

Read and sanitise revisions from an SVN dump.
To read from STDIN, specify file => "-".

=cut

use Moose;
use feature "switch";

use SVN::Dump;
use Date::Parse;
use URI::Escape;

has file           => ( is => "ro", required => 1   , isa => "Str" );
has _dump          => ( is => "ro", lazy_build => 1 , isa => "SVN::Dump" );
has _revision      => ( is => "rw", default => undef, isa => "Maybe[SVN::Dump::Record]" );


sub _build__dump { SVN::Dump->new({ file => shift->file }) };


my %processed_actions = (
    add     => [           "add" ],
    delete  => [ "delete"        ],
    replace => [ "delete", "add" ],
    change  => [ "change",       ],
);

=head2

    my $revision_data = $reader->get_revision_data;

Gets a sanitised representation of the next revision in the file.
Each revision looks like:

    {
        revision        => 5,
        author          => "andrew",
        log             => "Initial commit"
        date            => 123456789, # unix time
        nodes           => \@nodes, # see below
        mergeinfo_lines => 12,
    }

"mergeinfo_lines" is the total number of mergeinfo lines in this
revision.  Nodes are included in the order they appear in the dump,
apart from records-within-records - they appear as a separate record
after the parent record.  Each node looks like this:

    {
        path          => "branches/foo/README.txt",
        directory     => [ "branches", "foo" ],
        file          => "README.txt",
        kind          => "file",
        action        => "add", # can only be "add" or "delete"
        copyfrom_path => "trunk",
        copyfrom_rev  => 8,
        mergeinfo     => \@mergeinfo, # see below
    }

The "kind" value is "file" or "dir", or "(not specified)" (on deletion).

The "mergeinfo" property combines "svn:mergeinfo" and "svk:merge",
so consumers of this class only need to parse one common format.  Each
merge looks like:

    {
        uuid      => "abc-123-def-456",
        path      => "trunk",
        revisions => "1:10",
    }

Each merge is only included once, the first time it is seen.
The "revisions" string is documented here:
http://www.collab.net/community/subversion/articles/merge-info.html
As a special case, a range of "-N" (e.g. "-10") means "everything up
to and including revision N"
The "uuid" value is undef for merges from the current repository.

=cut
sub get_revision_data {

    my $self = shift;

    my $dump = $self->_dump;
    my $revision = $self->_revision;
    my @nodes;
    my $mergeinfo_lines = 0;

    # This will be set again later, unless we're at/past the end of the file:
    $self->_revision( undef );

    my $record = $dump->next_record;
    while ( $record ) {

        given ( $record->type() ) {

            when ( "node" ) {

                my $raw_action = $record->get_header( "Node-action" );

                # e.g. "replace" -> ( "delete", "add" )
                foreach my $action ( @{$processed_actions{ $raw_action }} ) {

                    my $path = $record->get_header( "Node-path" );
                    $path =~ s|^/||;

                    my %args = (
                        path          => $path,
                        kind          => $record->get_header( "Node-kind"   ),
                        action        => $action,
                        copyfrom_path => $record->get_header( "Node-copyfrom-path" ),
                        copyfrom_rev  => $record->get_header( "Node-copyfrom-rev"  ),
                    );

                    # Force the "kind" never to be specified for "delete",
                    # rather than deal with a special case:
                    $args{kind} = "(not specified)" if $action eq "delete";

                    # Set the file and directory:
                    if ( $args{kind} eq "file" ) {
                        die $path unless $path =~ s/^(.*?)([^\/]*)$/$1/;
                        $args{file} = $2;
                    };
                    $args{directory} = [ split( "/", $path ) ];

                    my @mergeinfo;


                    if (
                        # sometimes SVN::Dump reports a property exists and is undefined,
                        # when actually it doesn't exist:
                        $record->has_prop    ( "svn:mergeinfo" ) &&
                        $record->get_property( "svn:mergeinfo" )
                        ) {

                        my @merge_lines = split( "\n", $record->get_property( "svn:mergeinfo" ) );

                        push(
                            @mergeinfo,
                            map(
                                {
                                    /^\/*(.*?):\s*(.*)$/;
                                    my ( $path, $revisions ) = ( $1, $2 );

                                    $path =~ s/^\/+//; # sometimes users add a leading '/'
                                    $path =~ s/\/+$//; # sometimes users add a trailing '/'

                                    {
                                        uuid      => undef,
                                        path      => $path,
                                        revisions => $revisions,
                                        text      => $_,
                                    };
                                }
                                @merge_lines
                            )
                        );

                    };

                    # svnmerge.py output is broadly similary to svn:mergeinfo
                    # the algorithm below is based on the official migration script:
                    # http://svn.apache.org/repos/asf/subversion/trunk/contrib/client-side/svnmerge/svnmerge-migrate-history.py
                    foreach my $property (qw/ svnmerge-blocked svnmerge-integrated /) {

                        if (
                            # sometimes SVN::Dump reports a property exists and is undefined,
                            # when actually it doesn't exist:
                            $record->has_prop    ( $property ) &&
                            $record->get_property( $property )
                            ) {

                            my @merge_lines = split( /\s+/, $record->get_property( $property ) );

                            push(
                                @mergeinfo,
                                map(
                                    {
                                        /^\/*(.*?):\s*(.*)$/;
                                        my ( $path, $revisions ) = ( $1, $2 );
                                        $path = uri_unescape( $path );
                                        $path =~ s/^\/+//; # sometimes users add a leading '/'
                                        $path =~ s/\/+$//; # sometimes users add a trailing '/'
                                        {
                                            uuid      => undef,
                                            path      => $path,
                                            revisions => $revisions,
                                            text      => $_,
                                        };
                                    }
                                    @merge_lines
                                )
                            );

                        };

                    };

                    my $svk_merge =
                        (
                         $record->has_prop    ( "svk:merge" ) &&
                         $record->get_property( "svk:merge" )
                        );

                    if ( defined($svk_merge) ) {

                        while ( $svk_merge =~ /(([-[:xdigit:]]+):\/*([^\n]*|[^:]*):([0-9]+)($|\n+))/sg ) {
                            my ( $text, $uuid, $path, $revision ) = ( $1, $2, $3, $4 );

                            $path =~ s/^\/+//; # sometimes users add a leading '/'
                            $path =~ s/\/+$//; # sometimes users add a trailing '/'
                            $path =~ s/\n//g; # sometimes newlines creep in for some reason

                            push(
                                @mergeinfo,
                                {
                                    uuid      => ( $uuid eq $dump->uuid ) ? undef : $uuid,
                                    path      => $path,
                                    revisions => "-$revision",
                                    text      => $text, # useful for debugging
                                }
                            );
                        };

                    };

                    if ( @mergeinfo ) {
                        $mergeinfo_lines += @mergeinfo;
                        $args{mergeinfo} = \@mergeinfo
                            if $args{kind} eq "dir";
                    };

                    push( @nodes, \%args );}
                ;

            };

            when ( "revision" ) {

                # The revision record occurs before the nodes within it,
                # so we store this for next time:
                $self->_revision( $record );

                if ( $revision ) {
                    # not on the first run
                    last;
                } else {
                    # this is our first run - do another loop to get the actual data:
                    $revision = $record;
                };

            };

        };

        $record = $record->get_included_record // $dump->next_record;

    };

    if ( $revision ) {

        my $date = str2time( $revision->get_property( "svn:date"        ) );

        return {
            revision        => $revision->get_header  ( "Revision-number" ),
            author          => $revision->get_property( "svn:author"      ) // "(no author)",
            log             => $revision->get_property( "svn:log"         ),
            date            => $date,
            nodes           => \@nodes,
            mergeinfo_lines => $mergeinfo_lines,
        };
    } else {
        return undef;
    };

};

sub uuid { shift->_dump->uuid };

__PACKAGE__->meta->make_immutable;
