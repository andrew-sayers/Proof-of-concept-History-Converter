package SVN::BranchExport::ActionBuilder;

=head1 SVN::BranchExport::ActionBuilder

    my $action_builder = SVN::BranchExport::ActionBuilder->new(
        root_directory => $dirflows{""},
        commands       =>
            [
                # Note: commands must be sorted by add_revision:
                { command => "add"   , path => "foo/trunk", add_revision =>  1, },
                { command => "delete", path => "foo/trunk",  rm_revision => 10, },
                # ...
            ],
    );

    my $actions = $action_builder->process_revision( $output_from_log_reader );

    foreach my $action ( @actions ) {
        # process each action for the revision
    };

Each action is a hash of the form:

    {
        command       => "create branch" | "delete branch" | "deactivate branch" | "merge",
        path          => "branch/foo",
        probability   => 0.5, # in the range 0..1, indicating the probability this action is real
        comment       => "reason why this probability was given", # not included when probability=1

        # for "create branch" and "merge":
        from_branch   => "trunk",
        from_rev      => 122, # copyfrom rev or highest fully-merged rev

        # for "merge":
        unpicked_revs => \@unpicked, # revisions that have not even been cherry-picked

    }

The "probability" value is necessary for e.g. actions guessed from the log message.

=cut

use Moose;
use feature "switch";
use List::Util qw/ max /;
use List::MoreUtils qw/ uniq /;

use Readonly;

sub no_match { qr/(?!)/ };

has root_directory      => ( is => "ro", required => 1        , isa => "SVN::BranchExport::DirFlow", );
has commands            => ( is => "ro", required => 1        , isa => "ArrayRef[HashRef]" );

has ignored_pattern     => ( is => "ro", default => \&no_match, isa => "RegexpRef" );
has highlight_merges    => ( is => "ro", required => 1        , isa => "Bool" );

# Non-deleted path -> DirFlow mappings:
has     live_branches   => ( is => "ro", default => sub { {} }, isa =>          "HashRef[SVN::BranchExport::DirFlow]"  );
# Most recent path -> DirFlow mappings:
has     path_branches   => ( is => "ro", default => sub { {} }, isa =>          "HashRef[SVN::BranchExport::DirFlow]"  );
# Per-revision path -> DirFlow mappings:
has revision_branches   => ( is => "ro", default => sub { [] }, isa => "ArrayRef[HashRef[SVN::BranchExport::DirFlow]]" );

has  all_branch_pattern => ( is => "rw", default => \&no_match, isa => "RegexpRef" );
has live_branch_pattern => ( is => "rw", default => \&no_match, isa => "RegexpRef" );

has svk_depots          => ( is => "ro", required => 1        , isa => "HashRef[Int]" );

# Dirflow merged to -> path merged from -> merge string
has known_merges        => ( is => "ro", default => sub { {} }, isa => "HashRef[HashRef[HashRef[Int]]]" );

sub process_revision {

    my ( $self, $revision ) = @_;

    my $revision_no = $revision->{revision};
    my $root_directory = $self->root_directory;

    my @actions;

    my %changes;
    my $changed_branches = $self->revision_branches->[$revision_no] = {};

    #
    # STEP ONE: Process commands
    #

    my $commands = $self->commands;

    while ( @$commands &&
            ( $commands->[0]->{add_revision} //  $commands->[0]->{rm_revision} ) <= $revision_no
            ) {

        my $command = shift @$commands;

        given ( $command->{command} ) {

            when ( "add" ) {
                push(
                    @actions,
                    $self->_add_branch(
                        $changed_branches,
                        %$command,
                        author => $revision->{author},
                    )
                );
            };

            when ( "delete" ) {
                push( @actions, $self->_delete_branch( %$command ) );
            };

        };

    };

    #
    # STEP TWO: Process nodes
    #

    my %warned_paths;
    foreach my $node ( @{$revision->{nodes}} ) {

        # Each node creates zero or more actions:
        push(
            @actions,
            $self->_process_node(
                $node, \%warned_paths, $changed_branches, \%changes,
                $revision,
            )
        );

    };


    #
    # STEP THREE: Try to guess actions from the log message
    #

    return ( \@actions, "no branches changed" ) unless %$changed_branches;

    if ( %$changed_branches && !$revision->{mergeinfo_lines} && $revision->{log} ) {

        push(
            @actions,
            $self->_parse_log( $changed_branches, $revision ),
        );

    };

    my    @changes;
    push( @changes, $changes{add   } . " added"  ) if $changes{add};
    push( @changes, $changes{change} . " changed") if $changes{change};
    push( @changes, $changes{delete} . " deleted") if $changes{delete};

    my $branch_count = keys( %$changed_branches );
    my $summary =
        join( ", ", @changes ) .
            " in " . $branch_count .
            ( ( 1 == $branch_count ) ? " branch" : " branches" )
        ;

    return ( \@actions, $summary );

};

# Add or delete a branch from the various places it needs to be stored:
sub _add_branch {

    my ( $self, $changed_branches, %args ) = @_;

    my $branch = $self->root_directory->add( %args );

    my $path = $branch->path;

    $self->path_branches->{$path} = $branch;
    $self->live_branches->{$path} = $branch;
    $changed_branches   ->{$path} = $branch;

    $self->_update_patterns;

    return {
        command     => "create branch",
        branch      => $branch,
        path        => $path,
        probability => 1,

        from_branch => $args{copied_from} ? $args{copied_from}->path : undef,
        from_rev    => $args{copyfrom_rev},
    };

};

sub _delete_branch {

    my ( $self, %args ) = @_;

    my ( $path, $revision_no ) = @args{qw/ path rm_revision /};

    my $branch = delete $self->live_branches->{$path};

    $self->root_directory->delete(
        path        => $branch->path,
        rm_revision => $revision_no,
    );

    $self->_update_patterns;

    return {
        command     => $branch->has_unique_content ? "deactivate branch" : "delete branch",
        branch      => $branch,
        path        => $path,
        probability => 1,
    };

};

sub _update_patterns {

    my $self = shift;

    # Match the longest patterns first (so that approximate
    # matches pick the right branch name):
    my @all_branches = sort( { length($b) <=> length($a) } keys %{$self->path_branches} );
    my $all_branch_pattern =
        @all_branches
        ? join( "|", @all_branches )
        : "(?!)"
        ;
    $self->all_branch_pattern( qr{(?:\/?($all_branch_pattern)\/?)} );

    # Match the longest patterns first (so that approximate
    # matches pick the right branch name):
    my @live_branches = sort( { length($b) <=> length($a) } keys %{$self->live_branches} );
    my $live_branch_pattern =
        @live_branches
        ? join( "|", @live_branches )
        : "(?!)"
        ;
    $self->live_branch_pattern( qr{(?:\/?($live_branch_pattern)\/?)} );

};

sub _process_node {

    my ( $self, $node, $warned_paths, $changed_branches, $changes, $revision ) = @_;

    my $revision_no = $revision->{revision};

    my $root_directory = $self->root_directory;

    my $live_branch_pattern = $self->live_branch_pattern;
    my  $all_branch_pattern = $self->all_branch_pattern;

    my $is_ignored = $node->{path} =~ $self->ignored_pattern;

    if (         $node->{action       } eq "add" &&
         defined($node->{copyfrom_path})         &&
         !$is_ignored
        ) {

        # (possibly) copy a branch

        my $copyfrom = $root_directory->get(
            path         => $node->{copyfrom_path},
            add_revision => $node->{copyfrom_rev},
            allow_create => 0,
        );

        # if the path wasn't found, this is probably a subdirectory of a branch:

        if ( $copyfrom ) {

            # Looks like a branch (or an ancester of one)

            my @dirflows = ( $copyfrom ) if $copyfrom;
            my @ret;

            while ( @dirflows ) {

                my $maybe_branch = shift( @dirflows );

                if ( $maybe_branch->path =~ m{^$all_branch_pattern$} ) {

                    # make a branch from this path

                    # Change e.g. "/copiedfrom/child/branchname" to "/copiedto/child/branchname":
                    my $path =
                        $node->{path} .
                        "/" .
                        substr( $maybe_branch->path, length( $copyfrom->path ) );
                    $path =~ s/^\///;
                    $path =~ s/\/$//;

                    push(
                        @ret,
                        $self->_add_branch(
                            $changed_branches,
                            path         => $path,
                            copied_from  => $maybe_branch,
                            copyfrom_rev => $node->{copyfrom_rev},
                            add_revision => $revision_no,
                            author       => $revision->{author},
                        )
                    );

                } else {

                    # Check children to see if they're branches

                    push( @dirflows, @{$maybe_branch->children_at( $node->{copyfrom_rev} )} );

                };

            };

            return @ret if @ret;

        } elsif ( $node->{path} !~ m{^$live_branch_pattern(?:$|/)} ) {

            if ( $node->{copyfrom_path} =~ m{^$live_branch_pattern(?:$|/)} ) {

                # Looks like we created a new branch from branch's sub-directory
                # e.g. svn cp trunk/my_work branches/myproject
                # I guess this happens when a bit of work grows out of control?
                # For want of a better representation, we treat this as a branch.

                my $copyfrom = $root_directory->get(
                    path         => $1,
                    add_revision => $node->{copyfrom_rev},
                    allow_create => 0,
                );

                return
                    $self->_add_branch(
                        $changed_branches,
                        path         => $node->{path},
                        copied_from  => $copyfrom,
                        copyfrom_rev => $node->{copyfrom_rev},
                        add_revision => $revision_no,
                        author       => $revision->{author},
                    );
            };

            print STDERR
                "$node->{path}\@$revision_no was copied from $node->{copyfrom_path}\@$node->{copyfrom_rev}, " .
                "but is not on any live branch - do you need to add some trunks?";

        };

    };

    if (
        $node->{action} eq "delete"
        ) {

        # delete branch(es)

        my $delete = $root_directory->get(
            path         => $node->{path},
            add_revision => $revision_no,
            allow_create => 0,
        );

        # if the path wasn't found, this is probably a subdirectory of a branch:
        my @dirflows = ( $delete ) if $delete;
        my @ret;

        while ( @dirflows ) {

            my $maybe_branch = shift( @dirflows );

            if ( $maybe_branch->path =~ m{^$live_branch_pattern$} ) {

                # delete this branch

                push(
                    @ret,
                    $self->_delete_branch(
                        path        => $maybe_branch->path,
                        rm_revision => $revision_no,
                    )
                );

            } else {

                # Check children to see if they're branches

                push( @dirflows, @{$maybe_branch->children_at( $revision_no )} );

            };

        };

        return @ret if @ret;

    };

    if (
        $node->{action   } eq "change" &&
        $node->{mergeinfo} &&
        $node->{path     } =~ m{^$live_branch_pattern$}
        ) {

        # merge branch

        my $branch = $root_directory->get(
            path         => $1,
            add_revision => $revision_no,
            allow_create => 0,
        );

        my %merged;

        my $known_merges = $self->known_merges->{$branch->id} //= {};

        foreach my $merge_line ( @{$node->{mergeinfo}} ) {

            my $revisions = $merge_line->{revisions};
            my $path      = $merge_line->{path};
            my $uuid      = $merge_line->{uuid} // "";

            # We don't care about non-inheritable mergeinfo:
            $revisions =~ s/\*//g;

            if ( $known_merges->{"$uuid:$path"}->{$revisions}++ ) {
                --$revision->{mergeinfo_lines};
                next;
            };

            foreach my $range ( split( /\s*,\s*/, $revisions ) ) {

                my ( $from, @range );

                # There are three basic formats for individual merge commands:
                if ( $range =~ s/^-([0-9]+)$/$1/ ) { # everyting up to the specified revision

                    $from = $root_directory->get(
                        path         => $path,
                        add_revision => $range,
                        allow_create => 0,
                    );

                    if ( $from ) {
                        @range = ( $from->add_revision .. $range );
                    } else {
                        @range = ( $range );
                    };

                } elsif ( $range =~ /^([0-9]+)\-([0-9]+)$/ ) { # everything between the specified revisions

                    @range = ( $1 .. $2 );

                    my $min_from = $root_directory->get(
                        path         => $path,
                        add_revision => $range[0],
                        allow_create => 0,
                    );

                    $from = $root_directory->get(
                        path         => $path,
                        add_revision => $range[-1],
                        allow_create => 0,
                    );

                    if ( $range[0] == 1 && !defined($min_from) && defined($from) ) {

                        # svnmerge.py (not tested with svn 1.5) will let you merge
                        # "$branch:r1-rN" to mean "all revisions in $branch":
                        $range[0] = $from->add_revision;

                    } elsif ( $min_from != $from ) {

                        # This probably (hopefully!) never happens in the real world:
                        die
                            "Merging a single range from multiple flows is not currently supported.\n" .
                            "Please report this bug."
                            ;

                    };

                } else { # just the specified revision

                    $from = $root_directory->get(
                        path         => $path,
                        add_revision => $range,
                        allow_create => 0,
                    );

                    @range = ( $range );

                };

                if ( $uuid && ($self->svk_depots->{$uuid} // 0) < $range[-1] ) {

                    # Merge from an SVK depot that has already diverged
                    --$revision->{mergeinfo_lines};

                } elsif ( $from ) {
                    my $merge =
                        $branch->merge(
                            revision    => $revision_no,
                            from_branch => $from,
                            from_revs   => \@range,
                            probability => 1,
                            comment     => "Merge from metadata",
                        );
                    $merged{$from->id} = $merge
                        if defined($merge);
                } else {
                    s/(^|:)/${1}r/g foreach @range;
                    $merged{"???_$path"} //= {
                        command       => "merge",
                        probability   => 0.1,
                        comment       => "Bad merge metadata: cannot find $path",
                        branch        => $branch,
                        path          => $branch->path,
                        unpicked_revs => \@range,
                    };
                };
            };

        };

        return values %merged
            if %merged;

    };

    if ( $node->{path} =~ m{^$live_branch_pattern/} ) {

        # change branch

        my $name = $1;
        my $branch = $changed_branches->{$name} //=
            $root_directory->get(
                path         => $name,
                add_revision => $revision_no,
                allow_create => 0,
            );

        $branch->change( $revision_no, $revision->{author} );

        ++$changes->{ $node->{action} };

        $branch->has_files( 1 )
            if $node->{kind} eq "file";

    } elsif (
        $node->{action} eq "add" &&
        $node->{kind} eq "file" &&
        !$is_ignored
        ) {

        my $path = $node->{path};
        $path =~ s/[^\/]+$//;
        $path =~ s|/$||;

        print STDERR
            "\r$node->{path}\@$revision_no is not on any live branch - do you need to add some trunks?\n"
            unless $warned_paths->{$path}++;

    };

    return ();

};



#
# LOG PARSING STUFF BELOW THIS POINT
#


# Matches any string that looks like "merge":
Readonly::Scalar my $merge => qr/
    \b

    # words that have been seen before "merge" that don't modify its meaning:
    (?:
       (?:carefully|-)\s+
    )?

    # forms of the word "merge":
    merg(?:e|ed|es|eing|ing)

    \b

    # words that have been seen after "merge" that don't modify its meaning:
    (?:
       \s+
       (?: in \s+ changes \s+ from
         | changes \s+ in \s+ from
         | changes        \s+ from
         | in \s+ the \s+ following
         | in
       )
       \b
       \s*
       |
    )

/ix;


# Matches a string that strongly indicates a revision (e.g. "r123"):
Readonly::Scalar my $revision => qr/
    \b
    (?:r|rev|revisions?\s*:*)+
    \s* (?:[0-9]+)
    \b
/ix;

# Matches a string that weakly indicates a revision ( e.g. "123"):
Readonly::Scalar my $maybe_revision => qr/
       (?:
          $revision | \b[0-9]+\b
       )
/ix;

# Matches a string that strongly indicates a list of revisions (e.g. "revisions 1-10"):
Readonly::Scalar my $revision_list => qr/

    # must start with something that strongly indicates a revision:
    (?:$revision)

    # and continue with zero or more things that look fairly like revisions:
    (?:

       \s*
       (?:
           [-,:\s]
           |
           \s+ (?: an | and | to | through ) \s+
       )
       \s*

       $maybe_revision

    )*

/ix;

# Matches a string that weakly indicates a list of revisions (e.g. "1 through 10"):
Readonly::Scalar my $maybe_revision_list => qr/

    $maybe_revision

    # and continue with zero or more things that look fairly like revisions:
    (?:

       \s*
       (?:
           [-,:\s]
           |
           \s+ (?: and | to | through ) \s+
       )
       \s*
       $maybe_revision

    )*

/ix;

# Matches a string of uninteresting characters that can appear
# at the end of a log message (e.g. " ... ")
Readonly::Scalar my $log_suffix => qr/
    (?:
          [[:punct:]\s]* $ # e.g. "blah blah blah. "
       |
       \s*[[:punct:]]+\s*\n # e.g. "blah blah:\nblah"
    )
/x;

# Common probabilities for actions:
Readonly::Scalar my $very_high => 0.90;
Readonly::Scalar my      $high => 0.75;
Readonly::Scalar my   $average => 0.50;
Readonly::Scalar my       $low => 0.25;
Readonly::Scalar my  $very_low => 0.10;


# Tries to extract meaningful merge information from a log message:
sub _parse_log {

    my ( $self, $changed_branches, $revision ) = @_;

    my $branch_pattern = $self->all_branch_pattern;

    my $log = $revision->{log};
    # Remove anything that looks like a date:
    $log =~ s{
        (?:
            [0-9]{4}-[0-9]{2}-[0-9]{2} # date
            (?:
                \s*
                [0-9]{2}:[0-9]{2}:[0-9]{2} # time
                (?:
                    \s*
                    [+-][0-9]{4} # timezone
                  |
                )
              |
            )

            |

            \(
              (?:Mon|Tue|Wed|Thu|Fri|Sat|Sun),
              \s
              [0-9]{1,2}
              \s
              (?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)
              \s
              [0-9]{4}
            \)

        )
    }{(date)}gx;

    # Reduce anything that looks like an SVN log message to the revision number:
    $log =~ s{
        ^
        \s*
        (r[0-9]+)            \s \| \s
        [^|]+                \s \| \s
        \(date\) \s \(date\) \s \| \s
        (?:1\sline|[0-9]+\slines)
        $
    }{$1}gxm;

    # IMPLEMENTATION NOTE: don't be a hero!
    #
    # This function has two purposes:
    # - parse the 90% of obvious patterns so that people don't have to
    # - flag up the remaining possible patterns so that people are able to
    #
    # Before adding clever regexps below, consider the effect it will
    # have on the signal/noise ratio.  Common patterns can afford to
    # be lax, but rare patterns need to be very strict in order to
    # avoid drowning the user in useless log messages.

    given ( $log ) {

        # svnmerge writes merges in a standard format: http://www.orcaware.com/svn/wiki/Svnmerge.py
        # In theory we should have metadata and never get here, but just in case:
        when (/^$merge\s+($revision_list)\s+via\s+svnmerge\s+from(?:http:\/\/[^ ]*$branch_pattern)\n/i) {

            return $self->_guess_merge(
                path             => $2,
                revisions        => $1,
                from             => "svnmerge log message",
                probability      => $very_high,

                changed_branches => $changed_branches,
                revision         => $revision,
            );

        };

        # e.g. "merge from trunk: r123":
        when (/^$merge(?:\s+revisions?)\s+from\s+$branch_pattern\s*:\s*($maybe_revision_list)$log_suffix/i) {

            return $self->_guess_merge(
                path             => $1,
                revisions        => $2,
                from             => "whole log message",
                probability      => $high,

                changed_branches => $changed_branches,
                revision         => $revision,
            );

        };

        # e.g. "merge r123 from trunk":
        when (/^$merge\s+($maybe_revision_list)\s+from(?:\s+the)?\s+$branch_pattern$log_suffix/i) {

            return $self->_guess_merge(
                path             => $2,
                revisions        => $1,
                from             => "whole log message",
                probability      => $high,

                changed_branches => $changed_branches,
                revision         => $revision,
            );

        };

        # e.g. "merge r123":
        when (/^$merge\s+($maybe_revision_list)$log_suffix/i) {

            return $self->_guess_merge(
                # no path
                revisions        => $1,
                from             => "whole log message",
                probability      => $average,

                changed_branches => $changed_branches,
                revision         => $revision,
            );

        };

        # e.g. "merge up to r123":
        when (/^$merge\s+up\s+to\s+($maybe_revision)$log_suffix/i
              && !$#{$self->revision_branches->[$1]}
            ) {

            my $revision = $1;
            my $branch = (values %{$self->revision_branches->[$revision]})[0];

            return $self->_guess_merge(
                # no path
                revisions        => $branch->add_revision . ":$revision",
                from             => "whole log message",
                probability      => $average,

                changed_branches => $changed_branches,
                revision         => $revision,
            );

        };

        # e.g. "merge changes in from trunk":
        when ( /^$merge$branch_pattern$log_suffix/ ) {

            return $self->_guess_merge(
                path             => $1,
                # no revisions
                from             => "whole log message",
                probability      => $average,

                changed_branches => $changed_branches,
                revision         => $revision,
            );

        };

        when( /^ +r[0-9]+\@[^ ]+ \(orig r[0-9]+\):  [^ ]+ \| \(date\)\n/ ) {

            # SVK log messages seem to be of the following form:
            #
            #  r234@hostname (orig r123):  user | date
            #  message
            #
            # We assume that we are the "orig" for revisions pushed
            # directly to us, but not for indirect merges:
            #
            #  r234@host1 (orig r123):  user | date
            #   r345@host2 (orig r233):  user | date

            # Find all the "orig" lines:
            my @revisions;
            my @branches;
            while ( /^ +r[0-9]+\@[^ ]+ \(orig r([0-9]+)\):  [^ ]+ \| \(date\)$/gm ) {
                my $branch = (values %{$self->revision_branches->[$1]})[0];
                if ( $branch ) {
                    push( @revisions, $branch->add_revision . ":$1" );
                    push( @branches, $branch );
                };
            };

            # Downgrade the probability if there are any indirect merges:
            my $probability =
                /^  +r[0-9]+\@[^ ]+ \(orig r([0-9]+)\):  [^ ]+ \| \(date\)$/
                ? $low
                : $average;

            return $self->_guess_merge(
                path             => $#branches ? undef : $branches[0]->path,
                revisions        => join( ",", @revisions ),
                from             => "SVK merge message",
                probability      => $probability,

                changed_branches => $changed_branches,
                revision         => $revision,
            );

        };

        # e.g. "This commit merges everything in from trunk":
        when ( /$merge/ && /[0-9]+|$branch_pattern/ ) {

            # This is a last-ditch attempt to produce something
            # the user will find more readable than a log message.

            my @paths = /$branch_pattern/g;

            my $max_revision = $revision->{revision};

            my %from_revs;
            foreach my $rev ( /($maybe_revision_list|[0-9]+)/g ) {
                next unless $rev; # ignore revision 0
                next if $rev =~ /^[0-9]+$/ && $rev >= $max_revision;
                next if $from_revs{$rev}++;
                foreach my $r ( $rev =~ /([0-9]+)/g ) {
                    push( @paths, keys(%{$self->revision_branches->[$r]}) );
                };
            };

            my ( $an, $bn );
            my @from_revs = sort(
                {
                    $a =~ /([0-9]+)/ ; $an = $1;
                    $b =~ /([0-9]+)/ ; $bn = $1;
                    $an <=> $bn;
                }
                keys %from_revs
            );

            @paths = uniq( @paths );
            @paths = grep( { !$changed_branches->{$_} } @paths );
            @paths = sort( @paths );

            if ( @paths ) {

                if ( @paths == 1 && @from_revs == 1 &&
                     $from_revs[0] =~ /^[0-9]+$/ &&
                     /\bup\s+to\s+$from_revs[0]\b/i
                    ) {
                    my $from_branch = $self->path_branches->{$paths[0]};

                    @from_revs = ( $from_branch->add_revision . ":" . $from_revs[0] );
                };

                return
                    map(
                        {
                            $self->_guess_merge(
                                path             => $_,
                                revisions        => @from_revs ? join( ",", @from_revs ) : undef,
                                from             => "fragments of the log message",
                                probability      => $low,

                                changed_branches => $changed_branches,
                                revision         => $revision,
                            );
                        }
                        @paths,
                    );

            } else {

                $self->_highlight_merge( $revision );

                my $from_rev = max( map( { /([0-9]+)/g } @from_revs ) );

                return map(
                    {
                        {
                            command     => "merge",
                            probability => 0,
                            comment     => "Probably not a merge",
                            from_rev    => $from_rev,
                            branch      => $_,
                            path        => $_->path,
                        }
                    }
                    values %$changed_branches,
                );

            };

        };

        # e.g. "Fix a bunch of old merge conflicts":
        when ( /$merge/ ) {

            $self->_highlight_merge( $revision );

            return map(
                {
                    {
                        command     => "merge",
                        probability => 0,
                        comment     => "Probably not a merge",
                        branch      => $_,
                        path        => $_->path,
                    }
                }
                values %$changed_branches,
            );

        };

    };

};


# Guess the merges from at least one of "path" and "revisions":
sub _guess_merge {

    my ( $self, %args ) = @_;

    my $path      = $args{path};
    my $revisions = $args{revisions};
    my $revision  = $args{revision};
    my $probability = $args{probability};

    my $root_directory = $self->root_directory;

    my $from_branch;
    my $problem;

    my @from_revs;

    $self->_highlight_merge( $revision )
        if $probability <= $low;

    #
    # STEP ONE: Find the branch and revisions that were merged:
    #

    if ( defined($path) && !defined($revisions) ) {

        $from_branch = $self->path_branches->{$path};

        my $from_revs = $from_branch->changed_revisions;
        @from_revs = map( { defined($from_revs->[$_]) ? $_ : () } 0..$#$from_revs );

    } else {

        my %from_revs;

        # Disambiguate the message:
        $revisions =~ s/(?:,|and)/ /gi;
        $revisions =~ s/revisions?\s*:*|revision|rev|r/ /gi;
        $revisions =~ s/\s+/ /g;
        $revisions =~ s/^ //;
        $revisions =~ s/ $//;
        $revisions =~ s/\s*(?:-|:|to|through)\s*/:/gi;

        # parse revisions out:
        my @ranges;
        foreach my $range ( split( " ", $revisions ) ) {

            my ( $min, $max );

            if ( $range =~ /^[0-9]+$/ ) {
                $min = $max = $range;
            } elsif ( $range =~ /^([0-9]+):([0-9]+)/ ) {
                $min = $1;
                $max = $2;
            } else {
                die "Can't parse \"$revisions\"";
            };

            if ( length($max) < length($min) ) {
                # Change "12345-56" to "12345-12356":
                my $maybe_max = substr( $min, length($min) - length($max) ) . $max;
                $max = $maybe_max if $maybe_max > $min;
            };

            push( @ranges, [ $min, $max, $range ] );

        };

        # Higher numbers are more likely to be actual revisions
        # (e.g. "merge 123 from 1.2.3" is probably about merging r123 into branches/1.2.3)
        @ranges = sort( { $b->[1] <=> $a->[1] || $b->[0] <=> $a->[0] } @ranges );

        foreach my $range ( @ranges ) {

            my ( $min, $max, $range_string ) = @$range;

            unless ( defined($from_branch) ) {

                if ( defined($path) ) {

                    # Get the branch from the path

                    $from_branch   = $root_directory->get(
                        path         => $path,
                        add_revision => $max,
                        allow_create => 0,
                    );

                    $from_branch //= $root_directory->get(
                        path         => $path,
                        add_revision => $min,
                        allow_create => 0,
                    );

                } else {

                    # Get the branch that was changed in both mentioned revisions

                    my @min_branches = values(%{$self->revision_branches->[$min]});
                    my @max_branches = values(%{$self->revision_branches->[$max]});

                    if      ( !@min_branches ) { $problem //= "Revision $min did not change any branches";
                    } elsif ( $#min_branches ) { $problem //= "Revision $min changed multiple branches"  ;
                    } elsif ( !@max_branches ) { $problem //= "Revision $max did not change any branches";
                    } elsif ( $#max_branches ) { $problem //= "Revision $max changed multiple branches"  ;
                    } elsif ( $min_branches[0] != $max_branches[0] ) {
                        $problem //= "Revisions $min and $max changed different branches";
                    } else {
                        $from_branch = $min_branches[0];
                    };

                };

            };

            if ( $from_branch ) {

                my $changed = $from_branch->changed_revisions;

                if ( $from_branch->add_revision > $min ) {
                    $problem //= "trying to merge r$min, " .
                        "but the branch wasn't added until r" .
                        $from_branch->add_revision
                        ;
                    $min = $from_branch->add_revision;
                };

                if ( $from_branch->rm_revision &&
                          $from_branch->rm_revision <= $max ) {
                    $problem //= "trying to merge r$max, " .
                        "but the branch was already deleted in r" .
                        $from_branch->rm_revision
                        ;
                    $max = $from_branch->rm_revision-1;
                };

                unless ( $changed->[$max] ) {
                    $problem //= $from_branch->path . " wasn't changed in r$max";
                    --$max while $min > $max && !$changed->[$max];
                };

                unless ( $changed->[$min] ) {
                    $problem //= $from_branch->path . " wasn't changed in r$min";
                    --$min while $min > $max && !$changed->[$min];
                };

                foreach my $r ( $min..$max ) {
                    $from_revs{$r} = 1
                        if defined( $changed->[$r] );
                };

            } else {

                $problem //=
                    "Could not guess branch from revision(s) '$range_string'";

            };

        };

        @from_revs = sort( { $a <=> $b } keys %from_revs );

    };

    #
    # STEP TWO: Generate a list of actions:
    #

    my $changed_branches = $args{changed_branches};

    unless ( @from_revs ) {
        return
            map(
                {
                    {
                        command     => "merge",
                        probability => $very_low,
                        comment     => "Probably a merge with a typo: " . $problem,
                        branch      => $_,
                        path        => $_->path,
                    }
                }
                values %$changed_branches,
            );

    };

    if ( defined($problem) ) {

        # Couldn't get a sensible revision list:

        if ( $probability <= $low ) {
            $probability = $very_low;
        } else{
            $self->_highlight_merge( $revision );
            $probability = $low;
        };

        if ( $from_branch ) {
            return
                map(
                    {
                        $_->merge(
                            revision    => $revision->{revision},
                            from_branch => $from_branch,
                            from_revs   => \@from_revs,
                            probability => $probability,
                            comment     => "Probably a merge with a typo: " . $problem,
                            dry_run     => 1,
                            );
                    }
                    values %$changed_branches,
                );
        } else {
            return
                map(
                    {
                        {
                            command     => "merge",
                            probability => $probability,
                            comment     => "Probably a merge with a typo: " . $problem,
                            branch      => $_,
                            path        => $_->path,
                        }
                    }
                    values %$changed_branches
                );
        };

    } else {

        if ( keys(%$changed_branches) == 1 ) {

            # Everything looks good:

            my $to_branch = (values %$changed_branches)[0];

            my $comment;
            my $from = $args{from};
            given ( $probability ) {
                when ( $very_high ) { $comment = "Very strong guess from $from" };
                when (      $high ) { $comment =      "Strong guess from $from" };
                when (   $average ) { $comment =     "Average guess from $from" };
                when (       $low ) { $comment =        "Weak guess from $from" };
                when (  $very_low ) { $comment =   "Very weak guess from $from" };
                default             { $comment =             "Guess from $from" };
            };

            return
                $to_branch->merge(
                    revision    => $revision->{revision},
                    from_branch => $from_branch,
                    from_revs   => \@from_revs,
                    dry_run     => 1,
                    probability => $probability,
                    comment     => $comment,
                );

        } else {

            # More than one branch was changed in the current revision:

            my $probability = $average;
            $probability = $very_low if $probability <= $probability;

            return
                map(
                    {

                        $_->merge(
                            revision    => $revision->{revision},
                            from_branch => $from_branch,
                            from_revs   => \@from_revs,
                            probability => $probability,
                            comment     => "Not sure which branch(es) were merged to.",
                            dry_run     => 1,
                        );

                    }
                    values %$changed_branches
                );

        };

    };

};

sub _highlight_merge {

    my ( $self, $revision ) = @_;

    # Make the "merge" message easier to spot in the logs:
    $revision->{log} =~ s/($merge)/"<".uc($1).">"/ge
        if $self->highlight_merges && !$revision->{merges_highlighted}++;

};

__PACKAGE__->meta->make_immutable;
