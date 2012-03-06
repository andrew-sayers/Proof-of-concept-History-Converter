package SVN::BranchExport::DirFlow;

=head1 SVN::BranchExport::DirFlow

    my $dirflow = SVN::BranchExport::DirFlow->new(
        path         => "branches/foo",
        parent       => $dirflows{"branches"},
        copied_from  => $dirflows{"trunk"},
        copyfrom_rev => 42,
        add_revision => 43,
    );

Represents a flow associated with a directory path.

Deleting a directory and creating a new one with the same name
counts as a new flow.  For details, see:
http://svn.apache.org/repos/asf/subversion/trunk/notes/dump-load-format.txt

=head2 DATA STRUCTURES

A key data structure used here is the "revision hash":

    my %some_revision_hash = (  1=>1,  2=>1,  4=>0,  12=>0  );

This is a hash where each key represents a revision
(e.g. $some_revision_hash{1} represents r1).  Each value can be one
of:

    undef - no decision has been made about this revision yet
    0     - the algorithm has decided about this revision, in the negative
    1     - the algorithm has decided about this revision, in the affirmative

The meaning of "negative" and "affirmative" are specific to the
algorithm in question.  For example, _add_changed_revisions() uses it to
mean "does this revision still need to be included?".

=cut

use Moose;
use feature "switch";
use List::Util "max";

#
# Properties that describe where this directory fits in the directory tree:
#

has id               => ( is => "ro", lazy_build => 1      , isa => "Str", );
has path             => ( is => "ro", required => 1        , isa => "Str", );
has parent           => ( is => "ro", required => 1        , isa => "Maybe[SVN::BranchExport::DirFlow]", );
has         children => ( is => "ro", default => sub { {} }, isa => "HashRef[SVN::BranchExport::DirFlow]", );
has deleted_children => ( is => "ro", default => sub { {} }, isa => "HashRef[ArrayRef[SVN::BranchExport::DirFlow]]", );
has has_files        => ( is => "rw", default => 0         , isa => "Bool" );

#
# Properties that describe where this flow fits in the history:
#

has   copied_from => ( is => "ro", default => undef     , isa => "Maybe[SVN::BranchExport::DirFlow]", );
has   copied_to   => ( is => "ro", default => sub { [] }, isa => "ArrayRef[SVN::BranchExport::DirFlow]", );
has  copyfrom_rev => ( is => "ro", default => undef     , isa => "Maybe[Int]", );
has  add_revision => ( is => "ro", required => 1        , isa => "Int", );
has   rm_revision => ( is => "rw", required => 0        , isa => "Int", );

# The top-level branch this was originally copied from:
has trunk         => ( is => "rw", required => 0, isa => "SVN::BranchExport::DirFlow", predicate => "has_trunk" );
# A file that indicates this is a branch:
has branch_file   => ( is => "rw", required => 0, isa => "Str", );

# Revision hash representing the revisions in which this directory was changed:
has changed_revisions => ( is => "ro", lazy_build => 1      , isa => "ArrayRef", );
# Hash of revision hashes representing the revisions in that have been merged in here:
# (including revisions merged in implicitly when a merge revision is itself merged in)
has merged_revisions  => ( is => "ro", lazy_build => 1      , isa => "HashRef[HashRef[Int]]", );
# Hash of last-mergeable revisions:
has _last_mergeable   => ( is => "ro", lazy_build => 1      , isa => "HashRef[Int]", );

has last_author       => ( is => "rw", required   => 1      , isa => "Str", );

# Live branches that contain all the changes made in this branch
# (either because they're a copy or because the branch was merged there):
has duplicated_to     => ( is => "rw", default => sub { {} }, isa => "HashRef[SVN::BranchExport::DirFlow]", );
has duplicated_from   => ( is => "rw", default => sub { {} }, isa => "HashRef[SVN::BranchExport::DirFlow]", );

sub _build_changed_revisions {
    my @ret;
    $ret[shift->add_revision] = 1;
    return \@ret;
};
sub _build_merged_revisions {
    my $self = shift;
    return { $self->id => { $self->add_revision => 1 } };
};
sub _build__last_mergeable {
    my $self = shift;
    return
        $self->copied_from
        ? { $self->copied_from->id => $self->copyfrom_rev }
        : {}
        ;
};

# This structure roughly resembles the svn:mergeinfo block:
# [ # the outer array has one entry per revision number
#   [ # the inner array has one entry per branch merged in this revision
#     [ $other, 1, 3, 5, 7, ] # the innermost array has one entry per revision merged
#   ],
#   [
#     [ $other, 2, 4, 6, 8, ] # unlike svn:mergeinfo, revisions don't accumulate
#   ]
# ]
has _merge_info => ( is => "ro", default => sub { [] }, isa => "ArrayRef[HashRef]" );



# Repo-wide unique ID, used internally to e.g. store dirflows in hashes:
sub _build_id {
    my $self = shift;
    return $self->add_revision . "_" . $self->path;
};

# Utility function to get the name of a child directory:
sub _get_name {
    my ( $parent, $name ) = @_;
    if ( "" eq $parent ) {
        # we're in the root directory, do not prepend a "/":
        return $name;
    } else {
        # non-root directory - make sure there's a "/" before the name:
        return "$parent/$name";
    };
};

# Utility function to detect whether this was a typo directory.
# For example:
#     svn add tronk
#     svn ci
#     svn mv tronk trunk
#     svn ci -m "oops"
# We would want to treat "trunk" as a trunk branch,
# and ignore that it was actually copied from "tronk".
sub empty {
    my $self = shift;

    return
        !%{$self->children} &&
        !%{$self->deleted_children} &&
        !$self->has_files &&
        $self->rm_revision;

};

=head2 children_at

    my $children = $dirflow->children_at( 42 );

Returns the list of children that were live at the specified revision

=cut
sub children_at {

    my ( $self, $revision ) = @_;

    my @ret =
        grep(
            { $_->add_revision <= $revision }
            values(%{$self->children})
        );

    foreach my $children ( values(%{$self->deleted_children}) ) {
        foreach my $child ( @$children ) {
            push( @ret, $child )
                if $child->add_revision <= $revision &&
                   $child-> rm_revision >  $revision;
        };
    };

    return \@ret;

};


=head2 within_branch

    my $branch_dirflow = $dirflow->within_branch;

Return the nearest ancestor (not including $self) that looks like a
branch, or undef if no ancestors look like branches.

=cut

sub within_branch {

    my $self = shift;

    for ( my $dirflow = $self->parent; $dirflow; $dirflow = $dirflow->parent ) {
        return $dirflow if $dirflow->has_trunk;
    };

    return undef;

};


=head2 add

    $dirflow->add(
        path         => "branches/foo",
        copied_from  => $dirflows{"trunk"},
        copyfrom_rev => 42,
        add_revision => 43,
        author       => "user_name",
    );

Add a new dirflow to the tree.  Returns the new dirflow.

=cut

sub add {

    my ( $self, %args ) = @_;

    $args{directory} //= [ split( "/", $args{path} ) ];

    my $name = pop( @{$args{directory}} );

    my $dirflow = $self->get( %args, allow_create => 1, );

    if ( $dirflow->children->{$name} ) {
        warn
            "Created " . $args{path} . " without deleting the previous flow\n" .
            "Please report this bug.";
    };

    my $added = $dirflow->children->{$name} =
        SVN::BranchExport::DirFlow->new(
            %args,
            last_author => $args{author},
            parent => $dirflow,
        );

    if ( my $copied_from = $args{copied_from} ) {

        # Copy things over...

        # If the original had a trunk, we should too
        # (though not necessarily the same one):
        if ( $copied_from->has_trunk ) {
            $added->guess_trunk( $copied_from->branch_file, $args{add_revision} );
        };

        # Children should be copied over, based on the revision we were copied from:
        while ( my ( $name, $child ) = each( %{$copied_from->children} ) ) {

            if ( $child->add_revision <= $args{copyfrom_rev} ) {

                $added->add(
                    path         => _get_name( $args{path}, $name ),
                    directory    => [ $name ],
                    copied_from  => $child,
                    copyfrom_rev => $args{copyfrom_rev},
                    add_revision => $args{add_revision},
                    author       => $args{author},
                );
            };
        };

        # that includes children that have been deleted in the latest version:
        while ( my ( $name, $children ) = each( %{$copied_from->deleted_children} ) ) {

            next if $copied_from->children->{$name};

            foreach my $child ( @$children ) {

                if ( $child->add_revision <= $args{copyfrom_rev} &&
                     $child->rm_revision  >  $args{copyfrom_rev}
                    ) {
                    $added->add(
                        path         => _get_name( $args{path}, $name ),
                        directory    => [ $name ],
                        copied_from  => $child,
                        copyfrom_rev => $args{copyfrom_rev},
                        add_revision => $args{add_revision},
                        author       => $args{author},
                    );
                };

            };

        };

        $added->has_files( $copied_from->has_files );

        push( @{$copied_from->copied_to}, $added );

	if ( $args{copyfrom_rev} >= $copied_from->last_changed ) {
	    $copied_from->duplicated_to->{ $added->id } = $added;
	    $added->duplicated_from->{ $copied_from->id } = $copied_from;
	};

    };

    return $added;

};


=head2 delete

    $dirflow->delete(
        path        => "branches/foo",
        rm_revision => 42,
    );

Remove this from the directory tree, but don't destroy the object.

Returns a truthy value if this dirflow itself was deleted,
or a falsey value if a subdirectory was delted.

=cut

sub delete {

    my ( $self, %args ) = @_;

    $args{directory} //= [ split( "/", $args{path} ) ];

    my $rm_revision = $args{rm_revision};

    my $child = shift( @{$args{directory}} );

    if ( defined($child) ) {

        my $dirflow = $self->children->{$child};

        if ( $dirflow ) {

            if ( $dirflow->delete( %args ) ) {
                delete $self->children->{$child};
                push(
                    @{$self->deleted_children->{$child}},
                    $dirflow,
                );
            };

        } else {

            if ( $self->deleted_children->{$child} ) {

                die
                    "While looking for $args{path} in revision $rm_revision," .
                    " found that " . $self->path . " -> " . $child .
                    " had already been deleted.  Please report this bug."
                    ;

            } else {

                die
                    "While looking for $args{path} in revision $rm_revision," .
                    " found that " . $self->path . " -> " . $child .
                    " has never existed.  Please report this bug."
                    ;

            };

        };

        return 0;

    } else {

        $self->rm_revision( $rm_revision );
        foreach my $child ( values %{$self->children} ) {
            $child->delete(
                directory   => [],
                rm_revision => $rm_revision
            );
        };

        if ( $self->has_trunk && $self->trunk->empty ) {

            # If this turns out to be an empty branch,
            # Undo any trunk relationships pointing here:

            my @copied_to = @{$self->copied_to};

            while ( @copied_to ) {

                my $dirflow = shift @copied_to;
                push( @copied_to, @{$dirflow->copied_to} );

                if ( $dirflow->has_trunk && $dirflow->trunk == $self->trunk ) {
                    $dirflow->guess_trunk( $dirflow->branch_file, $rm_revision );
                };

            };
        };

	foreach my $from ( values %{$self->duplicated_from} ) {
	    delete $from->duplicated_from->{ $self->id };
	};

        return 1;

    };

};


=head2 get

    my $dirflow = $root_dirflow->get(
        path         => "branches/foo",
        add_revision => 43,
        allow_create => 1,
        author       => "user_name",
    );

Get the currently active dirflow with the specified path,
adding parent directories as necessary.

The "allow_create" argument controls the behaviour when the node (or a
parent) isn't found.  With allow_create => 1, the node will be created
based on the arguments passed.  Otherwise ->get will return undef.

=cut

sub get {

    my ( $self, %args ) = @_;

    my $dirflow = $self;

    $args{directory} //= [ split( "/", $args{path} ) ];

  DIRECTORY:
    while ( @{$args{directory}} ) {

        my $name = shift( @{$args{directory}} );

        my $revision = $args{add_revision};

        if ( my $child = $dirflow->children->{$name} ) {
            if ( $revision >= $child->add_revision ) {
                $dirflow = $child;
                next DIRECTORY;
            };
        };

        foreach my $child ( @{$dirflow->deleted_children->{$name}} ) {
            next unless $revision >= $child->add_revision;
            next unless $revision <  $child-> rm_revision;
            $dirflow = $child;
            next DIRECTORY;
        };

        if ( $args{allow_create} ) {

            $dirflow = $dirflow->children->{$name} =
                SVN::BranchExport::DirFlow->new(
                    path         => _get_name( $dirflow->path, $name ),
                    parent       => $dirflow,
                    add_revision => $args{add_revision},
                    last_author  => $args{author},
                );

        } else {

            return undef;

        };

    };

    return $dirflow;

};


=head2 change

    $dirflow->change( 42 );

Register a revision that changed this directory.

=cut

sub change {
    my ( $self, $revision, $author ) = @_;
    do {
        $self->changed_revisions            ->[$revision] = 1;
        $self->merged_revisions->{$self->id}->{$revision} = 1;
        $self->last_author( $author );
	if ( $self->duplicated_to ) {
	    foreach my $from ( values %{$self->duplicated_to} ) {
		delete $from->duplicated_from->{ $self->id };
	    };
	    $self->duplicated_to( {} );
	};
    } while $self = $self->parent;
};



=head2 guess_trunk

    my $trunk = $dirflow->guess_trunk( $path, $revision );

Assume this directory is (on) a branch, and find the trunk branch
associated with it.

For best results, this should be called the first time a file is added
within this directory (but not subdirectories).  The result is cached,
so you can call it every time a file is added if you like.

Note that this guess isn't perfect - for example, a directory labelled
as a trunk could later have an ancestor directory labelled as a trunk.

The "path" and "revision" arguments are used mainly to produce more
uselful error messages for users.

=cut

sub guess_trunk {

    my ( $self, $path, $revision ) = @_;

    $self->branch_file( $path );

    # First, check if the decision has already been made:
    if ( $self->has_trunk ) {
        return $self->trunk;
    };

    # Second, find the original directory this was copied from:
    if ( my $copied_from = $self->copied_from ) {
        unless ( $copied_from->empty ) {
            $self->trunk( $copied_from->guess_trunk( $path, $revision ) );
            $self->_find_recursive_branches( $revision );
            return $self->trunk;
        };
    };

    # Finally, find the original directory the nearest ancestor was copied from:
    my $ancestor = $self;
    my $trunk;
    while ( $ancestor = $ancestor->parent ) {

        # Get a cached trunk:
        if ( $ancestor->has_trunk ) {
            $trunk = $ancestor->trunk;
            last;
        };

        if ( my $copied_from = $ancestor->copied_from ) {
            unless ( $copied_from->empty ) {
                $trunk = $copied_from->trunk;
                last;
            };
        };

    };

    if ( $trunk ) {

        # Set trunk for dirflows below trunk but above here:
        for ( my $dirflow = $self; $dirflow != $ancestor; $dirflow = $dirflow->parent ) {
            $dirflow->branch_file( $path );
            $dirflow->trunk( $trunk );
        };

        return $self->trunk( $trunk );

    } else {

        # Neither this nor any ancestor was copied - this is probably a trunk.

        $self->trunk( $self );
        $self->_find_recursive_branches( $revision );

        return $self;

    };

};

sub _find_recursive_branches {

    my ( $self, $revision ) = @_;

    # Do nothing if this is obviously not actually a trunk:
    return if $self->trunk->within_branch;

    for ( my $dirflow = $self->parent; $dirflow; $dirflow = $dirflow->parent ) {
        $self->_check_recursive_branch( $dirflow, $revision );
    };

    my @descendents = (
        values %{$self->children},
        map( { @$_ } values(%{$self->deleted_children}) ),
    );

    while ( @descendents ) {

        my $dirflow = shift @descendents;

        unless ( $self->_check_recursive_branch( $dirflow, $revision ) ) {
            push(
                @descendents,
                values %{$dirflow->children},
                map( { @$_ } values(%{$dirflow->deleted_children}) ),
            );
        };

    };

};

sub _check_recursive_branch {

    my ( $self, $dirflow, $revision ) = @_;

    if ( $dirflow->has_trunk &&
         $dirflow->trunk != $self->trunk &&
         !$dirflow->trunk->within_branch
        ) {
        print STDERR
            "In r$revision, found branch \"", $self->path, "\" within branch \"", $dirflow->path, "\".\n";

        foreach my $branch ( $self, $dirflow ) {
            print STDERR
                "\t\"", $branch->path, "\"\n",
                "\t\tbelongs to trunk \"", $branch->trunk->path, "\" (created in r", $branch->trunk->add_revision, "),\n",
                "\t\twhich looks like a trunk because it contains \"",
                $branch->trunk->branch_file, "\"\n",
                ;
        };

        print STDERR
            "\tYou may want to rerun this script ignoring one the above files used to detect trunks,\n",
            "\tor one of the following files used to detect branches:\n",
            "\t\t", $dirflow->branch_file, "\n",
            "\t\t", $self   ->branch_file, "\n",
            "\tFor now, we assume the parent directory is the real branch.\n",
            "\n",
            ;

        return 1;

    } else {

        return 0;

    };

};

=head2 merge

    my $unmerged_data = $dirflow->merge(
        from_branch => $other_dirflow,
        revision    => 42,
        from_revs   => [ 1, 3, 5, 7, 9, 11, 13, 17, 19, ],
    );

Register revisions from another dirflow being merged into this one.

Returns undef if nothing was merged, or a hash otherwise:
{
    command       => "merge",
    probability   => 1,
    comment       => "Merge from metadata",
    branch        => $self,
    path          => $self->path,
    from_branch   => $from_branch,
        from_rev  => 13,
    unpicked_revs => [ 15, "23:41" ],
}

The "from_rev" value represents the highest-numbered revision that
has been completely merged in to this dirflow.  The "unpicked_revs"
key represent the revisions that have not even been cherry-picked.

The hash is formatted in a way used internally by ::ActionBuilder.

=cut
sub merge {

    my ( $self, %args ) = @_;

    my ( $revision, $from_branch, $from_revs, $probability, $comment, $dry_run ) =
        @args{qw/ revision from_branch from_revs probability comment dry_run /};

    #
    # STEP ONE: get the list of new revisions to merge:
    #

    my $changed_revisions = $from_branch->changed_revisions;
    my $merged_revisions = $self->merged_revisions->{ $from_branch->id } //= {};

    # get new revisions that really exist in the other branch:
    # (e.g. "merge 1:10" becomes "merge 2, 4, 6, 8, 10" if only odd revisions touched the branch)
    my @from_revs = grep(
        {
            defined($changed_revisions->[$_]) &&
            !exists($merged_revisions->{$_})
        }
        @$from_revs,
    );

    return unless scalar(@from_revs);

    #
    # STEP TWO: get indirect revisions (if some revisions were themselves merges)
    #

    my @merged_revisions;

    # Calculate any merges registered but not yet calculated (e.g. the add revision):
    $self->_add_changed_revisions( $self->merged_revisions );

    if ( $dry_run ) {

        # Don't modify the original data, or look for indirect revisions:

        @merged_revisions = ( keys %$merged_revisions, @from_revs );

    } else {

        my %previously_merged = %$merged_revisions;

        # Add the new revisions:
        @{$merged_revisions}{@from_revs} = map( 1, @from_revs );

        # merge them and any revisions merged in those revisions (etc.):
        $from_branch->_add_changed_revisions( $self->merged_revisions );

        @merged_revisions = keys %$merged_revisions;

        # get the complete list of newly-merged revisions:
        my @newly_merged =
            (
             $from_branch,
             grep( { !exists($previously_merged{$_}) } @merged_revisions ),
            );

        push( @{$self->_merge_info->[$revision]}, \@newly_merged );

    };

    #
    # STEP THREE: Calculate the return values
    #

    my $last_mergeable = 0;
    my $prev_last_mergeable = $self->_last_mergeable->{ $from_branch->id } // 0;
    my $needs_attention = $dry_run;

    # Revision hash with 1 => unmerged, 0 => merged
    my @unmerged_revisions = @$changed_revisions;
    @unmerged_revisions[@merged_revisions] = map( 0, @merged_revisions );

    my $highest_merged_revision = max( 0, @merged_revisions );

    my $n = max( $prev_last_mergeable, $from_branch->add_revision );

    while ( $n <= $highest_merged_revision ) {

        given ( $unmerged_revisions[$n] ) {
            when ( undef ) {};
            when ( 1     ) { last };
            default        { $last_mergeable = $n; };
        };

        ++$n;

    };

    $needs_attention = 1
        if $last_mergeable <= $prev_last_mergeable;

    $self->_last_mergeable->{ $from_branch->id } = $last_mergeable
        unless $dry_run;

    my @unpicked;

    while ( $n <= $highest_merged_revision ) {

        if ( $unmerged_revisions[$n] ) {

            my $unpicked_lowest = $n;
            my $unpicked_highest = $n;

            while ( ++$n <= $highest_merged_revision ) {
                my $r = $unmerged_revisions[$n];
                if ( defined($r) ) {
                    if ( $r ) {
                        $unpicked_highest = $n;
                    } else {
                        push(
                            @unpicked,
                            ( $unpicked_lowest == $unpicked_highest )
                            ? "r$unpicked_lowest"
                            : "r$unpicked_lowest:r$unpicked_highest"
                        );
                        $unpicked_lowest = 0;
                        last;
                    };
                };
            };

            push(
                @unpicked,
                ( $unpicked_lowest == $unpicked_highest )
                ? "r$unpicked_lowest"
                : "r$unpicked_lowest:r$unpicked_highest"
            )
                if $unpicked_lowest;

        };

        ++$n;

    };

    if ( $n < $#unmerged_revisions ) {
        until ( $unmerged_revisions[$n] ) { ++$n };
        my $unpicked_lowest = $n;
        my $unpicked_highest = $#unmerged_revisions;
        push(
            @unpicked,
            ( $unpicked_lowest == $unpicked_highest )
            ? "r$unpicked_lowest"
            : "r$unpicked_lowest:r$unpicked_highest"
        );
    };

    # Try harder to suggest a revision if it will be checked manually
    # anyway, since it's more important to be informative than correct:
    if ( $needs_attention &&
        $last_mergeable < $from_revs->[-1] ) {
        $last_mergeable = $from_revs->[-1];
    };

    if ( $last_mergeable == $self->last_changed && !$dry_run ) {
	$self->duplicated_from->{ $from_branch->id } = $from_branch;
	$from_branch->duplicated_to->{ $self->id } = $self;
    };

    return {
        command         => "merge",
        probability     => $probability,
        comment         => $comment,
        branch          => $self,
        path            => $self->path,
        from_branch     => $from_branch,
            from_rev    => $last_mergeable,
        unpicked_revs   => \@unpicked,
        needs_attention => $needs_attention,
    };

};


# _add_changed_revisions
#
#     my $hash_of_revision_hashes = (
#         $dirflow->id => $my_revision_hash,
#     );
#
#     $dirflow->_add_changed_revisions( $hash_of_revision_hashs );
#
# Adds revisions that have been changed, given an initial set of changed
# revisions.  A description of the "revision hash" data structure is
# provided above, but here's an example:
#
#     {
#         # revisions 1 and 5 need to be included; r3 has already been
#         "1_/trunk/foo" => { 1 => 1, 5 => 1, 3 => 0 },
#     }
sub _add_changed_revisions {

    my $self       = shift;
    our $revisions = shift;

    our %dirflows_to_add = ();

    my $this_dirflow_revisions = $revisions->{ $self->id };

    sub _add_dirflow {
        # Add one dirflow to the hash:
        my ( $dirflow, @new_revisions ) = @_;
        my $id = $dirflow->id; # caching this attribute boosts performance
        my $dirflow_revisions  = $revisions->{ $id } //= {};
        foreach my $r ( @new_revisions ) {
            if ( $dirflow_revisions->{$r} //= 1 ) {
                $dirflows_to_add{ $id } = $dirflow;
            };
        };
    };

    # Include all the revisions from the dirflow we were copied from:
    if ( $self->copied_from && $this_dirflow_revisions->{ $self->add_revision } ) {

        my $copyfrom_revisions = $self->copied_from->changed_revisions;
        my @copyfrom_revisions;
        for ( my $n=$self->copied_from->add_revision; $n<=$self->copyfrom_rev; ++$n ) {
            push( @copyfrom_revisions, $n ) if defined $copyfrom_revisions->[$n];
        };

        _add_dirflow( $self->copied_from, @copyfrom_revisions );

        # Don't do this yet, in case it's needed in the next loop:
        #$this_dirflow_revisions->[$n] = 0; # NO!

    };

    # Add all the revisions that were merged in during any relevant revision:
    my ( $r, $merged );
    while ( ( $r, $merged ) = each( %$this_dirflow_revisions ) ) {
        if ( $merged ) {
            if ( my $merge_info = $self->_merge_info->[$r] ) {
                foreach my $merge ( @$merge_info ) {
                    _add_dirflow( @$merge );
                };
            };
            $this_dirflow_revisions->{$r} = 0;
        };
    };

    # mark the add revision done, if it was (only) included in the first loop:
    if ( $this_dirflow_revisions->{ $self->add_revision } ) {
        $this_dirflow_revisions->{ $self->add_revision } = 0;
    };

    # Process all the revisions we've touched:
    my @dirflows = values(%dirflows_to_add);
    foreach my $dirflow ( @dirflows ) {
        $dirflow->_add_changed_revisions( $revisions );
    };

    return $revisions;

};

sub last_changed {
    return $#{shift->changed_revisions};
};

# Returns a truthy value if there are changes in this branch that
# haven't been copied or merged into any other branch:
sub has_unique_content {
    my $self = shift;
    return !$self->empty && !%{$self->duplicated_to};
};

__PACKAGE__->meta->make_immutable;
