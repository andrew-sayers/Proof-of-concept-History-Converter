#!/usr/bin/env perl

=head1 Git Branch Importer - split directories into branches

This script takes a single-branch history (as created by e.g. svn-fe),
plus a description of the branch history in a format described below,
and creates a collection of branches.


=head2 BRANCH HISTORY FORMAT

SVN doesn't represent advanced history information in a particularly
useful way - for example, it doesn't record which directories should
be treated as branches.  A branch history file describes your
project's history in a human- and machine-readable format that can be
used while importing your project into another version control system.

Here is an example script:

    # The file starts with an optional "setup" section:

    This is a version 1 Git Branch History file

    Read revisions from "master"
    Save branches in "refs/remotes/myproject"
    Save tags in "refs/tags/myproject"
    Remove git-svn-id from log messages

    # The file continues with any number of commands:

    # Create a branch with no parent branch:
    In r1, create branch "trunk" as "master"

    # Create a branch with a parent branch (as "branches/foo")
    In r2, create branch "branches/foo" from "trunk" revision 1

    # Merge a branch back into another branch:
    In r4, merge "branches/foo" revision 2 into "trunk"
    # Delete this branch altogether:
    In r4, delete "branches/foo"

    # Create a new tag with a specific name:
    In r5, create tag "tags/foo" as "tag-foo" from "trunk" revision 4
    # Ignore any further commits, but don't delete the tag:
    In r6, deactivate "branches/foo"

The branch history format has been designed to be easy to create with
a script, and easy to check by hand before use.

The branch history format allows you to call something a "tag" instead
of a "branch", even though SVN "tags" are implemented the same way as
branches.  Be careful if you choose to create tags, as there's no
clean way to represent their behaviour in other version control
systems.

=head3 Example representations of tags

There are several popular ways to convert SVN "tags" to equivalents in
other version control systems.  In the following examples, we'll
assume that a new directory "tags/foo" was copied from "trunk" in
revision 10, changes were made to it in revision 11, then it was
deleted in revision 12 and a new tag with the same name was created in
revision 13.

If you want to create a single tag and let the import script decide
what to do (most flexible representation):

    In r10, create tag "tags/foo" from trunk revision 9
    In r12, deactivate "tags/foo"
    In r13, create tag "tags/foo" as "tags/foo@13" from trunk revision 9

If you want to convert all SVN tags to git branches (most
technologically accurate representation):

    In r10, create branch "tags/foo" from trunk revision 9
    In r12, deactivate "tags/foo"
    In r13, create branch "tags/foo" as "tags/foo@13" from trunk revision 9

If you want to create a single tag from the final revision (closest
representation to what the author was trying to say):

    In r13, create tag "tags/foo" from trunk revision 9

If you want to create a tag for each revision (most self-explanatory
representation):

    In r10, create tag "tags/foo" from trunk revision 9
    In r10, deactivate "tags/foo"
    In r11, create tag "tags/foo" as "tags/foo@11" from trunk revision 9
    In r11, deactivate "tags/foo"
    In r13, create tag "tags/foo" as "tags/foo@13" from trunk revision 9
    In r13, deactivate "tags/foo"

=head2 IMPLEMENTATION DETAILS

At the core of the script is a very simple loop around the "git-commit-tree"
plumbing command.  Here's some pseudocode to give you a rough idea:

foreach my $commit ( commits_in( $svn_fe_branch ) ) {

    my $messsage  = $commit->message;
    my $commit_id = $commit->sha1;

    foreach my $branch ( branches_edited_in( $commit ) ) {
        my $path   = $branch->path;
        my $parent = $branch->HEAD;
        my $branch_commit_id =
            `echo $message | git commit-tree $commit_id:$path -p $parent`;
        $branch->add_commit( $branch_commit_id );
    };

};

The above is a very rough sketch - the actual code has to deal with
complexities caused by efficiency, branching and merging, tags, etc.
See the `git-mktag` plumbing command for tag creation.

=head3 Tags

When the user allows the script to deal with tags however it likes,
this script treats them how they act - those that behave like tags are
represented as such; those that behave more like branches are
represented accordingly.

When a tag is first created, we check whether the directory associated
with it is the same as its parent commit.  If so, we create a normal
git tag pointing to the parent commit, with the revision message
converted to a tag message.  Otherwise, we create a new commit and a
lightweight tag.

When a normal tag is changed or branched from, we downgrade the tag to
an empty commit.  After that, tags are handled exactly like branches.

=cut

package Git::BranchImport::Reader;

# Read the branch import file to a series of commands

use warnings;
use strict;
use 5.10.0; # for "//"

use IO::Handle;

# Used to aid parsing:
my $quoted_string = qr/"((?:[^"]|\\")*)"/;

sub print_error {
    my $self = shift;
    print STDERR "Line " . $self->{fh}->input_line_number . ": ", @_, "\n";
};

# ACCESSOR:
sub revisions_from { shift->{revisions_from} };
sub git_svn_id     { shift->{git_svn_id}     };

=head2 new

    my $reader = Git::BranchImport::Reader->new(
        fh => \*STDIN,
    );

=cut
sub new {

    my ( $class, %args ) = @_;

    $args{revisions_from    } //= "HEAD";
    $args{_branches_in      } //= "refs/remotes/svn/";
    $args{    _tags_in      } //= "refs/tags/svn/";
    $args{_stashed_command  }   = undef;
    $args{_path_refs        }   = {};
    $args{_previous_revision}   = 1;
    $args{git_svn_id        }   = 1;

    my $fh = $args{fh};

    die "Please pass a 'fh' argument to Git::BranchImport::Reader->new"
        unless $fh;

    my $self = bless( \%args, $class );

    # Read the setup section:
    while ( <$fh> ) {

        chomp;

        if ( /^#/ || /^\s*$/ ) {

            # empty lines and comments are ignored

        } elsif ( /^This is a version (.*) Git Branch History file$/ ) {

            $self->print_error(
                "this is a version $1 file, but this program only supports version 1 (ignored).\n"
            )
                if $1 != 1;

        } elsif ( /^Read revisions from $quoted_string$/ ) {

            $self->{revisions_from} = $1;

        } elsif ( /^Save (tags|branches) in $quoted_string$/ ) {

            $self->{"${1}_in"} = $2;
            $self->{"${1}_in"} =~ s/\/*$/\//;

        } elsif ( /^Remove git-svn-id from log messages$/ ) {

            $self->{git_svn_id} = 0;

        } else {

            # Will need to read this back later:
            $self->{_stashed_command} = $self->_read_line( $_ );

            last;

        };

    };

    return $self;

};

=head2 get_command

    my $command = $reader->get_command();

Gets the next command in the stream, or undef at the end of the command stream.

A command is a hash of the form:

{
    command     => ( "create branch" | "create tag" | "merge" | "delete" | "deactivate" ),
    revision    => 123,              # revision number to apply this command to
    branch      => "refs/tags/foo",  # branch name to be used in the new VCS
    path        => "branches/foo",   # directory name used in SVN
    from        => "bar",            # directory in SVN for the parent commit
    from_branch => "refs/heads/bar", # branch name in new VCS for the parent commit
    from_rev    => 122,              # revision number for the "from" branch
}

Note: the "from" keys will be undefined or left out for commands where they
don't apply.

=cut
sub get_command {

    my $self = shift;

    # We might have to stash a command after reading the setup section:
    return delete( $self->{_stashed_command} )
        if defined( $self->{_stashed_command} );

    my $fh = $self->{fh};

    while ( <$fh> ) {
        chomp;
        my $command = $self->_read_line( $_ );
        return $command if $command;
    };

    return undef;

};

sub _read_line {

    my ( $self, $line ) = @_;

    $_ = $line;
    chomp;

    if ( /^#/ || /^\s*$/ ) {

        # empty lines and comments are ignored
        return undef;

    } elsif ( s/^In r([0-9]+), // ) {

        # All commands must begin with a revision identifier

        my $revision = $1;

        if ( $revision < $self->{_previous_revision} ) {
            $self->print_error(
                "revision $revision is earlier than previous revision $self->{_previous_revision} (ignored)"
            );
            return undef;
        } else {
            $self->{_previous_revision} = $revision;
        };

        my %command;

        if ( /^create branch $quoted_string(?: as $quoted_string)?(?: from $quoted_string(?: r([0-9]+))?)?$/ ) {

            %command = (
                command  => "create branch",
                path     => $1,
                branch   => $self->{_branches_in} . ( $2 // $1 ),
                from     => $3,
                from_rev => $4 // $revision,
            );

            $self->{_path_refs}->{$command{path}} = $command{branch};

        } elsif ( /^create tag $quoted_string(?: as $quoted_string)? from $quoted_string(?: r([0-9]+))?$/ ) {

            %command = (
                command  => "create tag",
                path     => $1,
                branch   => $self->{_tags_in} . ( $2 // $1 ),
                from     => $3,
                from_rev => $4 // $revision,
            );

            $self->{_path_refs}->{$command{path}} = $command{branch};

        } elsif ( /^merge $quoted_string(?: r([0-9]+))? into $quoted_string$/ ) {

            %command = (
                command => "merge",
                path     => $3,
                from     => $1,
                from_rev => $2 // $revision,
            );

        } elsif ( /^(delete|deactivate) $quoted_string$/ ) {

            %command = (
                command  => $1,
                path     => $2,
            );

        };

        if ( %command ) {

            # Clean up formatting, validate branch names etc.

            $command{revision} = $revision;

            # unquote the path:
            $command{path  } =~ s/\\([\\"])/$1/g;

            if ( defined($command{from}) ) {
                # unquote the from path:
                $command{from} =~ s/\\([\\"])/$1/g;
            } else {
                $command{from_rev} = undef;
            };

            if ( defined($command{branch}) ) {
                # unquote the branch:
                $command{branch} =~ s/\\([\\"])/$1/g;
            } else {
                # Get the branch from the path:
                if ( my $branch = $self->{_path_refs}->{$command{path}} ) {
                    $command{branch} = $branch;
                } else {
                    $self->print_error( "unknown branch '$command{path}' (ignored)" );
                    return undef;
                };
            };

            if ( defined($command{from}) && !defined($command{from_branch}) ) {
                if ( my $branch = $self->{_path_refs}->{$command{from}} ) {
                    $command{from_branch} = $branch;
                } else {
                    $self->print_error( "unknown branch '$command{from}' (ignored)" );
                    return undef;
                };
            };

            if ( defined($command{from_rev}) && $command{from_rev} > $revision ) {
                $self->print_error(
                    "revision $command{from_rev} is greater than current revision $revision (ignored)"
                );
                delete @command{qw/ from from_rev from_branch /};
            };

            return \%command;

        };

    };

    $self->print_error( "could not parse this line (ignored)" );
    return undef;

};



package Git::BranchImport::Writer;

# Write a series of commands to git

=head1

    $writer->new( commit => "HEAD" );

    while ( my $revision = $writer->next_revison ) {
        # do stuff with this revision
        while ( my $modification = $writer->next_modification ) {
            # do stuff with this modification
        };
    };

    # create a branch (tags are considered a type of branch):
    my $branch = $writer->branch(
        name => "refs/remotes/svn/foo",
        path => "branches/foo",
    );

    # Normal commit:
    $branch->commit;

    # Commit with extra parents:
    $other_branch->commit( $commit_sha1 );

    # Save all branches (called automatically when the object is destroyed):
    $writer->flush;


WARNING: do not call next_revision() until next_modification returns false.
Behaviour is undefined if you call the wrong one at the wrong time.

=cut

use warnings;
use strict;
use 5.10.0; # for "//"
use Carp;

use Date::Parse;

use File::Path qw/ make_path /;
use File::Basename;
use Git;

my ( $verbose, $dry_run ) = ( 0, 0 );

sub new {

    my ( $class, %args ) = @_;

    my $repo = Git->repository();

    my $commit = $args{commit};

    die "Please pass a 'commit' argument to Git::BranchImport::Writer->new"
        unless defined($commit);

    git_cmd_try(
        sub { $repo->command_oneline([qw/ rev-list --quiet /, $commit ], { STDERR => 0 }) },
        "Commit '$commit' does not exist in this repository"
    );

    my ( $fh, $c ) = $repo->command_output_pipe(qw/ log -z --reverse --raw --no-abbrev /, $commit, "--");

    $args{_repo} = $repo;
    $args{_fh  } = $fh;
    $args{_c   } = $c;
    $args{refs } = {};

    return bless( \%args, $class );

};

sub DESTROY {

    my $self = shift;

    $self->flush;

    $self->{_repo}->command_close_pipe($self->{_fh}, $self->{_c});

};

# Update the internal state to the next revision:
sub next_revision {

    my $self = shift;

    local $/ = "\0";

    my $fh  = $self->{_fh};
    my $log = <$fh>;

    return unless $log;

    chomp $log;

    $log =~ m{
        ^
        commit\ ([[:xdigit:]]{40})\n # commit SHA1
        Author:\ (.*)\n              # author name/e-mail
        Date:\ (.*)\n                # Datestamp
        \n                           # end-of-header mark
        ((?:\ {4}.*\n|\n)*?)         # message
        ((?:\n:.*)?)                 # first modification
        $
    }x or die "Couldn't read commit $_";

    @{$self}{qw/ _root_commit _author _date _modification /} = ( $1, $2, $3, $5 );

    my $message = $4;

    $message =~ s/^    //gm;

    $message =~ s{(?:^|\n)git-svn-id: ([^ ]*?)/*@([0-9]+) ([[:xdigit:]-]+$)}{}
        or die "Couldn't get git-svn-id from message $message";

    @{$self}{qw/ _message _url _revision _uuid /} = ( $message, $1, $2, $3 );

    return $2;

};

# Update the internal state to the next modification:
sub next_modification {

    my $self = shift;

    local $/ = "\0";

    my $modification = $self->{_modification};
    my $fh           = $self->{_fh};

    return unless $modification;

    my $filename = <$fh>;
    chomp $filename;

    $self->{_modification} = <$fh>;

    if ( defined($self->{_modification}) ) {
        # Will be undef at the end of the log

        chomp $self->{_modification};

        $modification =~ / ([AMD])$/ or die "Couldn't read file modification '$modification'";

    };

    return {
        action   => $1,
        filename => $filename,
    };

};

# Make the tags and branches visible to users:
sub flush {

    my $self = shift;

    my $repo_path = $self->{_repo}->repo_path . '/';

    if ( $verbose ) {

        while ( my ( $file, $commit_sha1 ) = each( %{$self->{_refs}} ) ) {

            my $file = $repo_path . $file;
            print "\`echo $commit_sha1 > $file\`;\n";

        };

    };

    unless ( $dry_run ) {

        while ( my ( $file, $commit_sha1 ) = each( %{$self->{_refs}} ) ) {

            my $file = $repo_path . $file;

            make_path( (fileparse($file))[1] );

            # This can fail if you have a branch within a branch
            # TODO: work out what to do about this
            open( my $fh, ">", $file ) or warn "$!: $file";
            print( $fh  $commit_sha1 ) or warn "$!: $file";
            close( $fh               ) or warn "$!: $file";

        };

    };

    $self->{_refs} = {};

};

# build a set of arguments that will later be used in a commit:
sub _build_commit {

    my ( $self, %args ) = @_;

    my ( $ref_name, $path, $parents ) =
        @args{qw/ ref_name path parents /};

    my $root_commit = $self->{_root_commit};
    my $message     = $self->{_message    };
    my $url         = $self->{_url        };
    my $revision    = $self->{_revision   };
    my $uuid        = $self->{_uuid       };

    # Use the specified commit, or the empty tree commit if the
    # directory doesn't exist (e.g. empty SVN tree not written to git)
    my $tree_sha1 = "4b825dc642cb6eb9a060e54bf8d69288fbee4904";
    unless ( $dry_run ) {
        eval {
            $tree_sha1 = Git::command_oneline([ "rev-parse", "$root_commit:$path" ], { STDERR => 0 });
        };
    };

    $message .= "git-svn-id: $url/$path\@$revision $uuid"
        if $self->{git_svn_id};

    my ( $author, $email ) = ( $1, $2 )
        if $self->{_author} =~ /^(.*) <([^>]*)>$/;

    return {
        tree_sha1       => $tree_sha1,
        parents         => $parents,
        ref_name        => $ref_name,
        message         => $message,
        author          => $author,
        email           => $email,
        date            => $self->{_date},
        verbose_message => "\$commits['$path'][$revision]",
    };

};

# create an actual commit in git (should only be called by a branch object):
sub _commit {

    my ( $self, %args ) = @_;

    my ( $tree_sha1, $parents, $ref_name, $message, $author, $email, $date, $verbose_message )
        = @args{qw/ tree_sha1 parents ref_name message author email date verbose_message /};

    my @commit_tree_command = (
        'commit-tree', $tree_sha1,
        map( { ( "-p" => $_ ) } @$parents ),
    );

    my $commit_sha1;

    if ( $verbose ) {

        $message =~ s/^/\t/gm;

        print(
            "Running command: \n\t$commit_sha1 = \`",
            join( " ", @commit_tree_command, "<<END" ), "\n",
            $message,
            "\n\tEND\n\t\`;\n\n"
        );

    };

    if ( $dry_run ) {

        $commit_sha1 = $verbose_message;

    } else {

        if ( defined($author) ) {
            foreach my $type (qw/ AUTHOR COMMITTER /) {
                $ENV{"GIT_${type}_NAME" } = $author;
                $ENV{"GIT_${type}_EMAIL"} = $email ;
                $ENV{"GIT_${type}_DATE" } = $date  ;
            };
        };

        my ($pid, $in, $out, $ctx) = Git::command_bidi_pipe( @commit_tree_command );
        print( $out $message );
        Git::command_close_pipe($out, $ctx);
        $commit_sha1 = <$in>;
        chop($commit_sha1); # Guaranteed to end with "\n", but $/ eq "\0" right now
        Git::command_close_pipe($in, $ctx);
        waitpid($pid, 0);

    };

    $self->{_refs}->{$ref_name} = $commit_sha1
        if defined($ref_name);

    return $commit_sha1;

};

sub _tag {

    my ( $self, %args ) = @_;

    my ( $tree_sha1, $parents, $ref_name, $message, $author, $email, $date, $verbose_message )
        = @args{qw/ tree_sha1 parents ref_name message author email date verbose_message /};

    my $unix_date = str2time( $date );

    my $name = $ref_name;
    $name =~ s|^refs/tags/|| or die "invalid tag name: $name";

    die
        "Impossible: tried to create a tag with " . scalar(@$parents) . " parents\n" .
        "Please report this bug."
        if $#$parents;
    my $parent = $parents->[0];

    my $tag_data = <<END
object $parent
type commit
tag $name
tagger $author <$email> $unix_date +0000

$message
END
;

    my $tag_sha1;

    if ( $verbose ) {

        $tag_data =~ s/^/\t/gm;

        print(
            "Running command: \n\t\`git mktag > .git/$ref_name <<END\n",
            $tag_data,
            "\n\tEND\n\t\`;\n\n"
        );

    };

    if ( $dry_run ) {

        $tag_sha1 = $parent;

    } else {

        my ($pid, $in, $out, $ctx) = Git::command_bidi_pipe( "mktag" );
        print( $out $tag_data );
        Git::command_close_pipe($out, $ctx);
        $tag_sha1 = <$in>;
        chop($tag_sha1); # Guaranteed to end with "\n", but $/ eq "\0" right now
        Git::command_close_pipe($in, $ctx);
        waitpid( $pid, 0 );

    };

    $self->{_refs}->{$ref_name} = $tag_sha1;

    return $tag_sha1;

};

# create a tag in git:
sub tag {

    my ( $self, %args ) = @_;

    my ( $ref_name, $from_branch, $from, $parent, $path ) =
        @args{qw/ ref_name from_branch from parent path /};

    my $root_commit = $self->{_root_commit};

    my $commit_args = $self->_build_commit(
        ref_name    => $ref_name,
        path        => $path,
        parents     => [ $parent ],
    );

    my $is_tag;

    unless ( $dry_run ) {

        eval {
            Git::command( "diff-tree", "--quiet", $parent, "$root_commit:$path" );
        };

        $is_tag = !$@;

        if ( ref($@) && $@->value != 1 ) {
            confess "Please report this bug.";
        };

    };

    my $commit_sha1 =
        $is_tag
        ? $self->_tag   ( %$commit_args )
        : $self->_commit( %$commit_args )
        ;

    return Git::BranchImport::Writer::Branch->new(
        writer   => $self,
        name     => $ref_name,
        path     => $path,
        head     => $commit_sha1,
        tag_data => $is_tag ? $commit_args : undef,
    );

};

sub _delete {
    my ( $self, $ref ) = @_;
    delete $self->{_refs}->{$ref};
};

sub branch {

    my $self = shift;

    return Git::BranchImport::Writer::Branch->new( writer => $self, @_ );

};



package Git::BranchImport::Writer::Branch;

use warnings;
use strict;

sub new {

    my ( $class, %args ) = @_;

    # %args is assumed to contain name, path, and writer

    $args{head    } //= undef;
    $args{tag_data} //= undef;

    return bless( \%args, $class );

};

# ACCESSORS:
sub path($) { $_[0]->{path} };
sub head($) { $_[0]->{head} };

# downgrade a tag to a normal commit if necessary:
sub downgrade_tag {

    my $self = shift;

    if ( my $tag_data = $self->{tag_data} ) {

        # this was a non-lightweight tag
        $self->{head} = $self->{writer}->_commit( %$tag_data );
        delete $self->{tag_data};
        return $self->{head};

    } else {

        return undef;

    };

};

sub commit {

    my ( $self, @parents ) = @_;

    my $head = $self->downgrade_tag // $self->{head};

    unshift( @parents, $head ) if defined( $head );

    my $commit_args = $self->{writer}->_build_commit(
        ref_name    => $self->{name},
        path        => $self->{path},
        parents     => \@parents,
    );

    $head = $self->{writer}->_commit( %$commit_args );

    return $self->{head} = $head;

};

sub delete {
    my $self = shift;
    $self->{writer}->_delete( $self->{name} );
};

package Git::BranchImport::History;

sub new {

    my ( $class, %args ) = @_;

    $args{_revisions} = [];

    return bless( \%args, $class );

};

sub get_and_downgrade {

    my ( $self, $rev, $path ) = @_;

    my $rev_data = $self->{_revisions}->[$rev]{$path};

    if ( defined($rev_data) ) {
        # We assume that downgrading is only possible when there's only one commit:
        if ( my $sha1 = $rev_data->{branch}->downgrade_tag ) {
            $rev_data->{sha1} = $sha1;
        };
        return $rev_data->{sha1};

    } else {
        return undef;
    };

};

sub add {

    my ( $self, $branch, $sha1 ) = @_;

    $self->{_revisions}->[-1]->{$branch->path} = {
        branch => $branch,
        sha1   => $sha1
    };

};

sub update { shift->add( @_ ) };

sub delete {

    my ( $self, $path ) = @_;

    delete $self->{_revisions}->[-1]->{$path};

};

sub next_revision {

    my ( $self, $revision ) = @_;

    # Clone the old revision into the new one:
    my $revisions = $self->{_revisions};

    if ( @$revisions ) {
        $self->{_revisions}->[$revision] = { %{$self->{_revisions}->[-1]} };
    } else {
        $self->{_revisions}->[$revision] = {                              };
    };

};


package Git::BranchImport::Tree;

# Store a tree of branches (including branches within branches)

sub new {

    my ( $class, $known_branches ) = @_;

    my %tree;

    foreach my $branch ( values(%$known_branches) ) {
        my @name = split( "/", $branch->path );
        my $node = \%tree;
        while ( @name ) {
            $node = $node->{children}->{shift @name} //= {};
        };
        $node->{branch} = $branch;
    };

    return bless( \%tree, $class );

};

sub delete {

    my ( $self, $name ) = @_;

    my @name = split( "/", $name );
    my $node = $self;
    while ( @name ) {
        $node = $node->{children}->{shift @name};
    };
    delete $node->{branch};

};

sub get_and_delete {

    my ( $self, $name ) = @_;

    my @ret;

    my @name = split( "/", $name );
    my $node = $self;
    while ( @name ) {
        my $branch = delete $node->{branch};
        push( @ret, $branch ) if $branch;
        $node = $node->{children}->{shift @name};
        last unless $node;
    };

    return \@ret;

};



package Git::BranchImport;

use warnings;
use strict;
use feature 'switch';

sub import {

    my $fh = shift;

    my $reader = Git::BranchImport::Reader->new( fh => $fh );
    my $latest_command = $reader->get_command;
    my $writer = Git::BranchImport::Writer->new(
        commit     => $reader->revisions_from,
        git_svn_id => $reader->git_svn_id,
    );
    my $history = Git::BranchImport::History->new;
    my $time = 0;

    my %known_branches;

    while ( my $revision = $writer->next_revision ) {

        if ( $time < time ) {
            $time = time;
            print STDERR "r$revision\r";
        };

        my $unedited_branches = Git::BranchImport::Tree->new( \%known_branches );
        $history->next_revision( $revision );

        # STEP ONE: RUN COMMANDS

        while ( $latest_command && $latest_command->{revision} <= $revision ) {

            my ( $command, $revision, $branch_name, $path, $from, $from_branch, $from_rev ) =
                @{$latest_command}{qw/ command revision branch path from from_branch from_rev /};

            my $from_sha1;
            $from_sha1 = $history->get_and_downgrade( $from_rev, $from )
                if $from;

            $reader->print_error( "Warning: branch $from did not exist in revision $from_rev" )
                if $from && !$from_sha1;

            given ( $command ) {

                when ( "create branch" ) {

                    my $branch = $writer->branch(
                        name => $branch_name,
                        path => $path,
                    );

                    my $commit_sha1;
                    if ( $from_sha1 ) {
                        $commit_sha1 = $branch->commit( $from_sha1 );
                    } else {
                        $commit_sha1 = $branch->commit;
                    };

                    $known_branches{$branch_name} = $branch;
                    $history->add( $branch, $commit_sha1 );

                };

                when ( "create tag" ) {

                    my $tag = $writer->tag(
                        ref_name    => $branch_name,
                        from        => $from,
                        from_branch => $from_branch,
                        path        => $path,
                        parent      => $from_sha1,
                    );

                    $known_branches{$branch_name} = $tag;
                    $history->add( $tag, $tag->head );

                };

                when ( "merge" ) {

                    if ( defined($from_sha1) ) {
                        my $branch = $known_branches{$branch_name};
                        my $commit_sha1 = $branch->commit( $from_sha1 );
                        $history->update( $branch, $commit_sha1 );
                    };

                };

                when ( "deactivate" ) {
                    $history->delete( $path );
                    delete $known_branches{$branch_name};
                };

                when ( "delete" ) {
                    $history->delete( $path );
                    my $branch = delete $known_branches{$branch_name};
                    $branch->delete;
                };

            };

            $unedited_branches->delete($branch_name);

            $latest_command = $reader->get_command;

        };


        # STEP TWO: COMMIT TO ALL CHANGED BRANCHES

        while ( my $modification = $writer->next_modification ) {

            my $branches = $unedited_branches->get_and_delete( $modification->{filename} );

            foreach my $branch ( @$branches ) {
                my $commit_sha1 = $branch->commit;
                $history->update( $branch, $commit_sha1 );
            };

        };

    };

};



package main;

my $file = shift @ARGV;
my $fh;
if ( defined($file) ) {
    open( $fh, "<", $file ) or die "$!: $file";
} else {
    $fh = \*STDIN;
};
Git::BranchImport::import( $fh );
