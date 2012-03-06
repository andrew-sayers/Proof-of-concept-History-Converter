package SVN::BranchExport::Writer;

=head1 SVN::BranchExport::Writer

    my $writer = SVN::BranchExport::ActionBuilder->new(
        fh => \*STDOUT,
    );

    $writer->write( $revision, $actions );

See ::ActionBuilder for a description of the "action" hash.

=cut

use Moose;
use Moose::Util::TypeConstraints;
use feature "switch";

use Readonly;

has outfile      => ( is => "ro", required => 1  , isa => "Maybe[Str]" );
has fh           => ( is => "ro", lazy_build => 1, isa => "FileHandle" );
has start_time   => ( is => "ro", required => 1  , isa => "Int" );
has command_line => ( is => "ro", required => 1  , isa => "Str" );
has log_style    => ( is => "ro", required => 1  , isa => enum([qw[ git svn ]]) );
has tag_patterns => ( is => "ro", required => 1  , isa => "HashRef[Str]" );
has need_newline => ( is => "rw", default => 0   , isa => "Bool" );

# Anything at or above this probability will be uncommented by default:
has accept_probability => ( is => "ro", required => 1, isa => "Num" );
# Anything at or above this probability will be printed with no surrounding comments:
has quiet_probability  => ( is => "ro", required => 1, isa => "Num" );

# Names for branches that have already been used:
has used_branch_names => ( is => "ro", default => sub { {} }, isa => "HashRef[Int]", );

sub _build_fh {
    my $outfile = shift->outfile;
    if ( defined($outfile) && $outfile ne "-" ) {
        open( my $ret, ">", $outfile ) or die "$!: $outfile";
        chmod( 0755, $outfile );
        return $ret;
    } else {
        return \*STDOUT;
    };
};

sub BUILD {

    my $self = shift;

    my $start_time = localtime( $self->start_time );
    my $command_line = $self->command_line;

    print { $self->fh } <<END
#!git-branch-import.pl
# this line tells emacs to highlight the file as a -*-shell-script-*-

# This is a Git Branch History file.
# It specifies the branches and merges that occurred in directories
# in a linearised commit history (e.g. one imported from SVN).
#
# Please edit this file before running it.

# This file was created at: $start_time
# This file was created with the command: $command_line


# The file starts with an optional "setup" section:

This is a version 1 Git Branch History file

# Read revisions from "master"
# Save branches in "refs/remotes/myproject"
# Save tags in "refs/tags/myproject"
# Remove git-svn-id from log messages



# The file continues with any number of commands:

END
    ;

};

sub write {

    my ( $self, $revision, $action, $summary ) = @_;

    my $revision_no = $revision->{revision};
    my $branch = $action->{branch};

    given ( $action->{command} ) {

        when ( "create branch" ) {

            print( { $self->fh } "In r$revision_no, ", $self->get_create_string( $branch ) );

            if ( my $from_branch = $branch->copied_from ) {
                my $revision_no = $branch->copyfrom_rev;
                my $path     = $self->_quote( $from_branch->path );
                print( { $self->fh } " from $path r$revision_no" );
            };
            print( { $self->fh } "\n" );

            $self->need_newline( 1 );

        };

        when ( "delete branch" ) {
            my $path = $self->_quote( $branch->path );
            print( { $self->fh } "In r$revision_no, delete $path\n" );
            $self->need_newline( 1 );
        };

        when ( "deactivate branch" ) {
            my $path = $self->_quote( $branch->path );
            print( { $self->fh } "In r$revision_no, deactivate $path\n" );
            $self->need_newline( 1 );
        };

        when ( "merge" ) {

            my $needs_attention =
                 $action->{probability} < $self->quiet_probability ||
                !$action->{from_rev} ||
                 $action->{needs_attention}
            ;

            my $from_branch = $action->{from_branch};
            my $from_rev    = $action->{from_rev};
            my $path        = $self->_quote( $branch->path );

            my $from_path   = $from_branch && $self->_quote( $from_branch->path );

            print( { $self->fh } "\n" ) if $needs_attention && $self->need_newline;

            if ( $needs_attention ) {

                print(
                    { $self->fh }
                    $self->log_header(
                        $action->{comment},
                        $revision_no,
                        $revision->{author},
                        $revision->{date},
                        $revision->{log},
                        $summary,
                    ),
                );

                print { $self->fh } $self->_unmerged_header( $from_path, $path, $action->{unpicked_revs} )
                    if $from_branch;

                print { $self->fh } $self->branch_relations( $branch )
                    if $branch;

                if ( $action->{probability} < $self->accept_probability ) {

                    my $from_rev  = $action->{from_rev} // "YYY";
                    my $from_path = $action->{from_branch} && $action->{from_branch}->path;
                    my $path      = $action->{path};

                    $from_path = defined($from_path) ? $self->_quote( $from_path ) : "XXX";
                    $path      = defined(     $path) ? $self->_quote(      $path ) : "XXX";

                    my $unmerged = $self->_unmerged_header( $from_path, $path, $action->{unpicked_revs} );

                    print { $self->fh } $unmerged;

                    print { $self->fh } "#\n" if $unmerged;

                    print(
                        { $self->fh }
                        "# WARNING: Only uncomment merges that apply everything up to and including a given revision.\n",
                        "# Please ignore revisions that have been cherry-picked in.\n",
                        "#\n",
                    );

                    print( { $self->fh } "#In r$revision_no, merge $from_path r$from_rev into $path\n\n" );
                    $self->need_newline( 0 );

                    return;

                };

            } else {

                print { $self->fh } $self->_unmerged_header( $from_path, $path, $action->{unpicked_revs} );

            };

            if ( !defined($from_rev) ) {
                print { $self->fh } "#";
                $from_rev = "YYY";
            } elsif ( $needs_attention ) {
                print { $self->fh } "#";
            };

            print(
                { $self->fh }
                "In r$revision_no, merge $from_path r$from_rev into $path\n"
            );

            if ( $needs_attention ) {
                print( { $self->fh } "\n" );
                $self->need_newline( 0 );
            } else {
                $self->need_newline( 1 );
            };

        };

        default {
            die
                "Tried to run an unknown command: $_\n" .
                "Please report this bug.";
        };

    };

};

sub _quote {

    my ( $self, $path ) = @_;

    $path =~ s/([\\"])/\\$1/g;

    return "\"$path\"";

};

sub get_create_string {

    my ( $self, $branch ) = @_;

    my $revision      = $branch->add_revision;
    my $path          = $self->_quote( $branch->path );

    # Get a unique name that git can use to refer to the branch:
    my $branch_name = $branch->path;
    my $branch_or_tag = "branch";

    while ( my ( $pattern, $replacement ) = each( %{$self->tag_patterns} ) ) {
        if ( $branch_name =~ s/$pattern/$replacement/ ) {
            $branch_or_tag = "tag";
            keys %{$self->tag_patterns}; # reset the iterator
            last;
        };
    };

    if ( $branch_or_tag eq "branch" ) {
        $branch_name =~ s{(^|/)branches/}{$1};
    };

    $branch_name = $self->_quote( $branch_name );

    if ( $self->used_branch_names->{$branch_name} ) {

        $branch_name =~ s/"$/\@$revision"/;

        if ( $self->used_branch_names->{$branch_name} ) {
            my $suffix = 0;
            $branch_name =~ s/"$/\.$suffix"/;
            while ( $self->used_branch_names->{$branch_name} ) {
                ++$suffix;
                $branch_name =~ s/\.[0-9]+"$/.$suffix"/;
            };
        };

    };

    $self->used_branch_names->{$branch_name} = 1;

    my $ret = "create $branch_or_tag $path";

    if ( $branch_name ne $path ) {
        $ret .= " as $branch_name";
    };

    return $ret;

};



sub branch_relations {

    my ( $self, $branch ) = @_;

    my @ancestors;
    my $ancestor = $branch;
    while ( $ancestor = $ancestor->copied_from ) {
        unshift( @ancestors, $ancestor );
    };

    my $copied_to = $branch->copied_to;

    return "#\n# " . $self->_quote( $branch->path ) . " is a trunk that has never been branched\n#\n"
        unless @ancestors || @$copied_to;

    my $ret = "#\n# Related merge candidates for " . $self->_quote( $branch->path ) . ":\n";

    my $indent = 3;
    foreach my $ancestor ( @ancestors ) {
        $ret .=
            "#" . (" " x $indent) . "- " . $self->_quote( $ancestor->path )
            . " r" . $ancestor->last_changed . " was changed by " . $ancestor->last_author . "\n";
        $indent += 2;
    };

    $ret .=
        "#" . ( " " x $indent ) . "= " . $self->_quote( $branch->path ) . "\n";
    $indent += 2;

    my @copied_to = sort(
        {
            !!$a->rm_revision <=> !!$b->rm_revision || # live branches first
              $a->path        cmp   $b->path
        }
        @$copied_to
    );

    foreach my $child ( @copied_to ) {
        $ret .=
            "#" . (" " x $indent) . "- " . $self->_quote( $child->path ) . " r" .
            ($child->rm_revision
             ? $child->rm_revision  . " was deleted by "
             : $child->last_changed . " was changed by "
            ) .
            $child->last_author . "\n",
    };

    return $ret . "#\n";

};


sub _unmerged_header {
    my ( $self, $from_quoted_path, $to_quoted_path, $unmerged ) = @_;

    # Note: the line below is formatted to make it easy for users to remove
    # all reference to merges they have manually confirmed as being merged in.
    # e.g. with a regexp like the following in their favourite text editor:
    # s/($from to $to:.*) $manually_verified_revision /$1/g;

    if ( $unmerged && @$unmerged ) {
        return
            "# Revisions not yet merged from $from_quoted_path to $to_quoted_path: " .
            join( " ", @$unmerged ) . "\n";
    } else {
        return "";
    };

};


sub log_header {

    my ( $self, $comment, $revision, $author, $date, $log_message, $summary ) = @_;

    $log_message =~ s/\s*$//;
    $date = localtime( $date );

    given ( $self->log_style ) {

        when ( "svn" ) {

            $log_message =~ s/^/# /gm;
            $log_message =~ s/^#\s+$/#/gm;

            my $line_count = ( $log_message =~ /\n/g ) + 1;

            $line_count .= ( 1 == $line_count ) ? " line" : " lines";

            return <<END
# $comment:
# ------------------------------------------------------------------------
# r$revision | $author | $date | $line_count
# Changed path summary: $summary
#
$log_message
# ------------------------------------------------------------------------
END

        };


        when ( "git" ) {

            $log_message =~ s/^/#     /gm;
            $log_message =~ s/^#\s+$/#/gm;

            return <<END
# $comment:
# revision $revision
# Author: $author
# Date: $date
#
$log_message
#
# Changed path summary: $summary
#
END

        };


    };

};


__PACKAGE__->meta->make_immutable;
