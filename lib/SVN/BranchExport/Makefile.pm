package SVN::BranchExport::Makefile;

=head1 SVN::BranchExport::Makefile

    SVN::BranchExport::Makefile::write(

        # These values will usually be specified by the user:
        infile           => "myproject.dump",
        outfile          => "make.sh",
        ignored_patterns => [ "READ.*ME", "~\$" ],

        # These values will usually come from SVN::BranchExport::configure:
        trunks           => \@trunks,
        svk_depots       => \@svk_depots,
        tag_patterns     => { "^tags/" => "" },
        authors          => { "user" => "user <user@host>" },
        action_count     => 12345,

    );

    SVN::BranchExport::Makefile::read( "make.sh" );

During configuration, SVN::BranchExport writes a "make" file for the
user to check before actually making the history.  This module handles
writing and reading that file.

=cut

use warnings;
use strict;
use feature "switch";
use List::Util qw/ max /;

sub write {

    my %args = @_;

    my ( $infile, $outfile, $ignored_patterns, $data,
         $trunks, $svk_depots, $tag_patterns, $authors, $action_count )
        = @args{qw/
              infile outfile ignored_patterns data
              trunks svk_depots tag_patterns authors action_count
          /};

    my $dump_file;
    if ( defined($infile) ) {
        $dump_file = "dump_file \"" . _quote( $infile ) . "\"";
    } else {
        $dump_file = "#dump_file \"\" # dump read from standard input";
    };

    my $ignored_lines =
        join(
            "\n",
            map(
                { 'ignored_pattern "' . _quote($_) . '"' }
                @$ignored_patterns
            )
        );

    $ignored_lines = "# Pretend these files don't exist:\n$ignored_lines"
        if $ignored_lines;

    my $fh;
    if ( $outfile eq "-" ) {
        $fh = \*STDOUT;
    } else {
        open( $fh, ">", $outfile );
    };

print $fh <<END
#!$0 make
# this line tells emacs to highlight the file as a -*-shell-script-*-

#
# This file defines variables used to make the history.
# Please confirm the values below before running the script.
#

$dump_file
# Used to display better progress information:
action_count $action_count
$ignored_lines

# merges at or above this probability will be uncommented:
accept_probability 0.9
# merges below this probability will get a detailed merge comment:
quiet_probability 0.75
# Merge comments can look like either git or svn log messages:
log_style svn
# Replace "merge" with "<MERGE>" in merge comments:
highlight_merges yes


#
# AUTHORS SECTION
#
# Please get names and e-mail addresses for all of the following authors.
# For example:
#
# author andrew = Andrew Sayers <svn\@pileofstuff.org>
#

# Authors will be saved in this file:
authors_file "authors.txt"

END
;

    my $longest_author_name = max( map( { length($_) } keys(%$authors) ) );

    foreach my $author ( sort( keys %$authors ) ) {
        printf( $fh "author %${longest_author_name}s = %s\n", $author, $authors->{$author} );
    };

    if ( $svk_depots && @$svk_depots ) {

        print $fh <<END


#
# SVK SECTION
#
# SVK was an SVN-based DVCS that was discontinued in 2010.
#
# It's possible to automatically extract some useful SVK merge
# information if you know the revision where the SVK depot diverged
# from SVN.  Please confirm with the respective authors that the
# revision specified is the last revision they got from SVN before
# starting work in SVK.
#
END
;

        foreach my $repo ( @$svk_depots ) {
            my $diverged = $repo->{diverged_before}-1;
            my $revisions = ($repo->{merge_candidates}==1)?"revision":"revisions";
            print $fh <<END

# User "$repo->{author}" has an SVK depot with up to $repo->{merge_candidates} automatically mergeable $revisions:
svk_depot "$repo->{uuid}" r$diverged
END
;
        };

    };


    if ( %$tag_patterns ) {

        print $fh <<END


#
# TAGS SECTION
#
# Branches matching the specified regexps will be treated as branches.
# The branch names will be translated to tag names using the specified
# replacement.
#

END
;

        foreach my $tag ( sort keys %$tag_patterns ) {
            print $fh
                'tag_pattern "',
                _quote($tag), '" => "', _quote($tag_patterns->{$tag}),
                "\"\n";
        };

    };


    print $fh <<END


#
# TRUNKS SECTION
#
# You need to specify the trunks (but not the branches) in your
# repository.
#
# Please edit these trunks manually or run script again with
# "--ignore" flags for file patterns that shouldn't be used to guess
# trunks:
#
END
;


    my @trunks = sort(
        {
            $a->path         cmp $b->path         ||
            $a->add_revision <=> $b->add_revision
        }
        @$trunks
    );
    foreach my $trunk ( @trunks ) {
        my $revision = 'r' . $trunk->add_revision;
        $revision   .= ":r" . $trunk-> rm_revision
            if $trunk->rm_revision;
        print $fh "\n";
        print $fh "# This looks like a trunk because it contains " . $trunk->branch_file . ":\n";
        print $fh 'trunk "', _quote( $trunk->path ), "\" $revision\n";
    };

    if ( $outfile ne "-" ) {
        close( $fh ) or die "$!: $outfile";
        chmod( 0755, $outfile );
    };

};

sub read {

    my ( $file ) = @_;

    my %ret = (
        authors          => [],
        commands         => [],
        ignored_patterns => [],
        svk_depots       => {},
        tag_patterns     => {},
    );

    open( my $fh, "<", $file ) or die "$!: $file";

    while ( <$fh> ) {

        chomp;

        given ( $_ ) {

            # Ignore comments:
            when ( /^\s*($|#)/ ) {};

            when ( /^(dump_file|action_count|(?:accept|quiet)_probability|log_style|authors_file)\s+(.*?)\s*$/ ) {
                my ( $name, $value ) = ( $1, $2 );
                $ret{$name} = _unquote($value);
            };

            when ( /^author\s+(.*?)\s*$/ ) {
                push( @{$ret{authors}}, $1 );
            };

            when ( /^highlight_merges\s+(?i)(?:y|yes|on|1)\s*$/ ) {
                $ret{highlight_merges} = 1;
            };
            when ( /^highlight_merges\s+(?i)(?:n|no|off|0)\s*$/ ) {
                $ret{highlight_merges} = 0;
            };

            when ( /^ignored_pattern\s+(.*?)\s*$/ ) {
                push( @{$ret{ignored_patterns}}, _unquote($1) );
            };

            when ( /^svk_depot\s+(.*?)\s+r?([0-9]+)\s*$/ ) {
                my ( $depot, $revision ) = ( $1, $2 );
                $ret{svk_depots}{_unquote($depot)} = $revision;
            };

            when ( /^tag_pattern\s+("(?:[^"]|\\")*")\s*=>\s*("(?:[^"]|\\")*")\s*$/ ) {
                my ( $pattern, $result ) = ( $1, $2 );
                $ret{tag_patterns}{_unquote($pattern)} = _unquote( $result );
            };

            when ( /^trunk\s+(.*?)\s+r?([0-9]+)(?::r?([0-9]+))?\s*$/ ) {
                my ( $path, $from, $to ) = ( $1, $2, $3 );

                $path = _unquote( $path );

                push(
                    @{$ret{commands}},
                    { command => "add"   , add_revision => $from, path => $path, },
                );

                push(
                    @{$ret{commands}},
                    { command => "delete",  rm_revision => $to  , path => $path, },
                )
                    if defined($to);

            };

            default {
                die "Couldn't read make line: $_";
            };

        };

    };

    return \%ret;

};


sub _quote {
    my $value = shift;
    $value =~ s/([\\\n"])/\\$1/g;
    return $value;
};

sub _unquote {
    my $value = shift;
    if ( $value =~ s/^"(.*)"$/$1/ ) {
        $value =~ s/\\(.)/($1 eq "n" ) ? "\n" : $1/ge;
    };
    return $value;
};

1;
