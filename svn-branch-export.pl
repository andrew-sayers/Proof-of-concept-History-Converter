#!/usr/bin/perl -Ilib

=head1 SVN Branch Exporter - build a description of SVN history

This script takes an SVN dump file and produces an abstract
description of an SVN history.  Because SVN history is defined by
convention more than implementation, a lot of manual involvement is
required (especially if you want to represent merges cleanly).

The script runs in three stages:

=over 4

=item * configure - find authors, guess trunks, suggest tag regexps

=item * make - build a branch history file

=item * install - import the branch history file into git

=back

Run the "configure" stage like so:

    ./svn-branch-export.pl configure -o make.sh -i ReadMe.html -i readme.html ... < svn.dump

The "-i" flag specifies a regexp to be ignored for the purpose of
guessing trunk branches.  When this has finished, edit "make.sh" to
correct the authors and make sure the trunk directories have been
guessed correctly.

Once you're happy with your "make.sh", you can run it to begin the
"make" stage:

    ./make.sh -o install.sh < svn.dump

This will read the SVN dump and produce a file in Branch History
format.  It will also print authors to the specified file in a format
compatible with git-svn (we might look at more useful handling of
authors in the future).  You should edit this file heavily before
continuing (see below).

Once you're happy with your "install.sh", you can run it (it will call
git-branch-import.pl):

    cd myrepo && .../install.sh

This will split your repository into a collection of branches.

=head2 Editing files

For non-trivial repositories, you will need to edit make.sh and
install.sh heavily in order to get the best results.  Here are some
suggestions:

Create a git repository containing the pristine make.sh and install.sh
files as created by the script.  Managing this little repository will
make it much easier to fix your mistakes.

The first thing you should do with install.sh is an initial pass
fixing/removing the obviously good/bad messages (e.g. "merged
r123-r234 from trunk" or "added a new merge algorithm") - ignore all
the non-trivial cases for now.  You can usually clear up half the
ambiguous messages this way, and gain a better feel for the problems
you'll face with the hard cases.

If you are exporting history for a project with many authors, consider
asking other contributors to help clean up install.sh - it will give
people an opportunity to use git and they're more likely to know what
their revision logs meant than you are.

Cleaning the history of a large repositories will generally be quite
repetitive.  Read up on regexes and automation in your favourite
editor, e.g. http://www.emacswiki.org/emacs/KeyboardMacros or
http://vim.wikia.com/wiki/Macros

=cut

use warnings;
use strict;

use Term::ANSIColor;
use Getopt::Long;

use SVN::BranchExport;

my $command = shift( @ARGV );


=head2 diff

Diffs the contents of two files, removing noise and adding
git-style colour.

=cut

sub diff {

    my ( @files, @args );

    while ( @ARGV ) {
        my $arg = shift @ARGV;
        if ( $arg eq "--" ) {
            @files = @ARGV;
            last;
        } elsif ( $arg !~ /^-/ ) {
            push( @files, $arg );
        } else {
            push( @args, $arg );
        };
    };

    my ( $from, $to ) = @files;

    our $reset = color("reset");
    our %colours = (
        "@" => color( "cyan"  ),
        "+" => color( "green" ),
        "-" => color( "red"   ),
        );

    $^F = 255;

    sub show {

        my $file = shift;

        my $child_pid = open(my $fork_fh, "-|") // die "can't fork: $!";

        return $fork_fh if $child_pid;

        open( my $file_fh, "<", $file ) or die "$!: $file";

        my $unmerged_text = color("bold black") . "(unmerged revisions)$reset";
        my $command_line = color("bold black") . "(command line)$reset";

        while ( <$file_fh> ) {
            s/(# This file was created with the command: ).*/$1$command_line/;
            s/Revisions not yet merged from ".*/$unmerged_text/;
            print;
        };

        exit;

    };

    my $from_fh = show( $from );
    my   $to_fh = show( $to   );

    open(
        DIFF,
        "-|",
        "diff", "-NaurwB", @args,
        "--label", $from, "/dev/fd/" . fileno($from_fh),
        "--label", $to  , "/dev/fd/" . fileno(  $to_fh),
    );

    open(
        LESS,
        "|-",
        "less", "-RF"
    );

    while ( <DIFF> ) {

        s/^([@+-])(.*)/$colours{$1}$1$2$reset/;

        print LESS;

    };

};


my ( $time, $verbose ) = ( 0, 0 );
sub progress {

    if ( $verbose || $time < time ) {
        my $revision = shift;
        my $fraction = shift;
        print STDERR "\rr$revision";
        if ( $fraction ) {
            print STDERR " (", int( $fraction * 100 ), "%)";
        };
        $time = time;
    };

};

=head2 configure

Guess which directories in a repository represent the trunk nodes in
the history.

=cut

sub configure {

    my @ignored_patterns;
    my $outfile = "./make.sh";

    my $result = GetOptions(
        "i|ignore=s" => \@ignored_patterns,
        "o|out=s"    => \$outfile,
    ) or die "Couldn't parse options";

    my $infile = shift @ARGV;

    SVN::BranchExport::configure(
        infile           => $infile,
        outfile          => $outfile,
        ignored_patterns => \@ignored_patterns,
        progress         => \&progress,
    );

    print STDERR "\r";
    print "Created \"$outfile\" - please check it in an editor before running it.\n";
    unless ( defined($infile) ) {
        print "Make sure to pipe in the dumpfile when you run it.\n";
    };

    return 0;

};


=head2 make

Guess which directories in a repository represent the trunk nodes in
the history.

=cut

sub make {

    my $command_line = join( " ", map( { /\s/ ? "\"$_\"" : $_ } $0, @ARGV ) );
    my $start_time = time;

    my $outfile = "./install.sh";

    my $result = GetOptions(
        "o|out=s" => \$outfile,
    ) or die "Couldn't parse options";

    my $infile = shift @ARGV;

    open( my $fh, ">", $outfile ) or die "$!: $outfile";

    SVN::BranchExport::make(
        infile       => $infile,
        outfile      => $outfile,
        progress     => \&progress,
        start_time   => $start_time,
        command_line => $command_line,
    );

    close( $fh ) or die "$!: $outfile";

    print STDERR "\r";
    print "Created \"$outfile\" - please check it in an editor before running it.\n";
    unless ( defined($infile) ) {
        print "Make sure to pipe in the dumpfile when you run it.\n";
    };

    return 0;

};



my %commands = (
    configure => \&configure,
    make      => \&make,
    diff      => \&diff,
);



if ( $commands{$command} ) {
    exit $commands{$command}->();
};
