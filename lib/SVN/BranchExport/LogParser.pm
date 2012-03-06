package SVN::BranchExport::LogParser;

# This class searches a log message for potential merge information

use Moose;
use feature "switch";

use List::MoreUtils qw/ uniq /;
use Readonly;

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
       (?:in\s+changes\s+from|in\s+the\s+following|in)
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

# Matches a string that looks like a list of revisions (e.g. "revisions 1-10"):
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

# Matches a string that looks like a list of revisions (e.g. "revisions 1-10"):
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

=head2 _parse_revisions

    my @revision_numbers = $self->_parse_revision_numbers( "r123, revisions 234:r345 and 456 through 567" );

Extracts a list of revision numbers from a pattern matching $revision_list.

Returns a list of all the numbers it finds.

=cut
sub _parse_revision_list {

    my ( $self, $message ) = @_;

    my $original_message = $message; # useful for debugging

    my @revisions;
    my $already_found_revisions = 0;

    # Disambiguate the message:
    $message =~ s/(?:,|and)/ /gi;
    $message =~ s/revisions?\s*:*|revision|rev|r/ /gi;
    $message =~ s/\s+/ /g;
    $message =~ s/^ //;
    $message =~ s/ $//;
    $message =~ s/\s*(?:-|:|to|through)\s*/:/gi;

    # parse revisions out:
    foreach my $range ( split( " ", $message ) ) {
        if ( $range =~ /^[0-9]+$/ ) {
            push( @revisions, $range );
        } elsif ( $range =~ /^([0-9]+):([0-9]+)/ ) {
            push( @revisions, $1..$2 );
        } else {
            die "Can't parse \"$original_message\"";
        };
    };

    return [ sort( { $a <=> $b } uniq(@revisions) ) ];

};

# Make sure the revision numbers make sense for a strong guess
# (e.g. "merge r123 from trunk" -> does r123 modify trunk?)
sub _check_for_merge_typos {

    my ( $self, $mergeinfo ) = @_;

    my ( $branch, $revisions ) = @{$mergeinfo}{qw/ from_branch from_revs /};

    my %guessed_branches = %{$revision_branches->( @$revisions )};

    my $from_quoted_path = $branch->[0]->quoted_path;

    if ( delete $guessed_branches{$branch->[0]->path} ) {

        # The expected branch was modified in the relevant revisions...

        given ( scalar(keys(%guessed_branches)) ) {

            when ( 0 ) {
                # ... and nothing else, hurrah!
                return "";
            };

            when ( 1 ) {
                # ... but so was one other :(
                my $to_quoted_path = (values(%guessed_branches))[0]->quoted_path;
                return "$from_quoted_path was modified, but so was $to_quoted_path.";
            };

            default {
                # ... but so were many others :(
                return
                    "$from_quoted_path was modified, but so were these:\n\t"
                    . join( ", ", keys %guessed_branches );
            };

        };

    } else {

        # The expected branch was not modified in the relevant revisions...

        given ( scalar(keys(%guessed_branches)) ) {

            when ( 0 ) {
                # ... and neither was anything else ?!?
                return "no branches were actually modified - maybe the revision(s) don't exist?";
            };

            when ( 1 ) {
                # ... but one other branch was :(
                my $to_quoted_path = (values(%guessed_branches))[0]->quoted_path;
                return "$from_quoted_path wasn't modified, but $to_quoted_path was.";
            };

            default {
                # ... but many other branches were :(
                my @to_quoted_paths = map( { $_->quoted_path } values %guessed_branches );
                return
                    "$from_quoted_path wasn't modified, but these were:\n\t"
                    . join( ", ", @to_quoted_paths );
            };

        };

    };

};


=head1

    my $mergeinfo = $log_reader->scavenge_mergeinfo( 567, "merged rev 123, 234 and 345- 456" );

Tries to extract meaningful merge information from a message.

=cut

sub scavenge_mergeinfo {

    my ( $self, $revision, $branch_names, $revision_branches ) = @_;

    my $revision_no = $revision->{revision};
    my $message = $revision->{log};

    # Match the longest patterns first (so that approximate
    # matches pick the right branch name):
    my @branches = sort( { length($b) <=> length($a) } keys %$branch_names );
    my $branch_pattern =
        @branches
        ? join( "|", @branches )
        : "(?!)"
        ;
    $branch_pattern = qr{(?:\/?($branch_pattern)\/?)};

    my %ret;

    # IMPLEMENTATION NOTE: don't be a hero!
    #
    # This function has two purposes:
    # - parse the 90% of really obvious patterns so that people don't have to
    # - flag up the remaining possible patterns so that people are able to
    #
    # Before adding clever regexps below, consider how many real-world
    # merge commits you will catch - if the number is quite low,
    # consider using a stricter regexp to compensate for all the false
    # positives that users will manage to throw at you.

    given ( $message ) {

        # svnmerge writes merges in a standard format: http://www.orcaware.com/svn/wiki/Svnmerge.py
        when (/^$merge\s+($revision_list)\s+via\s+svnmerge\s+from(?:http:\/\/[^ ]*$branch_pattern)\n/i) {

            my ( $revisions, $branch ) = ( $1, $2 );

            $ret{from_branch} = $self->_get_branch( $branch );
            $ret{from_revs  } = $self->_parse_revision_list( $revisions );

            if ( my $error = $self->_check_for_merge_typos( \%ret ) ) {
                $ret{comment } = "Probably a merge with a typo: $error";
                $ret{strength} = 4;
            } else {
                $ret{comment } = "Strong guess from svnmerge log message";
                $ret{strength} = 1;
            };

        };

        # e.g. "merge from trunk: r123":
        when (/^$merge(?:\s+revisions?)\s+from\s+$branch_pattern\s*:\s*($maybe_revision_list)$log_suffix/i) {

            my ( $revisions, $branch ) = ( $2, $1 );

            $ret{from_branch} = $self->_get_branch( $branch );
            $ret{from_revs  } = $self->_parse_revision_list( $revisions );

            if ( my $error = $self->_check_for_merge_typos( \%ret ) ) {
                $ret{comment } = "Probably a merge with a typo: $error";
                $ret{strength} = 4;
            } else {
                $ret{comment } = "Strong guess from log message",
                $ret{strength} = 2;
            };

        };

        # e.g. "merge r123 from trunk":
        when (/^$merge\s+($maybe_revision_list)\s+from(?:\s+the)?\s+$branch_pattern$log_suffix/i) {

            my ( $revisions, $branch ) = ( $1, $2 );

            $ret{from_branch} = $self->_get_branch( $branch );
            $ret{from_revs  } = $self->_parse_revision_list( $revisions );

            if ( my $error = $self->_check_for_merge_typos( \%ret ) ) {
                $ret{comment } = "Probably a merge with a typo: $error";
                $ret{strength} = 4;
            } else {
                $ret{comment } = "Strong guess from log message";
                $ret{strength} = 2;
            };

        };

        # e.g. "merge r123":
        when (/^$merge\s+(up\s+to\s+$maybe_revision|$maybe_revision_list)$log_suffix/i) {

            my $revision_string = $1;
            $revision_string =~ s/^up\s+to\s+/1:/;

            my $revisions = $self->_parse_revision_list( $revision_string );
            my @branches = values( %{$revision_branches->( @$revisions )} );

            $ret{from_branch} = \@branches;
            $ret{from_revs  } = $revisions;

            given ( $#branches ) {

                when ( -1 ) {
                    $ret{comment } = "Probably a typo - no modified revisions could be found",
                    $ret{strength} = 4;
                }

                when ( 0 ) {
                    $ret{branches} = $branches[0];
                    $ret{comment } = "Average guess from log message";
                    $ret{strength} = 3;
                };

                default {
                    $ret{comment } = "Probably a typo or a merge from multiple branches";
                    $ret{strength} = 4;
                };

            };

        };

        # e.g. "This commit merges everything in from trunk":
        when ( /^$merge$branch_pattern$log_suffix/ ) {

            my $branch = $branch_names->{$1};

            $ret{from_branch} = [ $branch ];
            $ret{from_revs  } = [ 1 .. ($revision_no-1) ];
            $ret{comment    } = "Average guess from log message";
            $ret{strength   } = 3;

        };

        # e.g. "This commit merges everything in from trunk":
        when ( /$merge/ && /[0-9]+|$branch_pattern/ ) {

            my @revisions;

            while ( /($maybe_revision_list)/g ) {
                push( @revisions, @{$self->_parse_revision_list( $1 )} );
            };

            @revisions = grep( { $_ <= $revision_no } @revisions );

            my $guessed_branches = $revision_branches->( @revisions );

            $guessed_branches->{$_} //= $branch_names->{$_} foreach /$branch_pattern/g;

            if ( %$guessed_branches ) {
                $ret{from_branch} = [ values %$guessed_branches ];
                $ret{from_revs  } = \@revisions;
                $ret{comment    } = "Very weak guess from log message";
                $ret{strength   } = 5;
            } else {
                $ret{from_branch} = [];
                $ret{from_revs  } = \@revisions;
                $ret{comment    } = "Probably not a merge";
                $ret{strength   } = 99;
            };

        };

        # e.g. "Fix a bunch of old merge conflicts":
        when ( /$merge/ ) {
            $ret{comment    } = "Probably not a merge";
            $ret{strength   } = 99;
        };

        default {
            return undef;
        };

    };

    return \%ret;

};

__PACKAGE__->meta->make_immutable;
