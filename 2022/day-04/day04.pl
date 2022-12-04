#!/usr/bin/perl -w
use Test2::V0;    # turns on strict and warnings pragmas by default

sub lines_from_file {
    my $filename = shift;
    open my $fh, '<', $filename or die "Could not open file '$filename' $!";
    my @lines = <$fh>;
    close $fh;
    return map { chomp; $_ } @lines;
}

sub parse_lines {
    my @result = ();

    for my $line (@_) {
        my @pair       = split /,/, $line;
        my @left_pair  = split /-/, $pair[0];
        my @right_pair = split /-/, $pair[1];

        push @result,
          [ $left_pair[0], $left_pair[1], $right_pair[0], $right_pair[1] ];
    }

    return @result;
}

sub is_fully_contained {
    die if @_ != 4;
    my ( $left_min, $left_max, $right_min, $right_max ) = @_;

    my $is_left_in_right = $left_min >= $right_min && $left_max <= $right_max;
    my $is_right_in_left = $right_min >= $left_min && $right_max <= $left_max;

    return $is_left_in_right || $is_right_in_left;
}

sub find_fully_contained {
    return grep { is_fully_contained(@$_) } @_;
}

sub part1 {
    my $filename         = shift;
    my @lines            = lines_from_file($filename);
    my @parsed_lines     = parse_lines(@lines);
    my @fully_containted = find_fully_contained(@parsed_lines);

    return scalar @fully_containted;
}

# --------------- TESTS ---------------

my @expected_lines = (
    "2-4,6-8",    #
    "2-3,4-5",    #
    "5-7,7-9",    #
    "2-8,3-7",    #
    "6-6,4-6",    #
    "2-6,4-8"
);

my @sample_lines = lines_from_file("./day04.sample");

is \@sample_lines, \@expected_lines, "lines_from_file";

# -------------------------------------

my @expected_parsed_lines = (
    [ 2, 4, 6, 8 ],
    [ 2, 3, 4, 5 ],
    [ 5, 7, 7, 9 ],
    [ 2, 8, 3, 7 ],
    [ 6, 6, 4, 6 ],
    [ 2, 6, 4, 8 ]
);

my @parsed_lines = parse_lines(@sample_lines);

is \@parsed_lines, \@expected_parsed_lines, "parse_lines";

# -------------------------------------

ok !is_fully_contained( ( 2, 4, 6, 8 ) ),
  "is_fully_contained (no overlap at all)";
ok !is_fully_contained( ( 2, 4, 6, 8 ) ),
  "is_fully_contained (partial overlap only)";
ok is_fully_contained( ( 2, 4, 2, 8 ) ),
  "is_fully_contained (left fully in right)";
ok is_fully_contained( ( 3, 8, 4, 5 ) ),
  "is_fully_contained (right fully in left";

# -------------------------------------

my @assignments = (
    [ 2, 4, 6, 8 ],
    [ 2, 3, 4, 5 ],
    [ 5, 7, 7, 9 ],
    [ 2, 8, 3, 7 ],
    [ 6, 6, 4, 6 ],
    [ 2, 6, 4, 8 ]
);

my @expected_fully_contained = (
    [ 2, 8, 3, 7 ],    #
    [ 6, 6, 4, 6 ],
);

is find_fully_contained(@assignments), @expected_fully_contained,
  "find_fully_contained with sample data";

# -------------------------------------

is part1("./day04.sample"), 2, "part1 with sample data";

done_testing;

# --------------- END TESTS ---------------

print "\n";
print "Part 1: " . part1("./day04.in") . "\n";
