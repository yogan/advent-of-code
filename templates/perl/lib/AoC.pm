package AoC;

use strict;
use warnings;

use List::Util 'sum';

sub read_file {
    my $filename = shift;
    open my $fh, '<', $filename or die "Could not open file '$filename' $!";
    my @lines = <$fh>;
    close $fh;
    return @lines;
}

sub parse_input {
    return map {
        [ map { int($_) } split /x/, $_ ]
    } @_;
}

sub volume {
    my ( $l, $w, $h ) = @_;
    return $l * $w * $h;
}

sub surface_area {
    my ( $l, $w, $h ) = @_;
    return 2 * $l * $w + 2 * $w * $h + 2 * $h * $l;
}

sub part1 {
    return sum map { volume(@$_) } @_;
}

sub part2 {
    return sum map { surface_area(@$_) } @_;
}

1;    # indicate that the module loaded successfully
