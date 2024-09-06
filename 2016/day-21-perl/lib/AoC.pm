package AoC;

use strict;
use warnings;

sub read_file {
    my $filename = shift;
    open my $fh, '<', $filename or die "Could not open file '$filename' $!";
    my @lines = <$fh>;
    close $fh;
    return @lines;
}

sub swap_position {
    my ( $string, $i, $j ) = @_;
    my @chars = split //, $string;
    ( $chars[$i], $chars[$j] ) = ( $chars[$j], $chars[$i] );
    return join '', @chars;
}

sub swap_letter {
    my ( $string, $l, $k ) = @_;
    return swap_position $string, index( $string, $l ), index( $string, $k );
}

sub rotate_left {
    my ( $string, $x ) = @_;
    $x = $x % ( length $string );
    my $left  = substr $string, $x;
    my $right = substr $string, 0, $x;
    return $left . $right;
}

sub rotate_right {
    my ( $string, $x ) = @_;
    return rotate_left $string, ( length $string ) - $x;
}

sub rotate_letter {
    my ( $string, $c ) = @_;
    my $i = index $string, $c;
    my $x = $i < 4 ? 1 : 2;
    return rotate_right $string, $i + $x;
}

sub rotate_letter_inverse {
    my ( $string, $c ) = @_;

    # we just brute force this, since it's only 8 possible rotations
    for my $i ( 0 .. length($string) - 1 ) {
        my $candidate = rotate_left $string, $i;
        return $candidate if rotate_letter( $candidate, $c ) eq $string;
    }
}

sub reverse_positions {
    my ( $string, $i, $j ) = @_;
    my @chars = split //, $string;
    @chars[ $i .. $j ] = reverse @chars[ $i .. $j ];
    return join '', @chars;
}

sub move_position {
    my ( $string, $i, $j ) = @_;
    my @chars = split //, $string;
    my $c     = splice @chars, $i, 1;
    splice @chars, $j, 0, $c;
    return join '', @chars;
}

sub solve {
    my ( $unscramble, $string, @ops ) = @_;

    foreach (@ops) {
        if (/swap position (\d+) with position (\d+)/) {
            $string = swap_position $string, $1, $2;
        }
        elsif (/swap letter (\w) with letter (\w)/) {
            $string = swap_letter $string, $1, $2;
        }
        elsif (/rotate left (\d+) step/) {
            $string =
              $unscramble
              ? rotate_right $string, $1
              : rotate_left $string, $1;
        }
        elsif (/rotate right (\d+) step/) {
            $string =
              $unscramble
              ? rotate_left $string, $1
              : rotate_right $string, $1;
        }
        elsif (/rotate based on position of letter (\w)/) {
            $string =
              $unscramble
              ? rotate_letter_inverse $string, $1
              : rotate_letter $string, $1;
        }
        elsif (/reverse positions (\d+) through (\d+)/) {
            $string = reverse_positions $string, $1, $2;
        }
        elsif (/move position (\d+) to position (\d+)/) {
            $string =
              $unscramble
              ? move_position $string, $2, $1
              : move_position $string, $1, $2;
        }
    }

    return $string;
}

1;    # indicate that the module loaded successfully
