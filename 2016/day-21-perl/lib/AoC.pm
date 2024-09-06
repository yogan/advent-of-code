package AoC;

use strict;
use warnings;
use Data::Dumper;
use Test2::V0;

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

sub part1 {
    my ( $string, @ops ) = @_;

    for my $op (@ops) {
        if ( $op =~ /swap position (\d+) with position (\d+)/ ) {
            $string = swap_position $string, $1, $2;
        }
        elsif ( $op =~ /swap letter (\w) with letter (\w)/ ) {
            $string = swap_letter $string, $1, $2;
        }
        elsif ( $op =~ /rotate left (\d+) step/ ) {
            $string = rotate_left $string, $1;
        }
        elsif ( $op =~ /rotate right (\d+) step/ ) {
            $string = rotate_right $string, $1;
        }
        elsif ( $op =~ /rotate based on position of letter (\w)/ ) {
            $string = rotate_letter $string, $1;
        }
        elsif ( $op =~ /reverse positions (\d+) through (\d+)/ ) {
            $string = reverse_positions $string, $1, $2;
        }
        elsif ( $op =~ /move position (\d+) to position (\d+)/ ) {
            $string = move_position $string, $1, $2;
        }
    }

    return $string;
}

1;    # indicate that the module loaded successfully
