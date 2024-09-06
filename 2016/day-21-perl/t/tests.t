use strict;
use warnings;

use Test2::V0 -target => 'AoC';
use Test2::Tools::Spec;

describe 'operations' => sub {
    it 'swap_position works' => sub {
        is AoC::swap_position( "abcde", 0, 4 ), "ebcda";
    };

    it 'swap_letter works' => sub {
        is AoC::swap_letter( "ebcda", "d", "b" ), "edcba";
    };

    it 'rotate_left works' => sub {
        is AoC::rotate_left( "abcde", 0 ), "abcde";
        is AoC::rotate_left( "abcde", 1 ), "bcdea";
        is AoC::rotate_left( "abcde", 4 ), "eabcd";
        is AoC::rotate_left( "abcde", 5 ), "abcde";
        is AoC::rotate_left( "abcde", 6 ), "bcdea";
    };

    it 'rotate_right works' => sub {
        is AoC::rotate_right( "abcde", 0 ), "abcde";
        is AoC::rotate_right( "abcde", 1 ), "eabcd";
        is AoC::rotate_right( "abcde", 4 ), "bcdea";
        is AoC::rotate_right( "abcde", 5 ), "abcde";
        is AoC::rotate_right( "abcde", 6 ), "eabcd";
    };

    it 'rotate_letter works' => sub {
        is AoC::rotate_letter( "abdec", "b" ), "ecabd";
        is AoC::rotate_letter( "ecabd", "d" ), "decab";
    };

    it 'reverse_positions works' => sub {
        is AoC::reverse_positions( "edcba", 0, 4 ), "abcde";
        is AoC::reverse_positions( "edcba", 1, 3 ), "ebcda";
    };

    it 'move_position works' => sub {
        is AoC::move_position( "bcdea", 1, 4 ), "bdeac";
        is AoC::move_position( "bcdea", 1, 2 ), "bdcea";
        is AoC::move_position( "bdeac", 3, 0 ), "abdec";
    };
};

describe 'inverse operations' => sub {
    my $string = "abcde";

    it 'swap_position is its own inverse' => sub {
        my $tmp = AoC::swap_position( $string, 0, 4 );
        is AoC::swap_position( $tmp, 0, 4 ), $string;
    };

    it 'swap_letter is its own inverse' => sub {
        my $tmp = AoC::swap_letter( $string, "d", "b" );
        is AoC::swap_letter( $tmp, "d", "b" ), $string;
    };

    it 'rotate_left can be inverted with rotate_right' => sub {
        my $tmp = AoC::rotate_left( $string, 1 );
        is AoC::rotate_right( $tmp, 1 ), $string;
    };

    it 'rotate_right can be inverted with rotate_left' => sub {
        my $tmp = AoC::rotate_right( $string, 1 );
        is AoC::rotate_left( $tmp, 1 ), $string;
    };

    it 'rotate_letter can be inverted with rotate_letter_inverse' => sub {
        my $tmp = AoC::rotate_letter( $string, "b" );
        is AoC::rotate_letter_inverse( $tmp, "b" ), $string;

        $tmp = AoC::rotate_letter( $string, "d" );
        is AoC::rotate_letter_inverse( $tmp, "d" ), $string;

        $tmp = AoC::rotate_letter( $string, "e" );
        is AoC::rotate_letter_inverse( $tmp, "e" ), $string;
    };

    it 'reverse_positions is its own inverse' => sub {
        my $tmp = AoC::reverse_positions( $string, 0, 4 );
        is AoC::reverse_positions( $tmp, 0, 4 ), $string;

        $tmp = AoC::reverse_positions( $string, 2, 3 );
        is AoC::reverse_positions( $tmp, 2, 3 ), $string;
    };

    it 'move_position can be inverted by swapping the indices' => sub {
        my $tmp = AoC::move_position( $string, 1, 4 );
        is AoC::move_position( $tmp, 4, 1 ), $string;

        $tmp = AoC::move_position( $string, 3, 0 );
        is AoC::move_position( $tmp, 0, 3 ), $string;
    };
};

describe 'part 1' => sub {
    it 'works for the example' => sub {
        my @sample = (
            "swap position 4 with position 0",
            "swap letter d with letter b",
            "reverse positions 0 through 4",
            "rotate left 1 step",
            "move position 1 to position 4",
            "move position 3 to position 0",
            "rotate based on position of letter b",
            "rotate based on position of letter d",
        );

        is AoC::solve( 0, "abcde", @sample ), "decab";
    };
};

done_testing;
