use strict;
use warnings;

use Test2::V0 -target => 'AoC';
use Test2::Tools::Spec;
use Data::Dumper;

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

        is AoC::part1( "abcde", @sample ), "decab";
    };
};

done_testing;
