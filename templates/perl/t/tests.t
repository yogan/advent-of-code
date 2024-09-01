use strict;
use warnings;

use Test2::V0 -target => 'AoC';
use Test2::Tools::Spec;
use Data::Dumper;

describe 'parse_input()' => sub {
    it 'should handle an empty array' => sub {
        is [ AoC::parse_input() ], [];
    };

    it 'should parse "1x2x3" and "42x23x999" correctly' => sub {
        my @input    = ( "1x2x3", "42x23x999" );
        my @expected = ( [ 1, 2, 3 ], [ 42, 23, 999 ] );
        my @result   = AoC::parse_input(@input);
        is \@result, \@expected;
    };
};

describe 'parts 1 and 2' => sub {
    my @boxes = ( [ 1, 2, 3 ], [ 1, 1, 1 ] );

    it 'part1 returns sum of volumes' => sub {
        is AoC::part1(@boxes), 6 + 1;
    };

    it 'part2 returns sum of surface areas' => sub {
        is AoC::part2(@boxes), ( 2 + 2 + 3 + 3 + 6 + 6 ) + ( 6 * 1 );
    };
};

done_testing;
