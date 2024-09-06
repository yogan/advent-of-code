#!/usr/bin/perl
use strict;
use warnings;
use feature 'say';

use lib 'lib';
use AoC;

my $filename = shift || "./input.txt";
my @ops = AoC::read_file($filename);

say AoC::part1("abcdefgh", @ops);
# say AoC::part2(@boxes);
