#!/usr/bin/perl
use strict;
use warnings;
use feature 'say';

use lib 'lib';
use AoC;

my $filename = shift || "./input.txt";
my @lines    = AoC::read_file($filename);
my @boxes    = AoC::parse_input(@lines);

say AoC::part1(@boxes);
say AoC::part2(@boxes);
