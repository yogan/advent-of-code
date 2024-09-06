#!/usr/bin/perl
use strict;
use warnings;
use feature 'say';

use lib 'lib';
use AoC;

my $filename = shift || "./input.txt";
my @ops      = AoC::read_file($filename);
my @rev_ops  = reverse @ops;

say AoC::solve( 0, "abcdefgh", @ops );
say AoC::solve( 1, "fbgdceah", @rev_ops );
