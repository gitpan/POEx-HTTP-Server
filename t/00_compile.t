#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 3;
BEGIN { 
    use_ok('POEx::HTTP::Server::Request');
    use_ok('POEx::HTTP::Server::Response');
    use_ok('POEx::HTTP::Server');
}


