#!/usr/bin/perl

use strict;
use warnings;

use Test::More;
eval "use Test::Pod::Coverage 1.00";
plan skip_all => "Test::Pod::Coverage 1.00 required for testing POD coverage" if $@;

plan tests => 3;

pod_coverage_ok(
        "POEx::HTTP::Server",
        { also_private => [ qw( D new retry do_retry drop accept build_client 
                            build_server build_session build_handle close done
                            concurrency_up concurrency_down ) ], 
        },
        "POEx::HTTP::Server, ignoring private functions",
);

pod_coverage_ok(
        "POEx::HTTP::Server::Request",
        { also_private => [ qw( DEBUG socket ) ], 
        },
        "POEx::HTTP::Server::Request, ignoring private functions",
);

pod_coverage_ok(
        "POEx::HTTP::Server::Response",
        { also_private => [ qw( DEBUG ) ], 
        },
        "POEx::HTTP::Server::Response, ignoring private functions",
);

