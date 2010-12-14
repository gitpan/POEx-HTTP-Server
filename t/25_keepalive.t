#!/usr/bin/perl

use strict;
use warnings;

sub DEBUG () { 0 }

use Test::More;

use Data::Dump qw( pp );
use IO::Socket::INET;
use POEx::HTTP::Server;
use URI;

use t::Server;



eval "use LWP::UserAgent";
if( $@ ) {
    plan skip_all => "LWP::UserAgent isn't available";
    exit 0;
}

eval "use LWP::ConnCache";
if( $@ ) {
    plan skip_all => "LWP::ConnCache isn't available";
    exit 0;
}

plan tests => 8;


###################################################################
my $sock = IO::Socket::INET->new( LocalAddr => 0, Listen => 1, ReuseAddr => 1 );

my $uri = URI->new( "http://".$sock->sockhost.":".$sock->sockport );
DEBUG and 
    diag "Listen on $uri";

undef( $sock );

my $pid = open( CHILD, "-|" );
defined($pid) or die "Unable to fork: $!";
unless( $pid ) {    # Child
    $poe_kernel->has_forked;
    spawn( $uri->port, 5 ); 
    $poe_kernel->run;
    exit 0;
}


#######################################
# parent
diag "Sleep 1";
sleep 1;
my $UA = LWP::UserAgent->new;
$UA->agent("$0/0.1 " . $UA->agent);
my $CC = LWP::ConnCache->new();
$UA->conn_cache( $CC );

##### Test Keep-alive
$uri->path( '/dynamic/debug.txt' );
my $req = HTTP::Request->new( GET => $uri );
my $resp = $UA->request( $req );

ok( $resp->is_success, "One request" ) or die Dumper $resp;

our( $REQ, $RESP );
eval $resp->content;
die $@ if $@;

my $id = $REQ->connection->ID;
ok( $id, "Got a connection ID" );

#####
$req = HTTP::Request->new( GET => $uri );
$resp = $UA->request( $req );

ok( $resp->is_success, "One request" ) or die Dumper $resp;
eval $resp->content;
die $@ if $@;

is( $REQ->connection->ID, $id, "Same connection" );

#####
$CC->drop;

$req = HTTP::Request->new( GET => $uri );
$resp = $UA->request( $req );

ok( $resp->is_success, "Third request" ) or die Dumper $resp;
eval $resp->content;
die $@ if $@;

isnt( $REQ->connection->ID, $id, "New connection" );



#####
$uri->path( '/dynamic/shutdown' );
$req = HTTP::Request->new( GET => $uri );
$resp = $UA->request( $req );
ok( $resp->is_success, "GET $uri" ) or die "Failed: ", pp $resp;

while( <CHILD> ) {
    diag( $_ );
}


#####
END {
    if( $pid ) {
        kill 10, $pid;
        DEBUG and diag "PID=$pid";
        my $kid = waitpid( $pid, 0 );
        is( $?, 0, "Sane exit" );
    }
}


