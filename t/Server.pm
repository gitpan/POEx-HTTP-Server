use strict;
use warnings;

use Test::More;

use Data::Dump qw( pp );
use IO::Socket::INET;
use POE;
use POEx::HTTP::Server;
use URI;

################################################################################
sub spawn
{
    my( $PORT, $KA, $prefork ) = @_;

    $KA ||= 0;

    POE::Session->create(
            package_states => [ 
                    Worker1 => [ qw( _start _stop shutdown error static root USR1 ) ]
                ],
            heap => {
                    alias => 'worker1'
                }
        );

    POE::Session->create(
            package_states => [ 
                    Worker2 => [ qw( _start _stop shutdown dynamic dynamic_resp ) ]
                ],
            heap => {
                    alias => 'worker2'
                }
        );

    # $DB::single = 1;
    POEx::HTTP::Server->spawn(
            inet  => { BindPort => $PORT },
            keepalive => $KA,
            prefork => $prefork,
            alias => 'web-server',
            options => { debug => ::DEBUG, trace => 0, default => 0 },
            headers => { Server => "$0/0.1" },
            handlers => [
                    '^/$'       => 'poe:worker1/root',
                    '^/static'  => 'poe:worker1/static',
                    ''          => 'poe:worker2/dynamic',
                ]
        );
}

sub spawn_prefork
{
    POE::Component::Daemon->spawn(
            alias   => 'Daemon',
            verbose => ::DEBUG,
            start_children  => 2,
            requests        => 1,
            min_spare       => 2,
            max_children    => 10,
        );
}


################################################################################
package Worker;

use strict;
use warnings;
use POE;

#######################################
sub _start
{
    my( $heap, $kernel ) = @_[HEAP, KERNEL];
    $kernel->alias_set( $heap->{alias} );
    ::DEBUG and warn "$$:$heap->{alias}: _start";
    $kernel->sig( shutdown => 'shutdown' );
}

#######################################
sub _stop 
{
    my( $heap, $kernel ) = @_[HEAP, KERNEL];
    ::DEBUG and warn "$$:$heap->{alias}: _stop";
}

#######################################
sub shutdown
{
    my( $heap, $kernel ) = @_[HEAP, KERNEL];
    $kernel->alias_remove( $heap->{alias} );
    ::DEBUG and warn "$$:$heap->{alias}: shutdown";
}

################################################################################
package Worker1;

use strict;
use warnings;
use POE;
use Data::Dump qw( pp );

use base qw( Worker );

#######################################
sub _start
{
    my( $heap, $kernel ) = @_[ HEAP, KERNEL ];
    shift->SUPER::_start( @_ );
    $kernel->sig( USR1 => 'USR1' );
}

#######################################
sub USR1
{
    my( $heap, $kernel ) = @_[HEAP, KERNEL];
    ::DEBUG and warn "$$:$heap->{alias}: USR1";
    $kernel->signal( $kernel => 'shutdown' );
}

#######################################
sub error
{
    my( $heap, $kernel, $req, $resp ) = @_[HEAP, KERNEL, ARG0, ARG1];
    die "$heap->{alias}: ERROR=", pp $req;
}

#######################################
sub root
{
    my( $heap, $kernel, $req, $resp ) = @_[HEAP, KERNEL, ARG0, ARG1];
    ::DEBUG and warn "$$:$heap->{alias}: root";
    $resp->content_type( 'text/html' );
    $resp->content(<<HTML);
<html>
<head><title>Hello world</title></head>
<body>
<h1>Hello and welcome to my unit test</h1>
<ul>
    <li><a href="/static/something.txt">something.txt</a></li>
    <li><a href="/dynamic/debug.txt">debug.txt</a></li>
</ul>
</body>
HTML
    $resp->respond;
    $resp->done;
    return;
}

#######################################
sub static
{
    my( $heap, $kernel, $req, $resp ) = @_[HEAP, KERNEL, ARG0, ARG1];
    my $path = $req->uri->path;
    $path =~ s/\/static/t/;
    ::DEBUG and warn "$$:$heap->{alias}: static $path";
    $resp->sendfile( $path );
}




################################################################################
package Worker2;

use strict;
use warnings;

use HTTP::Status;
use POE;
use Data::Dump qw( pp );
use base qw( Worker );

#######################################
sub dynamic
{
    my( $self, $heap, $kernel, $req, $resp ) = @_[OBJECT,HEAP, KERNEL, ARG0, ARG1];
    ::DEBUG and warn "$$:$heap->{alias}: dynamic";

    my $path = $req->uri->path;
    if( $path =~ /shutdown/ ) {
        return $self->do_shutdown( $resp );
    }
    elsif( $path =~ /posted/ ) {
        return $self->do_post( $req, $resp );
    }
    $kernel->yield( dynamic_resp => $req, $resp );
}

sub dynamic_resp
{
    my( $heap, $kernel, $req, $resp ) = @_[HEAP, KERNEL, ARG0, ARG1];
    ::DEBUG and warn "$$:$heap->{alias}: dynamic_resp";

    my $rq = pp $req;
    my $rp = pp $resp;
    # my $ev = pp \%ENV;
    $resp->content_type( 'text/plain' );
    $resp->content(<<TXT);
\$PID=$$;
\$REQ=$rq;
\$RESP=$rp;
TXT
    $resp->respond;
    $resp->done;
}

sub do_shutdown 
{
    my( $self, $resp ) = @_;    
    $poe_kernel->signal( $poe_kernel, 'shutdown' );
    $resp->content( 'OK' );
    $resp->respond;
    $resp->done;
    return;
}

sub do_post
{
    my( $self, $req, $resp ) = @_;
    unless( $req->content_type eq 'application/x-www-form-urlencoded' ) {
        return $resp->error( RC_BAD_REQUEST, 'Wrong content-type' );
    }
    my %in;
    foreach my $c ( split /&/, $req->content ) {
        my( $k, $v ) = split /=/, $c, 2;
        $in{$k} = $v;
    }
    $resp->content_type( 'text/plain' );
    $resp->content( join "\n", sort( values %in ), '' );
    $resp->respond;
    $resp->done;
}

1;
