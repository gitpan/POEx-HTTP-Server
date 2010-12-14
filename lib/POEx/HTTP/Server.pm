package POEx::HTTP::Server;

use strict;
use warnings;

use Carp qw( carp croak confess cluck );

use POE;
use POE::Wheel::SocketFactory;
use POE::Session::PlainCall;
use POE::Session::Multiplex;
use POEx::HTTP::Server::Error;
use POEx::URI;
use Data::Dump qw( pp );
use Scalar::Util qw( blessed );
use Storable qw( dclone );

our $VERSION = '0.02';


#######################################
sub spawn
{
    my( $package, %options ) = @_;
    my $self = $package->new( %options );
    my $session = $self->build_session;
    return $self->{alias};
}

#######################################
sub new
{
    my( $package, %options ) = @_;
    my $self = bless {}, $package;
    $self->__init( \%options );
    return $self;
}

sub D () { $_[0]->{options}{debug} }

#######################################
sub __init
{
    my( $self, $opt ) = @_;
    $self->{N} = 1;
    $self->{C} = 0;

    $self->{options} = delete $opt->{options};
    $self->{options} ||= {};

    $self->{headers} = delete $opt->{headers};
    $self->{headers} ||= { Server => join '/', ref( $self ), $VERSION };

    $self->{retry} = delete $opt->{retry};
    $self->{retry} = 60 unless defined $self->{retry};

    $self->{concurrency} = delete $opt->{concurrency};
    $self->{concurrency} = -1 unless defined $self->{concurrency};

    $self->{inet}   = delete $opt->{inet};
    my $I = $self->{inet} || {};
    $I->{Listen} ||= 1;
    $I->{Reuse}     = 1  unless defined $I->{Reuse};
    $I->{LocalPort} = 80 unless defined $I->{LocalPort};
    $I->{BindAddr} = delete $I->{LocalAddr} 
                if $I->{LocalAddr} and not defined $I->{BindAddr};
    $I->{BindPort} = delete $I->{LocalPort} 
                if $I->{LocalPort} and not defined $I->{BindPort};

    $self->{alias}  = delete $opt->{alias};
    $self->{alias} ||= 'HTTPd';

    if( $opt->{error} ) {
        $self->{error}  = POEx::URI->new( delete $opt->{error} );
    }

    $self->{keepalive} = delete $opt->{keepalive};
    if( defined $self->{keepalive} and $self->{keepalive} and
            $self->{keepalive} !~ /^\d+$/ ) {
        $self->{keepalive} = 5;
    }
    $self->{keepalive} ||= 0;
    $self->{keepalivetimeout} = delete $opt->{keepalivetimeout};
    if( not defined $self->{keepalivetimeout} and $self->{keepalive} ) {
        $self->{keepalivetimeout} = 15;
    }

    $self->__init_handlers( $opt );
}

#######################################
sub __is_special    
{ 
    $_[0] =~ /^(on_error|on_connect|on_disconnect|pre_request|post_request)$/;
}
sub __init_handlers
{
    my( $self, $opt ) = @_;
    $self->{handlers} = delete $opt->{handlers};

    # handler => URI
    unless( $self->{handlers} ) {
        croak "Missing required handler or handlers param" 
                unless $self->{handler};
        $self->{handlers} = { '' => delete $self->{handler} };
    }
    $self->{todo} = [];
    # handlers => URI
    unless( ref $self->{handlers} ) {
        $self->{todo} = [ '' ];
        $self->{handlers} = { '' => $self->{handlers} };
    }
    # handlers => { match => URI, ... }
    elsif( 'HASH' eq ref $self->{handlers} ) {
        $self->{todo} = [ keys %{ $self->{handlers} } ];
    }
    # handlers => [ match => URI, ... }
    else {
        my %h;
        while( @{ $self->{handlers} } ) {
            my $re = shift @{ $self->{handlers} };
            push @{ $self->{todo} }, $re unless __is_special( $re );
            $h{$re} = shift @{ $self->{handlers} };
        }
        $self->{handlers} = \%h;
    }

    # Get a list of special handlers
    my $H = $self->{handlers};
    my $S = $self->{specials} = {};

    foreach my $re ( keys %$H ) {
        $H->{$re} = POEx::URI->new( $H->{$re}, 'poe' ) unless blessed $H->{$re};
        next unless __is_special( $re );
        $S->{$re} = delete $H->{$re};
    }
    return;
}

#######################################
sub build_session
{
    my( $self ) = @_;

    return POEx::HTTP::Server::Session->create( 
                      options => $self->{options}, 
                      package_states => [
                                ref( $self ) => [ qw( _start _stop shutdown 
                                                      build_server close
                                                      accept error retry
                                                ) ],
                                'POEx::HTTP::Server::Client' => [ qw( 
                                                    input client_error
                                                    timeout 
                                                    shutdown_client
                                                    respond send flushed done
                                                ) ]
                            ],
                      args => [ $self ],
                      heap => { O=>$self }
                    );
}

#######################################
sub build_handle
{
    my( $self ) = @_;
    return %{ $self->{inet} };
}

#######################################
sub build_error
{
    my( $package, $uri ) = @_;
    $uri ||= '/';
    return POEx::HTTP::Server::Error->new( HTTP::Status::RC_INTERNAL_ERROR() );
}



#######################################
sub build_server
{
    my( $self ) = @_;
    $self->D and warn "$self->{alias}: build_server";
    my %invoke = $self->build_handle;
    $self->D and warn "$self->{alias}: ", pp \%invoke;
    $self->{server} = POE::Wheel::SocketFactory->new(
            %invoke,
            SuccessEvent => ev 'accept',
            FailureEvent  => ev 'error'
        );
    return;
}

sub drop
{
    my( $self ) = @_;
    $self->D and warn "$self->{alias}: drop";
    delete $self->{server};
    return;
}

#######################################
sub _start
{
    my( $package, $self ) = @_;
    $self->D and warn "$self->{alias}: _start";
    $poe_kernel->alias_set( $self->{alias} );

    poe->session->object( HTTPd => $self );
    $poe_kernel->sig( shutdown => evo HTTPd => "shutdown" );
    $poe_kernel->yield( evo HTTPd => 'build_server' );
    return;
}

sub done
{
    my( $self ) = @_;
    $self->D and warn "$self->{alias}: done";
    poe->session->object_unregister( 'HTTPd' );
}


#######################################
sub _stop
{
    my( $package ) = @_;
    my $self = poe->heap->{O};
    $self->D and warn "$self->{alias}: _stop";
}

#######################################
sub shutdown
{
    my( $self ) = @_;
    $self->D and warn "$self->{alias}: Shutdown";
    $poe_kernel->alias_remove( $self->{alias} );
    foreach my $name ( keys %{ $self->{clients}||{} } ) {
        $self->D and warn "$self->{alias} shutdown client=$name";
        $poe_kernel->yield( evo $name => 'shutdown_client' );
    }
    $self->drop;
}

#######################################
sub accept
{
    my( $self, $socket, $peer ) = @_;
    
    $self->D and warn "$self->{alias}: accept";
    my $obj = $self->build_client( $self->{N}++, $socket );
    poe->session->object( $obj->name, $obj );
    $obj->build_wheel( $socket );

    $self->concurrency_up;
    $self->{clients}{$obj->name} = 1;

    $obj->on_connect;
}

sub close
{
    my( $self, $name ) = @_;
    $self->D and warn "$self->{alias}: close $name";

    $self->concurrency_down;
    delete $self->{clients}{$name};

    unless( $self->{C} > 0 or $self->{server} ) {
        $self->done;
    }
}

sub concurrency_up
{
    my( $self ) = @_;
    $self->{C}++;
    return unless $self->{concurrency} > 0;
    if( $self->{C} >= $self->{concurrency} ) {
        $self->D and warn "$self->{alias}: pause_accept C=$self->{C}";
        $self->{server}->pause_accept;
        $self->{paused} = 1;
    }
}

sub concurrency_down
{
    my( $self ) = @_;
    $self->{C}--;
    return unless $self->{concurrency} > 0;
    unless( $self->{C} >= $self->{concurrency} and $self->{paused} ) {
        if( $self->{server} ) {
            $self->D and warn "$self->{alias}: resume_accept C=$self->{C}";
            $self->{server}->resume_accept;
        }
        $self->{paused} = 0;
    }
}

#######################################
sub error
{
    my( $self, $op, $errnum, $errstr, $id ) = @_;
    
    $self->D and warn "$self->{alias}: error ($errnum) $errstr";
    delete $self->{server};

    $self->retry;
    $self->on_error( $op, $errnum, $errstr );
}

#######################################
sub on_error
{
    my( $self, $op, $errnum, $errstr ) = @_;

    return unless $self->{specials}{on_error};
    my $req = $self->build_error();
    $req->details( $op, $errnum, $errstr );
    poe->kernel->post( @{ $self->{specials}{on_error} }, $req );
}

#######################################
sub retry
{
    my( $self ) = @_;
    return unless $self->{retry};
    my $tid = poe->kernel->delay_set( ev"do_retry" => $self->{retry} );
    $self->D and warn "$self->{alias}: Retry in $self->{retry} seconds.  tid=$tid.";
    return $tid;
}

#######################################
sub do_retry
{
    my( $self ) = @_;
    $self->D and warn "$self->{alias}: do_retry";
    $self->build_server;
}

#######################################
sub build_client
{
    my( $self, $N, $socket ) = @_;
    my $name = join '-', $self->{alias}, $N;
    return POEx::HTTP::Server::Client->new( 
                    socket => $socket,
                    __close => ev"close",
                    alias => $self->{alias}, 
                    name  => $name, 
                    debug => $self->{options}{debug},
                    todo  => $self->{todo},
                    handlers => dclone $self->{handlers},
                    specials => dclone $self->{specials},
                    headers => $self->{headers},
                    error => $self->{error},
                    keepalive => $self->{keepalive},
                    keepalivetimeout => $self->{keepalivetimeout},
                );
}

##############################################################################
package POEx::HTTP::Server::Client;

use strict;
use warnings;

use Carp;
use HTTP::Status;
use POE;
use POE::Wheel::ReadWrite;
use POE::Filter::HTTPD;
use POEx::HTTP::Server::Request;
use POEx::HTTP::Server::Response;
use POEx::HTTP::Server::Connection;
use POEx::HTTP::Server::Error;
use POE::Session::PlainCall;
use POE::Session::Multiplex;
use POE::Filter::Stream;


use Data::Dump qw( pp );

#######################################
sub new
{
    my( $package, %param ) = @_;

    my $self = bless { %param }, $package;
    $self->build_connect( delete $self->{socket} );
    return $self;

}

sub D () { $_[0]->{debug} }
sub name () { $_[0]->{name} }

#######################################
sub build_wheel
{
    my( $self, $socket ) = @_;

    my $filter = $self->build_filter;
    $self->{wheel} = POE::Wheel::ReadWrite->new( 
                        Handle => $socket,
                        InputEvent => evo( $self->{name}, 'input' ),
                        ErrorEvent => evo( $self->{name}, 'client_error' ),
                        FlushedEvent => evo( $self->{name}, 'flushed' ),
                        Filter     => $filter
                    );
}

sub build_filter
{
    return POE::Filter::HTTPD->new;
}

sub build_stream_filter
{
    return POE::Filter::Stream->new;
}


sub build_connect
{
    my( $self, $socket ) = @_;
    $self->{connection} = 
                POEx::HTTP::Server::Connection->new( $self->{name}, $socket );
}

#######################################
sub on_connect
{
    my( $self ) = @_;
    $self->special_dispatch( on_connect => $self->{connection} );
}

sub on_disconnect
{
    my( $self ) = @_;
    $self->special_dispatch( on_disconnect => $self->{connection} );
}

#######################################
sub client_error
{
    my( $self, $op, $errnum, $errstr, $id ) = @_;
    if( $op eq 'read' and $errnum == 0 ) {  
        # this is a normal error
        $self->D and warn "$self->{name}: $op error ($errnum) $errstr";
    }
    else {
        my $err = POEx::HTTP::Server->build_error;
        $err->details( $op, $errnum, $errstr );
        
        $self->D and warn "$self->{name}: $op error ($errnum) $errstr";
        $self->special_dispatch( on_error => $err );
    }
    $self->close;
}

#######################################
sub close
{
    my( $self ) = @_;
    $self->{will_close} = 0;
    $self->D and warn "$self->{name}: Close";
    poe->kernel->yield( $self->{__close}, $self->name );
    poe->session->object_unregister( $self->{name} );
    $self->on_disconnect;
    $self->close_connection;
    $self->keepalive_stop;
}

sub close_connection
{
    my( $self ) = @_;
    $self->D and warn "$self->{name}: close_connection";
    delete $self->{connection};
    my $W = delete $self->{wheel};
    $W->DESTROY if $W;
    return;
}

sub drop
{
    my( $self ) = @_;
    delete $self->{req};
    delete $self->{resp};
}

sub flushed 
{
    my( $self ) = @_;
    $self->{flushed} = 1;
    $self->D and warn "$self->{name}: Flushed";
    $self->drop;
    $self->close if $self->{will_close};
}

#######################################
sub input
{
    my( $self, $req ) = @_;
    $self->keepalive_stop;

    die "New request while a request is pending ", pp $req if $self->{req};
    if ( $req->isa("HTTP::Response") ) {
        $self->input_error( $req );
        return;
    }

    # Rebless to our package
    $self->{req} = bless $req, 'POEx::HTTP::Server::Request';
    $req->connection( $self->{connection} );

    # Tell the user code
    $self->special_dispatch( 'pre_request', $req );

    # Build response
    $self->{resp} = POEx::HTTP::Server::Response->new(RC_OK);
    $self->{resp}->request( $self->{req} );
    $self->{resp}->{__respond} = [ evs"respond" ];
    $self->{resp}->{__send} = [ evs"send" ];
    $self->{resp}->{__done} = [ evs"done" ];

    $self->{once} = 0;
    $self->{flushed} = 0;

    $self->dispatch;
}

sub input_error
{
    my( $self, $resp ) = @_;
    $self->D and warn "$self->{name}: ERROR ", $resp->status_line;
    bless $resp, 'POEx::HTTP::Server::Error';
    $self->special_dispatch( on_error => $resp );
    $self->{resp} = $resp;
    $self->{shutdown} = 1;
    $self->respond;
}

#######################################
sub dispatch
{
    my( $self ) = @_;
    my $path = $self->{req} && $self->{req}->uri ?
                               $self->{req}->uri->path : '/';

    my( $re, $handler ) = $self->find_handler( $path );
    if( $handler ) {
        $self->invoke( $re, $handler, $self->{req}, $self->{resp} );
    }
    else {
        $self->{resp}->error( RC_NOT_FOUND, "No handler for path $path.\n" );
    }
}
        
#######################################
sub find_handler
{
    my( $self, $path ) = @_;
    $self->D and warn "$self->{name}: Request for $path";
    foreach my $re ( @{ $self->{todo} } ) {
        next unless $re eq '' or $path =~ /$re/;
        return( $re, $self->{handlers}{$re} );
    }
    return;
}

#######################################
sub invoke
{
    my( $self, $re, $handler, @args ) = @_;
    $self->D and warn "$self->{name}: Invoke handler for '$re' ($handler)";
    eval { poe->kernel->call( @$handler, @args ) };
    if( $@ ) {
        warn $@;
        $self->{resp}->error( RC_INTERNAL_SERVER_ERROR, $@ );
    }
}

#######################################
sub special_dispatch
{
    my( $self, $why, @args ) = @_;

    my $handler = $self->{specials}{$why};
    return unless $handler;
    $self->invoke( $why, $handler, @args );
}
        
#######################################
sub respond
{
    my( $self ) = @_;

    $self->D and warn "$self->{name}: respond";
    confess "Responding more then once to a request" if $self->{once};
    $self->{once}++;

    unless( $self->{resp}->sent ) {
        $self->should_close;
        $self->send_headers;
    }

    $self->{resp}->content( undef() );
    return;
}

sub send_headers
{
    my( $self ) = @_;

    $self->D and warn "$self->{name}: Response: ".$self->{resp}->status_line;
    $self->__fix_headers;
    $self->{wheel}->put( $self->{resp} );
    $self->{resp}->sent( 1 );
}

#######################################
sub send
{
    my( $self, $something ) = @_;
    confess "Responding more then once to a request" unless $self->{resp};
    unless( $self->{resp}->sent ) {
        $self->should_close;
        $self->send_headers;
        $self->{wheel}->set_output_filter( $self->build_stream_filter );
    }
    $self->{wheel}->put( $something );
    return;
}


#######################################
sub __fix_headers
{
    my( $self ) = @_;
    while( my( $h, $v ) = each %{$self->{headers}} ) {
        next if $self->{resp}->header( $h );
        $self->{resp}->header( $h => $v);
    }
}

#######################################
sub should_close
{
    my( $self ) = @_;
    $self->{will_close} = 1;
    if ( $self->{req} and $self->{req}->protocol eq 'HTTP/1.1' ) {
        $self->{will_close} = 0;                   # keepalive
        # It turns out the connection field can contain multiple
        # comma separated values
        my $conn = $self->{req}->header('Connection')||'';
        $self->{will_close} = 1 if qq(,$conn,) =~ /,\s*close\s*,/i;
        #warn "conn=$conn will_close=$self->{will_close}";
        # Allow handler code to control the connection
        $conn = $self->{resp}->header('Connection')||'';
        $self->{will_close} = 1 if qq(,$conn,) =~ /,\s*close\s*,/i;
        #warn "conn=$conn will_close=$self->{will_close}";
    }
    else {
        # HTTP/1.0-style keep-alives fail
        #my $conn = $self->{req}->header('Connection')||'';
        #$self->{will_close} = 0 if qq(,$conn,) =~ /,\s*keep-alive\s*,/i;
        #warn "conn=$conn will_close=$self->{will_close}";
    }

    $self->{will_close} = 0 if $self->{resp}->streaming;
    #warn "post streaming will_close=$self->{will_close}";
    $self->{will_close} = 1 unless $self->{keepalive} > 1;
    #warn "post keepalive will_close=$self->{will_close}";
    $self->{will_close} = 1 if $self->{shutdown};
    $self->D and warn "$self->{name}: should_close = $self->{will_close}";
    return $self->{will_close};
}

#######################################
sub done
{
    my( $self ) = @_;
    $self->special_dispatch( 'post_request', $self->{req}, $self->{resp} );
    $self->drop;
    $self->keepalive_start;
}

#######################################
sub keepalive_start
{
    my( $self ) = @_;
    return if $self->{will_close};
    $self->{keepalive}--;
    return unless $self->{keepalive} > 0;
    $self->D and warn "$self->{name}: keep-alive=$self->{keepalive}";
    $self->D and warn "$self->{name}: keep-alive timeout=$self->{keepalivetimeout}";
    $self->{TID} = poe->kernel->delay_set( ev"timeout", 
                                               $self->{keepalivetimeout} 
                                             );
    $self->D and warn "$self->{name}: keep-alive start tid=$self->{TID}";
}

sub timeout
{
    my( $self ) = @_;
    $self->keepalive_stop;
    $self->close;
}

#######################################
sub keepalive_stop
{
    my( $self ) = @_;
    return unless $self->{TID};
    $self->D and warn "$self->{name}: keep-alive stop tid=$self->{TID}";
    poe->kernel->alarm_remove( delete $self->{TID} );
}

#######################################
sub shutdown_client
{
    my( $self ) = @_;
    $self->D and warn "$self->{name}: shutdown_client";
    $self->{shutdown} = 1;
    $self->{will_close} = 1;
    $self->close if $self->{flushed};
    $self->keepalive_stop;
}


##############################################################################
package POEx::HTTP::Server::Session;

use strict;
use warnings;

use POE::Session::PlainCall;
use POE::Session::Multiplex;

use Data::Dump qw( pp );
use base qw( POE::Session::Multiplex POE::Session::PlainCall );



1;

__END__

=head1 NAME

POEx::HTTP::Server - HTTP server in pure POE

=head1 SYNOPSIS

    use POEx::HTTP::Server;

    POEx::HTTP::Server->spawn( 
                    inet => {
                                LocalPort => 80 
                            },
                    handlers => [
                                '^/$' => 'poe:my-alias/root',
                                '^/static' => 'poe:my-alias/static',
                                '' => 'poe:my-alias/error'
                            ]
                    );
                

    # events of session my-alias:
    sub root {
        my( $heap, $req, $resp ) = @_[HEAP,ARG0,ARG1];
        $resp->content_type( 'text/html' );
        $resp->content( << HTML );
<html>...</html>
HTML
        $resp->done;
    }

    sub static {
        my( $heap, $req, $resp ) = @_[HEAP,ARG0,ARG1];
        my $file = File::Spec->catfile( $heap->{root}, $req->path );
        $resp->sendfile( $file );
    }

    sub error {
        my( $heap, $req, resp ) = @_[HEAP,ARG0,ARG1];
        $resp->error( 404, "Nothing to do for ".$req->path );
    }


=head1 DESCRIPTION

POEx::HTTP::Server is a clean reimplementation of an HTTP server.  It uses
L<POEx::URI> to simplify event specification.  It allows limiting connection
concurrency.  



POEx::HTTP::Server differs from L<POE::Component::Server::HTTP> by having a cleaner
code base and still being maintained.

POEx::HTTP::Server differs from L<POE::Component::Server::SimpleHTTP> by not
using Moose and not using the YELLING-STYLE of parameter passing.



=head1 METHODS

=head2 spawn

    POEx::HTTP::Server->spawn( %CONFIG );

Spawns the server session.  C<%CONFIG> contains one or more of the following
parameters:

=head3 inet

    POEx::HTTP::Server->spawn( inet => $HASHREF );

Specify the parameters handed to L<POE::Wheel::SocketFactory> when creating
the listening socket.

As a convenience, C<LocalAddr> is changed into C<BindAddress> and 
C<LocalPort> into C<BindPort>.


Defaults to:

    POEx::HTTP::Server->spawn( inet => { Listen=>1, BindPort=> 80 } );


=head3 handlers

    POEx::HTTP::Server->spawn( handlers => $HASHREF );
    POEx::HTTP::Server->spawn( handlers => $ARRAYREF );

Set the events that handle a request.  Keys to C<$HASHREF> are regexes which 
match on all or part of the request path.  Values are L<url|POEx::URI> to
the events that will handle the request.

The regexes are not anchored.  This means that C</foo> will match the path 
C</something/foo>.  Use C<^> if that's what you mean; C<^/foo>.

Specifiying an C<$ARRAYREF> allows you to control the order in which 
the regexes are matched:

    POEx::HTTP::Server->spawn( handlers => [ 
                        'foo'  => 'poe:my-session/foo',
                        'onk'  => 'poe:my-session/onk',
                        'honk' => 'poe:my-session/honk',
                    ] );
    
The handler for C<onk> will always match before C<honk> can.

Use C<''> if you want a catchall handler.

See L<HANDLERS> below.

=head3 handler

    POEx::HTTP::Server->spawn( handler => $uri );

Syntatic sugar for

    POEx::HTTP::Server->spawn( handler => [ '' => $uri ] );

=head3 alias

    POEx::HTTP::Server->spawn( alias => $ALIAS );
    
Sets the server session's alias.  The alias defaults to 'HTTPd'.

=head3 concurrency

    POEx::HTTP::Server->spawn( concurrency => $NUM );
    
Sets the request concurrency level; this is the number of requests that 
may be serviced in parallel.  Defaults to (-1) infinit concurrency.

=head3 headers

    POEx::HTTP::Server->spawn( concurrency => $HASHREF );

All the key/value pairs in C<$HASHREF> will be set as HTTP headers on
all responses.

=head3 keepalive

    POEx::HTTP::Server->spawn( keepalive => $N );

=head3 keepalivetimeout

    POEx::HTTP::Server->spawn( keepalivetimeout => $TIME );


=head3 options

    POEx::HTTP::Server->spawn( options => $HASHREF );

Options passed L<POE::Session>->create.  You may also specify C<debug> to 
turn on some debugging output.

=head3 retry

    POEx::HTTP::Server->spawn( retry => $SECONDS );

If binding to the port fails, the server will wait C<$SECONDS> to retry the 
operation.

Defaults to 60.  Use 0 to turn retry off.

=head1 HANDLERS

A handler is a POE event that will handle a given HTTP request.  C<ARG0> is
a L<POEx::HTTP::Server::Request> object.  C<ARG1> is a
L<POEx::HTTP::Server::Response> object.  The handler should query the
request object for details or parameters of the request.  

    my $req = $_[ARG0];
    my $file = File::Spec->catfile( $doc_root, $req->uri->path );
    
    my $query = $req->uri->query_form;

    my $conn = $req->connection;
    my $ip   = $conn->remote_ip;
    my $port = $conn->remote_port;

The handler must populate the response object with necessary headers and
content.  If the handler wishes to send an error to the browser, it may set
the response code.  A default code of RC_OK (200) is used.  The response is
the send to the browser with either C<respond> or C<send>.  When the handler
is done, C<done> is called on the response object.

    my $resp = $_[ARG1];
    $resp->content_type( 'text/plain' );
    $resp->content( "Hello world\n" );
    $resp->respond;
    $resp->done;

    use HTTP::Status;
    $resp->code( RC_FOUND );
    $resp->header( 'Location' => $new_uri );

    $resp->content_type( 'text/plain' );
    my $io = IO::File->new( $file );
    while( <$io> ) {
        $resp->send( $_ );
    }
    $resp->done;

The last example is silly.  It would be better to use C<sendfile> like so:

    $resp->content_type( 'image/gif' );
    $resp->sendfile( $file );
    # Don't call ->done after sendfile

Handlers may chain to other event handlers, using normal POE events.  You must
keep track of at least the request handler so that you may call C<done> when
the request is finished.

Here is an example of an unrolled loop:

    sub handler {
        my( $heap, $resp ) = $_[HEAP,ARG1];
        $heap->{todo} = [ qw( one two three ) ];
        $poe_kernel->yield( next_handler => $resp );
    }

    sub next_handler {
        my( $heap, $resp ) = $_[HEAP,ARG0];

        # Get the request object from the response
        my $req = $resp->request;
        # And you can get the connection object from the request

        my $h = shift @{ $heap->{todo} };
        if( $h ) {
            # Send the content returned by event handlers in another session
            $resp->send( $poe_kernel->call( $heap->{session}, $h, $req ) );
            $poe_kernel->yeild( next_handler => $resp );
        }
        else {
            $poe_kernel->yield( 'last_handler', $resp );
        }
    }

    sub last_handler {
        my( $heap, $resp ) = $_[HEAP,ARG0];
        $resp->done;
    }


=head2 Handler parameters

POE URIs are allowed to have their own parameter.  If you use them, they
will appear as a hashref in C<ARG0> with the request and response objects as
C<ARG1> and C<ARG2> respectively.

    POEx::HTTP::Server->spawn( handler => 'poe:my-session/handler?honk=bonk' );

    sub handler {
        my( $args, $req, $resp ) = @_[ARG0, ARG1, ARG2];
        # $args = { honk => 'bonk' }
    }


=head2 Special handlers

There are 5 special handlers that are invoked when a browser connection is opened and closed,
before and after each request and when an error occurs.

The note about L<handler parameters> also aplies to special handlers.

=head3 on_connect

Invoked when a new connection is made to the server.  C<ARG0> is a
L<POEx::HTTP::Server::Connection> object that may be queried for
information. This connection object is shared by all requests objects that
use this connection.  

    POEx::HTTP::Server->spawn( 
                        handlers => { on_connect => 'poe:my-session/on_connect' }
                     );
    sub on_connect {
        my( $object, $connection ) = @_[OBJECT, ARG0];
        # ...
    }

It goes without saying that if you use L</keepalive> L</pre_request> will be
invoked more often then C<on_connect>.

=head3 on_disconnect

Invoked when a connection is closed. C<ARG0> is the same
L<POEx::HTTP::Server::Connection> object that was passed to L<on_connect>.

=head3 pre_request

Invoked after a request is read from the browser but before it is processed.
C<ARG0> is a L<POEx::HTTP::Server::Request> object.  There is no C<ARG1>.

    POEx::HTTP::Server->spawn( 
                        handlers => { pre_request => 'poe:my-session/pre' }
                     );
    sub pre {
        my( $object, $request ) = @_[OBJECT, ARG0];
        my $connection = $request->connection;
        # ...
    }

=head3 post_request

Invoked after a response has been sent to the browser.  
C<ARG0> is a L<POEx::HTTP::Server::Request> object.  
C<ARG1> is a L<POEx::HTTP::Server::Response> object, with it's C<content> cleared.

    POEx::HTTP::Server->spawn( 
                        handlers => { pre_request => 'poe:my-session/post' }
                     );
    sub post {
        my( $self, $request, $response ) = @_[OBJECT, ARG0, ARG1];
        my $connection = $request->connection;
        # ...
    }

=head3 on_error

Invoked when the server detects an error. C<ARG0> is a
L<POEx::HTTP::Server::Error> object.  There are 2 types of errors: network
errors and HTTP errors.  They are distiguished by calling the error object's
C<op> method.  If C<op> returns undef(), it is an HTTP error, otherwise a
network error.  HTTP error already has a message to the browser with HTML
content. You may modify the HTTP error's content and headers before they get
sent back to the browser.

    POEx::HTTP::Server->spawn( 
                        handlers => { on_error => 'poe:my-session/error' }
                     );
    sub error {
        my( $self, $err ) = @_[OBJECT, ARG0];
        if( $err->op ) {    # network error
            $self->LOG( $err->op." error [".$err->errnum, "] ".$err->errstr );
            # or the equivalent
            $self->LOG( $err->content );
        }
        else {              # HTTP error
            $self->LOG( $err->status_line );
            $self->content_type( 'text/plain' );
            $self->content( "Don't do that!" );
        }
    }
    
=head1 EVENTS

=head2 shutdown

    $poe_kernel->signal( $poe_kernel => 'shutdown' );
    $poe_kernel->post( HTTPd => 'shutdown' );

Initiate server shutdown.  Note however that any pending requests will stay 
active.  The session will exit when the last of the requests has exited.

=head2 handlers_get

B<TODO>

=head2 handlers_set

B<TODO>

=head2 handlers_add

B<TODO>

=head2 handlers_remove

B<TODO>

=head1 SEND HEADERS

B<TO BE WRITEN>

=head1 STREAMING

B<TO BE WRITEN>

=head1 SEE ALSO

L<POE>

=head1 AUTHOR

Philip Gwyn, E<lt>gwyn -at- cpan.orgE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2010 by Philip Gwyn

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.8 or,
at your option, any later version of Perl 5 you may have available.


=cut
