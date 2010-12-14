# $Id$
# Copyright 2010 Philip Gwyn

package POEx::HTTP::Server::Error;

use strict;
use warnings;

use base qw( POEx::HTTP::Server::Response );

sub details
{
    my( $self, $op, $errnum, $errstr ) = @_;
    $self->{op} = $op;
    $self->{errnum} = $errnum;
    $self->{errstr} = $errstr;

    $self->content( "$op error [$errnum] $errstr" );
}

sub op     { $_[0]->{op} }
sub errnum { $_[0]->{errnum} }
sub errstr { $_[0]->{errstr} }
sub errstring { $_[0]->{errstring} }

1;

__END__

=head1 NAME

POEx::HTTP::Server::Error - Object encapsulating an error

=head1 SYNOPSIS

    use POEx::HTTP::Server;

    POEx::HTTP::Server->spawn( handlers => {
                on_error => 'poe:my-alias/error' );

    # events of session my-alias:
    sub error {
        my( $heap, $ereq ) = @_[HEAP,ARG0,ARG1];

        if( $err->op ) {
            warn $err->op, " error [", $err->errnum, "] ", $err->errstr;
        }
        else {
            warn $err->content;
        }
    }


=head1 DESCRIPTION

It is a sub-class of L<HTTP::Request>.


=head1 METHODS


=head2 op

=head2 errnum

=head2 errstr

=head2 errstring

=head1 SEE ALSO

L<POEx::HTTP::Server>, 
L<POEx::HTTP::Server::Request>, 
L<HTTP::Request>.


=head1 AUTHOR

Philip Gwyn, E<lt>gwyn -at- cpan.orgE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2010 by Philip Gwyn

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.8 or,
at your option, any later version of Perl 5 you may have available.


=cut
