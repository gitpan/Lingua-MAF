# $Id: MAF.pm,v 1.9 2006/09/01 09:40:44 rousse Exp $
package Lingua::MAF;

=head1 NAME

Lingua::MAF - Multi-Annotation Framework format writer

=head1 VERSION

Version 0.1.1

=head1 DESCRIPTION

This module allows to easily produces MAF-format documents. MAF
(Morpho-Syntactic Annotation Framework) is a standard proposal developed by ISO
TC37SC4 commitee, see http://www.tc37sc4.org for further details.

=head1 SYNOPSIS

    use Lingua::MAF;

    my $maf = Lingua::MAF->new(
        author   => 'me',
        language => 'english',
        format   => {
            reduced  => 1,
            compact  => 1,
        }
    );

    $maf->add_token(
        id      => 't1',
        content => 'content1',
        pleft   => 0,
        pright  => 6
    );
    $maf->add_word_form(
        source => 0,
        target => 1,
        tokens => [ 't1' ]
        form   => 'form1',
        entry  => 'entry1',
        tag    => 'tag1'
    );
    $maf->add_word_form(
        source => 1,
        target => 2,
        tokens => [ 't1' ]
        form   => 'form2',
        entry  => 'entry2',
        tag    => 'tag2'
    );
    $maf->flush();

=cut

use warnings;
use strict;
use XML::Twig;
use Lingua::Features;
use DateTime;
use Carp;

our $VERSION = '0.1.1';

=head1 Constructor

=head2 Lingua::MAF->new(I<%options>)

Creates and returns a new C<Lingua::MAF> object.
I<%options> is an hash with the following keys:

=over

=item author

The author of the document

=item language

The language of the document

=item format

The format of this document, as an hashref with the following keys:

=over

=item tagset

add the full tagset at the beginning of the document

=item reduced

use reduced fsm format

=item compact

use compact tag format

=item standoff

use standoff format

=item embedded_token

use embedded token format

=back

=back

=cut

sub new {
    my ($class, %options) = @_;

    my $twig = XML::Twig->new(
        pretty_print    => 'indented',
        output_encoding => 'ISO-8859-1',
    );

    my $root = XML::Twig::Elt->new('maf', {
            author     => $options{author},
            date       => DateTime->now()->ymd(),
            language   => $options{language},
            addressing => $options{format}->{standoff} ? 'byte' : 'embedded',
        });

    my @variant;
    push(@variant,'reduced') if $options{format}->{reduced};
    push(@variant,'compact') if $options{format}->{compact};
    push(@variant,'embedded_token') if $options{format}->{embedded_token};
    $root->set_att('variant' => join(' ', @variant)) if @variant;
    $twig->set_root($root);

    if ($options{format}->{tagset}) {
        my $tagset = _tagset2xml();
        $tagset->paste($root);
        $options{format}->{compact} = 1;
    }

    my $self = bless {
        _twig           => $twig,
        _debug          => $options{format}->{debug},
        _reduced        => $options{format}->{reduced},
        _compact        => $options{format}->{compact},
        _standoff       => $options{format}->{standoff},
        _embedded_token => $options{format}->{embedded_token}
    }, $class;

    return $self;
}

=head1 Other methods

=head2 $maf->add_token(I<%token>)

Adds a token to the document.
I<%token> is a hash with the following keys:

=over

=item id

=item content

=item pleft

=item pright

=back

=cut

sub add_token {
    my ($self, %token) = @_;
    croak "not a class method" unless ref $self;

    $self->{_tokens}->{$token{id}} = \%token;
}

=head2 $maf->add_word_form(I<%word_form>)

Adds a word form to the document.
I<%word_form> is a hash with the following keys:

=over

=item source

=item target

=item tokens

=item form

=item entry

=item tag

=item author

=back

=cut

sub add_word_form {
    my ($self, %word_form) = @_;
    croak "not a class method" unless ref $self;

    push(
        @{$self->{_fsm}->{$word_form{source}}->{$word_form{target}}},
        \%word_form
    );
}

=head2 $maf->paste_fsm_twig(I<$fsm>)

Directly paste an fsm twig extracted from another MAF document.

=cut

sub paste_fsm_twig {
    my ($self, $fsm) = @_;
    croak "not a class method" unless ref $self;

    $fsm->cut();
    $fsm->paste('last_child', $self->{_twig}->root());
    $self->_flush($fsm);
}

=head2 $maf->start_fsm_twig()

Start a fsm twig directly

=cut

sub start_fsm_twig {
    my ($self, $token) = @_;
    croak "not a class method" unless ref $self;

    my $current = XML::Twig::Elt->new('fsm');

    $current->paste('last_child', $self->{_twig}->root());
    $self->{_current} = $current;
}

=head2 $maf->paste_token_twig(I<$token>)

Directly paste a token twig extracted from another MAF document.

=cut

sub paste_token_twig {
    my ($self, $token) = @_;
    croak "not a class method" unless ref $self;
    croak "no current fsm" unless $self->{_current};

    $token->cut();
    $token->paste('last_child', $self->{_current});
}

=head2 $maf->paste_transition_twig(I<$token>)

Directly paste a transition twig extracted from another MAF document.

=cut

sub paste_transition_twig {
    my ($self, $transition) = @_;
    croak "not a class method" unless ref $self;
    croak "no current fsm" unless $self->{_current};

    $transition->cut();
    $transition->paste('last_child', $self->{_current});
}

=head2 $maf->stop_fsm_twig()

Stop current fsm twig.

=cut

sub stop_fsm_twig {
    my ($self, $token) = @_;
    croak "not a class method" unless ref $self;
    croak "no current fsm" unless $self->{_current};

    $self->_flush($self->{_current});
    $self->{_current} = undef;
}

=head2 $maf->flush_fsm()

Merge current word forms and related tokens into a finite state automata,
flushed to STDOUT.

=cut

sub flush_fsm {
    my ($self) = @_;
    croak "not a class method" unless ref $self;

    my $root = $self->{_twig}->root();
    my $current = $root;
    my %used_tokens;

    if ($self->{_reduced}) {
        my $in_fsm = 0;
        my @content = ();
        my $fsmstart = 0;
        foreach my $left (sort {$a <=> $b} keys %{$self->{_fsm}}) {
            if ($in_fsm && $left == $in_fsm) {
                $in_fsm = 0;
                $current->set_att('final', $left);
                $current->set_att('init', $fsmstart);
                $current->paste('last_child', $root);
                $current = $root;
            }
            my @rights = sort {$b <=> $a} keys %{$self->{_fsm}->{$left}};
            if (@rights == 1) {
                my $right = $rights[0];
                my @word_forms = (@{$self->{_fsm}->{$left}->{$right}});
                unless ($self->{_embedded_token}) {
                    foreach my $word_form (@word_forms) {
                        foreach my $id (@{$word_form->{tokens}}) {
                            unless ($used_tokens{$id}) {
                                $self->_token2xml($self->{_tokens}->{$id})->paste('last_child', $root);
                                $used_tokens{$id} = 1;
                            }
                        }
                    }
                }
                my $node;
                if (@word_forms > 1) {
                    my $alt = XML::Twig::Elt->new('wfAlt');
                    foreach my $word_form (@word_forms) {
                        $self->_word_form2xml($word_form)->paste('last_child', $alt);
                    }
                    $node = $alt;
                } else {
                    $node = $self->_word_form2xml($word_forms[0]);
                }
                if ($in_fsm) {
                    my $transition = XML::Twig::Elt->new('transition', {
                            source => $left,
                            target => $right
                        });
                    $node->paste('last_child', $transition);
                    $transition->paste('last_child', $current);
                } else {
                    $node->paste('last_child', $current);
                }
            } else {
                if (!$in_fsm) {
                    $fsmstart = $left;
                    $in_fsm = $rights[0];
                    $current = XML::Twig::Elt->new('fsm');
                }
                foreach my $right (@rights) {
                    my @word_forms = (@{$self->{_fsm}->{$left}->{$right}});
                    foreach my $word_form (@word_forms) {
                        unless ($self->{_embedded_token}) {
                            foreach my $id (@{$word_form->{tokens}}) {
                                unless ($used_tokens{$id}) {
                                    $self->_token2xml($self->{_tokens}->{$id})->paste('last_child', $root);
                                    $used_tokens{$id} = 1;
                                }
                            }
                        }
                        $self->_word_form2xml($word_form)->paste('last_child', $current);
                    }
                }
            }
        }
        if ($in_fsm) {
            $current->set_att(final => $in_fsm);
            $current->set_att(init => $fsmstart);
            $current->paste('last_child', $root);
            $current = $root;
            $in_fsm = 0;
        }
    } else {
        $current = XML::Twig::Elt->new('fsm');
        $current->paste('last_child', $root);
        my $min = 1000;
        my $max = 0;
        foreach my $left (sort {$a <=> $b} keys %{$self->{_fsm}}) {
            $min = $left unless ($left > $min);
            foreach my $right (sort {$b <=> $a} keys %{$self->{_fsm}->{$left}}) {
                $max = $right unless ($right < $max);
                my @word_forms = (@{$self->{_fsm}->{$left}->{$right}});
                foreach my $word_form (@word_forms) {
                    unless ($self->{_embedded_token}) {
                        foreach my $id (@{$word_form->{tokens}}) {
                            unless ($used_tokens{$id}) {
                                $self->_token2xml($self->{_tokens}->{$id})->paste('last_child', $current);
                                $used_tokens{$id} = 1;
                            }
                        }
                    }
                    $self->_word_form2xml($word_form)->paste('last_child', $current);
                }
            }
        }
        $current->set_att('init', $min);
        $current->set_att('final', $max);
    }

    # clear variables
    delete $self->{_tokens}->{$_} foreach keys %used_tokens;
    $self->{_fsm} = {};

    # flush until current fsm
    $self->_flush($current);

}

=head2 $maf->flush_document()

Flush current document to STDOUT.

=cut


sub flush_document {
    my ($self) = @_;
    croak "not a class method" unless ref $self;

    # flush all the twig
    $self->_flush();
}

sub _flush {
    my ($self, $elt) = @_;

    # flush twig
    $self->{_twig}->flush($elt);
}

sub _token2xml {
    my ($self, $token) = @_;

    my $xml = XML::Twig::Elt->new('token', {
            id => $token->{id}
        });

    if ($self->{_standoff}) {
        $xml->set_att('from', $token->{pleft});
        $xml->set_att('to', $token->{pright});
    } else {
        $xml->set_content($token->{content});
    }

    return $xml;
}

sub _word_form2xml {
    my ($self, $word_form) = @_;

    my $xml = XML::Twig::Elt->new('wordForm', {
            author => $word_form->{author},
            entry  => $word_form->{entry},
            form   => $word_form->{form}
        });


    if ($self->{_embedded_token}) {
        if (grep { !defined $self->{_tokens}->{$_} } @{$word_form->{tokens}}) {
            $xml->set_att('tokens', join(' ', @{$word_form->{tokens}}));
        }
        $self->_token2xml($self->{_tokens}->{$_})->paste('last_child', $xml) foreach (@{$word_form->{tokens}});
    } else {
        $xml->set_att('tokens', join(' ', @{$word_form->{tokens}}));
    }

    if ($self->{_compact}) {
        $xml->set_att('tag', $word_form->{tag});
    } else {
        my $fs = XML::Twig::Elt->new('fs');

        my %features = Lingua::Features::Structure->from_string(
            $word_form->{tag}
        )->get_features();

        foreach my $key (keys %features) {
            my $f = XML::Twig::Elt->new(
                'f', {
                    name => $key
                }
            );
            my $node;
            my $values = $features{$key};
            next unless $values;

            if (@$values > 1) {
                my $valt = XML::Twig::Elt->new('vAlt');
                $f->paste('last_child', $valt);
                $node = $valt;
            } else {
                $node = $f;
            }

            foreach my $value (@$values) {
                my $symbol = XML::Twig::Elt->new(
                    'symbol', {
                        value => $value
                    }
                );
                $symbol->paste('last_child', $node);
            }
            $node->paste('last_child', $fs);
        }

        $fs->paste('last_child', $xml);
    }

    unless ($self->{_reduced}) {
        my $transition = XML::Twig::Elt->new(
            transition => {
                source => $word_form->{source},
                target => $word_form->{target}
            }
        );
        $xml->paste('last_child', $transition);
        $xml = $transition;
    }

    return $xml;
}

sub _tagset2xml {
    my ($self) = @_;

    my $tagset = XML::Twig::Elt->new('tagset');

    foreach my $type (Lingua::Features::FeatureType->types()) {
        my $id = $type->id();
        my $dcs = XML::Twig::Elt->new(
            dcs => {
                private    => $id,
                registered => "dcs:morphosyntax:fr:$id",
                rel        => "eq"
            }
        );
        $dcs->paste('last_child', $tagset);

        my $vlib =  XML::Twig::Elt->new(
            vLib => {
                name => $id
            }
        );
        $vlib->paste('last_child', $tagset);

        foreach my $value_id ($type->values()) {
            my $value_name = $type->value_name($value_id);
            my $symbol = XML::Twig::Elt->new(
                symbol => {
                    value => $value_name,
                    id    => $value_id
                }
            );
            $symbol->paste('last_child', $vlib);
        }
    }

    return $tagset;
}

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2005-2006, INRIA.

This program is free software; you can redistribute it and/or modify it under the same terms as Perl itself.

=head1 AUTHORS

Guillaume Rousse <grousse@cpan.org>

Eric de la Clergerie, <Eric.De_La_Clergerie@inria.fr>

=cut

1;
