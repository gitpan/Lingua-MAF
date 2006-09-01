#!/usr/bin/perl
# $Id: 00distribution.t,v 1.2 2006/09/01 09:41:03 rousse Exp $

use Test::More;

BEGIN {
    eval {
        require Test::Distribution;
    };
    if($@) {
        plan skip_all => 'Test::Distribution not installed';
    } else {
        import Test::Distribution not => 'versions';
    }
}
