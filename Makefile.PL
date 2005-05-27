# $Id: Makefile.PL,v 1.2 2005/05/02 16:04:21 rousse Exp $

use ExtUtils::MakeMaker;
use ExtUtils::MakeMaker::Coverage;

WriteMakefile(
    NAME          => 'Lingua::MAF',
    VERSION_FROM  => 'lib/Lingua/MAF.pm',
    ABSTRACT_FROM => 'lib/Lingua/MAF.pm',
    AUTHOR        => 'Guillaume Rousse <grousse@cpan.org>',
    PREREQ_PM     => {
	'XML::Twig'        => 0,
	'Lingua::Features' => 0,
	'DateTime'         => 0,
	'Test::More'       => 0
    },
    dist           => {
	COMPRESS => 'gzip -9f',
	SUFFIX   => 'gz',
    },
    clean          => {
	FILES => 'Lingua-MAF-*'
    },
);
