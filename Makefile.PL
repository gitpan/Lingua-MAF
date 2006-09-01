# $Id: Makefile.PL,v 1.4 2006/09/01 09:38:46 rousse Exp $

use ExtUtils::MakeMaker;
eval {
    require ExtUtils::MakeMaker::Coverage;
    import ExtUtils::MakeMaker::Coverage;
};

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
