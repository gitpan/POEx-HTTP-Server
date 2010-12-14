use 5.008008;
use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    NAME              => 'POEx::HTTP::Server',
    VERSION_FROM      => 'lib/POEx/HTTP/Server.pm', # finds $VERSION
    PREREQ_PM         => {
            'POE'           => 1,
            'POEx::URI'     => 0.03,
            'POE::Session::Multiplex' => 0.04,
            'POE::Session::PlainCall' => 0.02,
            'Data::Dump'    => 1,
            'Scalar::Util'  => 0,
            'Storable'      => 1,
            'HTTP::Status' => 1,
            'HTTP::Request' => 1,
            'HTTP::Response' => 1,
            'HTTP::Date' => 1
		},
    ($] >= 5.005 ?     ## Add these new keywords supported since 5.005
      (ABSTRACT_FROM  => 'lib/POEx/HTTP/Server.pm', # retrieve abstract from module
       AUTHOR         => 'Philip Gwyn <fil@localdomain>') : ()),
    #LIBS              => [''], # e.g., '-lm'
    #DEFINE            => '', # e.g., '-DHAVE_SOMETHING'
    #INC               => '-I.', # e.g., '-I. -I/usr/include/other'
	# Un-comment this if you add C files to link with later:
    # OBJECT            => '$(O_FILES)', # link all the C files too
);
