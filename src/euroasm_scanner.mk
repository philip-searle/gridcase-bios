perl_begin
{
	use strict;
	package Mpp::Scanner::Euroasm;
	use Mpp::Scanner::C;
	our @ISA = qw/Mpp::Scanner::C/;
	sub get_directive {
		s/^\s*(include)1?\s*//i ? $1 : undef;
	}
	sub factory {
		Mpp::Scanner::Euroasm->new(@_);
	}
	1;

	package Mpp::CommandParser::Euroasm;
	use Mpp::CommandParser;
	our @ISA = qw/Mpp::CommandParser/;
	use Mpp::File;
	sub factory {
		shift;
		Mpp::CommandParser::Euroasm->new(@_);
	}
	sub new {
		my $self = &Mpp::CommandParser::new;
		$self->{SCANNER} = new Mpp::Scanner::Euroasm($self->rule, $self->dir);
		$self;
	}
	sub xparse_command {
		my ($self, $command, $setenv) = @_;
		my @files;
		my ($binary, @words) = @$command;
		local $_;
		while (defined($_ = shift @words)) {
			push @files, $_;
		}

		my $scanner = $self->{SCANNER};
		my $context = $scanner->get_context;
		$scanner->{LANGUAGE} = 'euroasm';
		$scanner->add_include_dir(user => '.');
		$scanner->reset($context);
		foreach my $file (@files) {
			$scanner->reset($context);
			my $xxx = $scanner->scan_file($self, 'user', $file) or return undef;
		}
		return 1;
	}
	1;
}
perl_end

register-parser euroasm.exe Mpp::CommandParser::Euroasm
