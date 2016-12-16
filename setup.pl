#!/usr/bin/env perl
use v5.16;
use warnings;
no warnings 'uninitialized';

use Cwd qw(getcwd abs_path);
use File::Basename qw(dirname);
use File::Spec::Functions qw(canonpath catdir);
use File::Path qw(make_path);
use Getopt::Long;

my $dot_emacs_contents = qq{
(add-to-list 'load-path "~/.emacs.d/lisp")
(load-library "init-common")
(load-library "init-putty")
} =~ s/^  //mgr =~ s/^\s+//gr;

GetOptions(
    'homedir=s' => \(my $homedir = $ENV{HOME}),
    'repodir=s' => \(my $repodir = dirname abs_path __FILE__),
);
$_ = abs_path $_ for ($homedir, $repodir);


sub step_mkdir {
  my ($path) = @_;
  say "make path: $path";
  mkdir $path or die $!;
}
sub step_symlink {
  my ($target, $link) = @_;
  say "symlink: $link -> $target";
  symlink $target, $link or die $!;
}
sub step_make_dot_emacs {
  my $path = "$homedir/.emacs";
  die "$path already exists!" if -f $path;
  say "create file $path:";
  print $dot_emacs_contents =~ s/^/ > /mgr;
  open my $f, '>', $path or die $!;
  print {$f} $dot_emacs_contents;
}

say "";
say "Installing home files from $repodir to $homedir.";
say "";
step_mkdir "$homedir/.emacs.d";
step_mkdir "$homedir/.emacs.d/autosaves";
step_mkdir "$homedir/.emacs.d/backups";
step_symlink "$repodir/emacs-lisp", "$homedir/.emacs.d/lisp";
say "";
step_make_dot_emacs;
say "";
step_symlink "$repodir/.gitconfig", "$homedir/.gitconfig";
step_symlink "$repodir/.tmux.conf", "$homedir/.tmux.conf";
say "";
