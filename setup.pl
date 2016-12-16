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
  if (-d $path) {
    say "$path already exists";
  } else {
    mkdir $path or die "mkdir $path: $!";
    say "make path: $path";
  }
}
sub step_symlink {
  my ($target, $link) = @_;
  if (-f $link) {
    say "$link already exists";
  } else {
    symlink $target, $link or die "symlink $target, $link: $!";
    say "symlink: $link -> $target";
  }
}
sub step_make_dot_emacs {
  my $path = "$homedir/.emacs";
  die "$path already exists!" if -f $path;
  open my $f, '>', $path or die "open $path: $!";
  print {$f} $dot_emacs_contents;
  say "create file $path:";
  print $dot_emacs_contents =~ s/^/ > /mgr;
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
