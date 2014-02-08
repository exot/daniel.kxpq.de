#!/usr/bin/perl
#
# Automatically start website on daniel.kxpq.de if not running yet
#

use 5.014_000;
use utf8;
use warnings;
use strict;

use LWP::UserAgent;
exit 0
  if LWP::UserAgent->new->get("http://daniel.kxpq.de")->code != 500;

exit system("/usr/bin/emacs",
            "--eval", <<HERE);
(require 'slime)
(slime-start :program "ccl"
             :directory (expand-file-name "~/lisp/cl/source/website/")
             :program-args '("-e" "(progn
                                     (ql:quickload :website)
                                     (funcall (read-from-string \"website:start-website\")))"))
HERE
;
