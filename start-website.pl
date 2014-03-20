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

chdir "/home/borch/lisp/cl/source/website";

exit system("/home/borch/.local/bin/ccl",
            "--eval", <<'HERE');
  (progn
    (ql:quickload :website)
    (funcall (read-from-string "website:start-website"))
    (ql:quickload :swank)
    (funcall (read-from-string "swank-loader:init"))
    (funcall (read-from-string "swank:create-server") :port 50001 :dont-close t))
HERE
;