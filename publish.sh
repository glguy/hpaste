#!/usr/bin/env sh

runghc Setup build && sudo /sbin/service httpd restart && sudo cp dist/build/hpaste.fcgi/hpaste.fcgi /var/www/cgi-bin && sudo chcon -t httpd_unconfined_script_exec_t /var/www/cgi-bin/hpaste.fcgi
