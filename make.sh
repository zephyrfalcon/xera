#!/bin/sh

# note: make sure eggs are installed in 'xera' directory

# rm -rf xera
csc -deploy xera.scm -postlude '(main (list))' 
ls -l xera