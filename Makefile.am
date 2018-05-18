## This is a automake file, part of the GPTL package.
# Copyright 2018, see the COPYRIGHT file for more information.

# This is the main automake file for GPTL.
# Ed Hartnett, 5/18/18

# This directory stores libtool macros, put there by aclocal.
ACLOCAL_AMFLAGS = -I m4

# These files get added to the distribution.
EXTRA_DIST = COPYING INSTALL README

# This is the list of subdirs for which Makefiles will be constructed
# and run.
SUBDIRS = include src

install-data-hook:

	@echo ''
	@echo '+-------------------------------------------------------------+'
	@echo '| Congratulations! You have successfully installed GPTL!      |'
	@echo '|                                                             |'
	@echo '| CAUTION:                                                    |'
	@echo '|                                                             |'
	@echo '| If you have not already run "make check", then we strongly  |'
	@echo '| recommend you do so. It does not take very long.            |'
	@echo '|                                                             |'
	@echo '| GPTL is developed and maintained at NOAA.                   |'
	@echo '+-------------------------------------------------------------+'
	@echo ''