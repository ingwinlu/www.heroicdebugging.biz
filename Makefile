PY?=python
PELICAN?=pelican
PELICANOPTS=

BASEDIR=$(CURDIR)
INPUTDIR=$(BASEDIR)/content
OUTPUTDIR=/docker/nginx_www
OUTPUTDIRDRAFT=/docker/nginx_draft
CONFFILE=$(BASEDIR)/pelicanconf.py


help:
	@echo 'Makefile for a pelican Web site                                        '
	@echo '                                                                       '
	@echo 'Usage:                                                                 '
	@echo '   make draft                       provide a draft into draft dir     '
	@echo '   make publish                     (re)generate the web site          '
	@echo '   make clean                       remove the generated files         '
	@echo '                                                                       '
	@echo 'Set the DEBUG variable to 1 to enable debugging, e.g. make DEBUG=1 html'
	@echo '                                                                       '

draft:
	$(PELICAN) $(INPUTDIR) -o $(OUTPUTDIRDRAFT) -s $(CONFFILE) -D

publish:
	$(PELICAN) $(INPUTDIR) -o $(OUTPUTDIR) -s $(CONFFILE)

clean:
	[ ! -d $(OUTPUTDIR) ] || rm -rf $(OUTPUTDIR)

.PHONY: help draft publish clean
