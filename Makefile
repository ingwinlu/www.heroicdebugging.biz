PELICAN_VERSION?=3.7.1
BOOTSTRAPIFY_VERSION?=1.0.0
SITEMAP_VERSION?=bb1aa5ad03ecdcc2f81b7e6a76ac1e592fcaba17
TOC_VERSION=1.2.0
TWITCHY_VERSION=4bb0fc53f269fb4ce92838cd2a10d2aff9526624

BASEDIR=$(CURDIR)
INPUTDIR=$(BASEDIR)/content
CONFDIR=$(BASEDIR)/config

SSH_PORT=22
SSH_USER=winlu
SSH_HOST=heroicdebugging.biz
SSH_TARGET_DIR=/docker/nginx_www

DOCKER_IMAGE_NAME=heroicdebugging
DOCKER_IMAGE_TAG=latest
DOCKER_IMAGE_ID=$(DOCKER_IMAGE_NAME):$(DOCKER_IMAGE_TAG)
DOCKER_CONTAINER_NAME=heroicdebugging_builder
DOCKER_CONTAINER_VOLUME=heroicdebugging_builder_data

DOCKER_RSYNC_IMAGE=netroby/alpine-rsync@sha256:e9dc90ffc522fdbd245efd2d3e1ae878888d582e8b2310fe8b3a7a91ff740459

PREVIEW_PORT=8080

V ?= 0

verbose_0 = @
verbose_2 = set -x;
verbose = $(verbose_$(V))


help:
	@echo 'Makefile for a pelican Web site (in container)                         '
	@echo '                                                                       '
	@echo 'Usage:                                                                 '
	@echo '   make image                       build image for building site      '
	@echo '   make dev                         build site with dev settings       '
	@echo '   make preview                     preview local dev build of site    '
	@echo '   make html                        build site with production settings'
	@echo '   make publish                     upload production version via rsync'
	@echo '   make clean                       clean up container and image       '
	@echo '   make clean-container             removes container holding site     '
	@echo '   make clean-image                 removes builder image              '
	@echo '                                                                       '

volume: clean-volume
	$(verbose) docker volume create $(DOCKER_CONTAINER_VOLUME)

image:
	$(verbose) docker build --build-arg PELICAN_VERSION=$(PELICAN_VERSION) --build-arg BOOTSTRAPIFY_VERSION=$(BOOTSTRAPIFY_VERSION) --build-arg SITEMAP_VERSION=${SITEMAP_VERSION} --build-arg TOC_VERSION=${TOC_VERSION} --build-arg TWITCHY_VERSION=${TWITCHY_VERSION} -t $(DOCKER_IMAGE_ID) .


DOCKER_RUN=docker run \
	   --rm \
	   -v $(INPUTDIR):/usr/src/pelican/content \
	   -v $(CONFDIR):/usr/src/pelican/config \
	   -v $(DOCKER_CONTAINER_VOLUME):/usr/share/nginx/html \
	   --name $(DOCKER_CONTAINER_NAME) \
	   $(DOCKER_IMAGE_ID)

prerun: clean-container clean-volume image volume


dev: prerun
	$(verbose) $(DOCKER_RUN) -s ./config/pelicanconf.py

preview: do-preview cleanup-preview

do-preview: dev
	$(verbose) echo "Visit http://localhost:$(PREVIEW_PORT)/"
	-$(verbose) docker run -i -t --rm -p $(PREVIEW_PORT):80 -v $(DOCKER_CONTAINER_VOLUME):/usr/share/nginx/html nginx:alpine

cleanup-preview: do-preview
	-$(verbose) docker volume rm $(DOCKER_CONTAINER_VOLUME)


html: prerun
	$(verbose) $(DOCKER_RUN) -s ./config/publishconf.py

publish: do-publish cleanup-publish

do-publish: html
	-$(verbose) docker run --rm -i -t \
		-v $(DOCKER_CONTAINER_VOLUME):/usr/share/nginx/html \
		-v $(HOME)/.ssh/:/root/.ssh/:ro  \
		$(DOCKER_RSYNC_IMAGE) \
		rsync -e "ssh -p $(SSH_PORT)" -P -rvzc --delete \
		/usr/share/nginx/html/ $(SSH_USER)@$(SSH_HOST):$(SSH_TARGET_DIR) \
		--cvs-exclude

cleanup-publish: do-publish
	-$(verbose) docker volume rm $(DOCKER_CONTAINER_VOLUME)

dist-clean: clean clean-image

clean: clean-container clean-volume

clean-container:
	-$(verbose) docker rm $(DOCKER_CONTAINER_NAME)

clean-image:
	-$(verbose) docker rmi $(DOCKER_IMAGE_ID)

clean-volume:
	-$(verbose) docker volume rm $(DOCKER_CONTAINER_VOLUME) &> /dev/null

.PHONY: help volume image dev preview publish clean clean-container clean-image clean-volume
