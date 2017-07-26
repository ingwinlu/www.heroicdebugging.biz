FROM python:2-alpine

ARG PELICAN_VERSION
ARG BOOTSTRAPIFY_VERSION
ARG SITEMAP_VERSION
ARG TOC_VERSION
ARG TWITCHY_VERSION


RUN apk add --no-cache --virtual build-dependencies curl ca-certificates libarchive-tools
RUN pip install --no-cache-dir Markdown beautifulsoup4 typogrify pelican==$PELICAN_VERSION

WORKDIR /usr/src/pelican
RUN mkdir /usr/src/pelican/plugins

WORKDIR /usr/src/pelican/plugins

RUN mkdir bootstrapify && \
    curl -L https://github.com/ingwinlu/pelican-bootstrapify/archive/$BOOTSTRAPIFY_VERSION.zip | bsdtar -xf- -C bootstrapify --strip 1
RUN mkdir sitemap && \
    curl -L https://github.com/ingwinlu/pelican-sitemap/archive/$SITEMAP_VERSION.zip | bsdtar -xf- -C sitemap --strip 1
RUN mkdir toc && \
    curl -L https://github.com/ingwinlu/pelican-toc/archive/$TOC_VERSION.zip | bsdtar -xf- -C toc --strip 1


WORKDIR /usr/src/pelican
RUN mkdir pelican-twitchy && \
    curl -L https://github.com/ingwinlu/pelican-twitchy/archive/$TWITCHY_VERSION.zip | bsdtar -xf- -C pelican-twitchy --strip 1

VOLUME /usr/src/pelican/content
VOLUME /usr/src/pelican/config
VOLUME /usr/share/nginx/html

ENTRYPOINT ["pelican", "-D", "content", "-o", "/usr/share/nginx/html"]
CMD ["-s", "config/pelicanconf.py"]
