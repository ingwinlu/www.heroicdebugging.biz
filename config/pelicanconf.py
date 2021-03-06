#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

AUTHOR = 'winlu'
SITENAME = 'Heroic Debugging'
SITEURL = ''
RELATIVE_URLS = True
SITESUBTITLE = 'A Blog about Diminishing Returns of Heroic Debugging and <a href="/pages/about.html">more...</a>'

PATH = '../content'

TIMEZONE = 'Europe/Paris'

DEFAULT_LANG = 'en'

# Feed generation
FEED_ALL_ATOM = None
AUTHOR_FEED_ATOM = None
AUTHOR_FEED_RSS = None
CATEGORY_FEED_ATOM = None
TRANSLATION_FEED_ATOM = None

#static
STATIC_PATHS = [
    'images',
    'static',
    'extra/sitelogo.png',
    'extra/favicon.ico',
    'extra/favicon.png',
    'extra/robots.txt',
]
EXTRA_PATH_METADATA = {
                        'extra/sitelogo.png': {'path': 'sitelogo.png'},
                        'extra/robots.txt': {'path': 'robots.txt'},
                        'extra/favicon.ico': {'path': 'favicon.ico'},
                        'extra/favicon.png': {'path': 'favicon.png'},
                      }

#Pagination
#DEFAULT_PAGINATION = 3

#theme
THEME = "themes/pelican-twitchy"
BOOTSTRAP_THEME = 'sandstone'
PYGMENTS_STYLE = 'colorful'
#PYGMENTS_STYLE = 'emacs'

#paths
#PAGE_SAVE_AS = ''
AUTHOR_SAVE_AS = ''
AUTHORS_SAVE_AS = ''
ARTICLE_URL = '{date:%Y}/{date:%m}/{date:%d}/{slug}/'
ARTICLE_SAVE_AS = '{date:%Y}/{date:%m}/{date:%d}/{slug}/index.html'

#plugins
PLUGIN_PATHS = [
        '../plugins',
    ]
PLUGINS = [
        'pelican-bootstrapify',
        'pelican-sitemap',
        'pelican-toc',
    ]

#sitemap settings
SITEMAP = {
    'format': 'xml',
    'priorities': {
        'articles': 0.5,
        'indexes': 0.6,
        'pages': 0.3
    },
    'changefreqs': {
        'articles': 'monthly',
        'indexes': 'daily',
        'pages': 'monthly'
    }
}

# TOC = {
#     'TOC_INCLUDE_TITLE': 'false'
# }

# BOOTSTRAPIFY = {
#         'table': ['table', 'table-striped']
# }

#dateformat
DEFAULT_DATE_FORMAT = '%Y-%m-%d'
DEFAULT_DATE = 'fs'

# Social widget
SOCIAL = (
            ('Twitter', 'https://twitter.com/derwinlu'),
            ('Bitbucket', 'https://bitbucket.org/winlu'),
            ('GitHub', 'https://github.com/ingwinlu'),
          # ('Google+', 'https://plus.google.com/115771807029208924055'),
          # ('RSS', SITEURL + '/' + FEED_ALL_ATOM),
            ('EMAIL', 'mailto:derwinlu@gmail.com'),
          )

# Share
SHARE = False  # seems broken

#disqus
# DISQUS_SITENAME = 'winlu'
# DISQUS_LOAD_LATER = True

#typography
TYPOGRIFY = True

#sitelogo
SITELOGO = 'sitelogo.png'
SITELOGO_SIZE = '200'
HIDE_SITENAME = True

#menu
DISPLAY_RECENT_POSTS_ON_MENU = True
DISPLAY_CATEGORIES_ON_MENU = True
DISPLAY_PAGES_ON_MENU = True
DISPLAY_TAGS_ON_MENU = False
EXPAND_LATEST_ON_INDEX = True

#tag cloud
#TAG_CLOUD_STEPS = 3
#TAG_CLOUD_MAX_ITEMS = 20

#Cookie Consent
COOKIE_CONSENT = False

#license
CC_LICENSE = "CC-BY-NC-SA"
CC_ATTR_MARKUP = True

#Markdown
#MD_EXTENSIONS = ['codehilite(css_class=highlight)','extra']

#Cache
CACHE_CONTENT = False

#Open Graph
OPEN_GRAPH = True
OPEN_GRAPH_IMAGE = "favicon.png"
