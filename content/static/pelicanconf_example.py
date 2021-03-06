#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

AUTHOR = 'winlu'
SITENAME = 'Winlus Blog'
SITEURL = 'https://www.heroicdebugging.biz'
SITESUBTITLE = 'A Blog about Diminishing Returns of Heroic Debugging and <a href="/pages/about.html">more...</a>'

PATH = 'content'

TIMEZONE = 'Europe/Paris'

DEFAULT_LANG = 'en'

# Feed generation
FEED_ALL_ATOM = 'feeds/all.atom.xml'
AUTHOR_FEED_ATOM = None
AUTHOR_FEED_RSS = None
CATEGORY_FEED_ATOM = None
TRANSLATION_FEED_ATOM = None

#static
STATIC_PATHS = ['images',
                'static',
                'extra/sitelogo.png',
                'extra/favicon.ico',
                'extra/robots.txt',
                'extra/google331456191962689c.html',
                'extra/CNAME']
EXTRA_PATH_METADATA = {
                        'extra/sitelogo.png': {'path': 'sitelogo.png'},
                        'extra/robots.txt': {'path': 'robots.txt'},
                        'extra/favicon.ico': {'path': 'favicon.ico'},
                        'extra/google331456191962689c': {'path': 'google331456191962689c.html'},
                        'extra/CNAME': {'path': 'CNAME'},
                      }

#Pagination
#DEFAULT_PAGINATION = 3

#theme
THEME = "/home/winlu/gitrepos/pelican_my_themes/pelican-twitchy"
BOOTSTRAP_THEME = 'sandstone'
PYGMENTS_STYLE = 'colorful'

#paths
#PAGE_SAVE_AS = ''
ARTICLE_URL = '{date:%Y}/{date:%m}/{date:%d}/{slug}/'
ARTICLE_SAVE_AS = '{date:%Y}/{date:%m}/{date:%d}/{slug}/index.html'

#plugins
PLUGIN_PATHS = ['/home/winlu/gitrepos/pelican-plugins', '/home/winlu/gitrepos/pelican_my_plugins']
#PLUGINS = ['render_math' , 'sort_tags' , 'bootstrapify']
PLUGINS = ['render_math' , 'sort_tags' , 'bootstrapify', 'toc']

#dateformat
DEFAULT_DATE_FORMAT = '%Y-%m-%d'
DEFAULT_DATE = 'fs'

# Blogroll
LINKS =  (('barely.sexy Blog', 'https://barely.sexy/'),
          ('Archlinux', 'https://www.archlinux.org/'),
          ('FreeDNS', 'http://freedns.afraid.org/'),
          ('Rasperry Pi', 'http://www.raspberrypi.org/'),
         )

# Social widget
SOCIAL = (
            ('Twitter', 'https://twitter.com/derwinlu'),
            ('Bitbucket', 'https://bitbucket.org/winlu'),
            ('GitHub', 'https://github.com/ingwinlu'),
            ('Google+', 'https://plus.google.com/115771807029208924055'),
            ('RSS', SITEURL + '/' + FEED_ALL_ATOM),
            ('EMAIL', 'mailto:derwinlu@gmail.com'),
          )

# Share
SHARE = True

#disqus
DISQUS_SITENAME = 'winlu'
DISQUS_LOAD_LATER = True

#typography
TYPOGRIFY = True

#sitelogo
SITELOGO = '/sitelogo.png'
SITELOGO_SIZE = '200'
HIDE_SITENAME = True

#menu
DISPLAY_RECENT_POSTS_ON_MENU = True
DISPLAY_CATEGORIES_ON_MENU = True
DISPLAY_PAGES_ON_MENU = True
DISPLAY_TAGS_ON_MENU = False
EXPAND_LATEST_ON_INDEX = True

#tag cloud
TAG_CLOUD_STEPS = 3
TAG_CLOUD_MAX_ITEMS = 20

#Google Analytics
GOOGLE_ANALYTICS='UA-8040053-2'

#Markdown
#MD_EXTENSIONS = ['codehilite(css_class=highlight)','extra']
