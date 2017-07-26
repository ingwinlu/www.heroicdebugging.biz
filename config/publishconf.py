#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

import os
import sys
sys.path.append(os.path.join(os.curdir, "config"))
from pelicanconf import *


# URL settings for publish
SITEURL = 'https://www.heroicdebugging.biz'
RELATIVE_URLS = False
FEED_ALL_ATOM = 'feeds/all.atom.xml'
