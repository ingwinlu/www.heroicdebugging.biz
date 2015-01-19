Title: Heroic Debugging of Pelican
Date: 2014-05-11 19:08
Tags: heroicdebugging, pelican, simpleMC
Summary: Some heroic debugging and a small update on simpleMC/Things

Spent most of my Sunday looking into a pelican issue and [this](https://github.com/getpelican/pelican/issues/1343) is the result. Lessons learned: Debugging Runtime optimizations is really hard, but really satisfying.

In other news: University and work is keeping me really busy, managed to crank out a small improvement to simpleMC though: End Users who just want to run the program don't get cluttered in DEBUG messages anymore. Argparse is implemented and `--help` and `--debug` are in place.