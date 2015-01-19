Title: Simple Media Center (simpleMC) V0.1 release
Date: 2014-03-28 17:12:45
Tags: python, RPi, simpleMC, xbmc
Summary: First working release of Simple Media Center (simpleMC)

While most of the planned features are still only available as draft, Version 0.1 already can already start and stop playback with omxplayer via a jinja/turbogear generated Interface.

Player allocation is probably going to move into the interface, seeing as it would be nice to change from a file based player towards for example youtube playback. Hence that site generation will probably also move from a filebrowser to a more generic abstraction.

This is also what is currently needed most, a working filecrawler with the playlist function attached. With that also comes the design choice to stay single threaded or move towards multi threading. So far I have tried to keep it simple but busy waiting can only get you so far ;).

Problems came mostly from lacking turbogear documentation when it is used in minimal mode (i.e. not a generated project). Had to dig through the source code to find out how to set the template directory manually.

Check out the [source code here](https://github.com/ingwinlu/simpleMediaCenter) and if you want to contribute to the project don't hesitate to contact me!