Title: Simple Media Center (simpleMC) V0.6 release
Date: 2014-05-03 21:28:54
Tags: python, RPi, simpleMC, xbmc
Summary: features...features everywhere

Earlier tonight I pushed v0.6 of *simple Media Center* upstream. Feels like a long time ago when I implemented auto selection of players (1 month if you believe github). Most notable features in this release:

* completly redid distribution setup for much easier installation, distribution, updating
* rewrote main file to be a class, more flexibility and nicer capsulation
* redid the browser navigation, much more readable and easier to extend
* added Youtube browser
* added basic search interface, implemented Youtube search
* added exception handler which handles exception display in the web interface, also improved exception handling 
* improved logger messages
* volume control and redesigned player buttons
* added mplayer support for non raspberry pi playback

As a small forecast, I am probably going to rework the webinterface for v0.7 and make the functions threadable. This would allow for loading feedback to be displayed which is useful since youtube and twitch browsing tend to take some seconds.

Check out the [source code here](https://github.com/ingwinlu/simpleMediaCenter), for more information and if you want to contribute to the project don't hesitate to contact me! (**NEED TESTERS=)**)
