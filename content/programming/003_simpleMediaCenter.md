Title: Introducing Simple Media Center (simpleMC)
Date: 2014-03-26 19:02:06
Tags: python, simpleMC, RPi
Summary: Startup of a new project. Simple Media Center (simpleMC) aims to provide a lightwight alternative to XMBC.

With my last private projects coming to an end I thought of something new that I have need of: a simple Media Center solution.

I used XBMC in the past, but it is just way too buggy, slow and overall overloaded. I want something really, really simple.

Just a directory listing and a player interface on my mobile phone / desktop pc. The whole thing should rely on turbogears / jinja so there is no need to be overly concerned with the whole interface. Since I will design/test/run it on my rpi I want to treat really lightly on resources. Also I want omxplayer support.

Doodled some kind of overview:
![simple MediaCenter Overview UML]({static}/images/simpleMediaServer_Overview.PNG "simpleMediaCenter overview") 

and put up a repository on [github](https://github.com/ingwinlu/simpleMediaCenter). Next step is to define interfaces and simultaneously develop some modules to get a feeling if more abstraction is needed.