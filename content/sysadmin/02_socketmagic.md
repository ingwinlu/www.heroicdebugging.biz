Title: How to start systemd services on incoming network traffic (XBMC example)
Slug: systemd_socket_magic
Date: 2014-02-13 18:33:00
Tags: Arch, Server, systemd, xbmc
Summary: Short introduction to systemd .socket files and how to use them to start services

#Introduction

Systemd is a system and service manager which provides a neat and easy way to organize and start your services on Linux. Only recently I found out you can use [.socket][systemd_socket_man] files to start services on incoming network traffic. 

#Usage with XBMC and XBMC Remote
For example you can use a .socket file to conveniently start xbmc when you connect via the [XBMC Remote App][xbmc_remote]. See the following .socket file:

    :::bash
    #/etc/systemd/system/xbmcnet.socket
    #You might have to use the Service= and Accept=no options if the service name differs from the socket name
    [Unit]
    Conflicts=xbmcnet.service
    
    [Socket]
    ListenStream=8080
    #This is the tcp port we listen to traffic to, set to the port you have set in xbmc->settings->network http control port
    
    [Install]
    WantedBy=sockets.target

And the matching .service file:

    :::bash
    #/etc/systemd/system/xbmcnet.service
    #See comments for the xbmcnet.socket file above!
    [Unit]
    Description=Launch XBMC on main display - oneshot
    
    [Service]
    PermissionsStartOnly=true
    User = xbmc
    Group = xbmc
    Type=oneshot
    Nice=-1
    ExecStart = /usr/bin/xbmc-standalone -l /run/lirc/lircd
    ExecStartPost = /usr/bin/bash -c "sleep 10 && systemctl start xbmcnet.socket"
    #The sleep is needed because sometimes you are too slow with exiting the xbmc remote app and the traffic might start the service again immediately
    
    [Install]
    WantedBy=multi-user.target

Now you only need to start/enable the socket file and everything should be good to go!

    :::bash
    systemctl enable xbmcnet.socket
    systemctl start xbmcnet.socket
    

[systemd_socket_man]: http://www.freedesktop.org/software/systemd/man/systemd.socket.html "systemd socket man page"
[xbmc_remote]: https://play.google.com/store/apps/details?id=org.xbmc.android.remote "xbmc remote app"
