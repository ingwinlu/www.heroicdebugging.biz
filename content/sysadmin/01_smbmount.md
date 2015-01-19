Title: Automated Samba Client Script
Date: 2014-02-09 20:01:21
Tags: Server, RPi, Samba, Bash, xbmc
Summary: Presenting a small script that automatically mounts remote Samba Shares as they become available on a home network

#Intro
I use my Pi as mediacenter with xbmc and I wanted a simple way to dynamically mount Samba Shares from my Windows computers when they are up and running. So I quickly fixed up a bash script to run via cron that checks if a server IP is online and mounts/unmounts accordingly.
#Requirements
The script uses **mountpoint** which is provided on Arch by **util-linux**, **udevil** and **cifs-utils** to mount smb shares. Don't forget to set cifs as an allowed type in */etc/udevil/udevil.conf*.

#The Script
Downloadable from [here](|filename|/static/networkMount.sh).

    :::bash
    #!/bin/bash
    #Original Script by winlu <derwinlu AT gmail DOT com> from winlu.mooo.com
    
    
    if [ $# -ne "2" ]; then
        echo Usage: $0 SERVER_IP SHARES
        exit 1
    fi
    
    SERVER_IP=$1
    SHARES=$2
    
    OPTIONS='credentials=/media/.smbcred'
    
    ping -c 1 $SERVER_IP &>/dev/null
    if [ $? -ne 0 ]; then
        # server is down so unmount
        #
        # if we query the mount point and it was previously mounted, the script freezes
        # so just unmount forcing while lazy
        for MNT in $SHARES; do
            udevil unmount -l -f //$SERVER_IP/$MNT &>/dev/null
        done
    else
        # server is up
        #
        # check if mount point is live and try to mount if not
        for MNT in $SHARES; do
            mountpoint -q /media/smb-$SERVER_IP-$MNT || udevil mount -o $OPTIONS smb://$SERVER_IP/$MNT &>/dev/null
        done
    fi
    

#Installation

I use cron entries similar to this one:

    :::bash
    */5 * * * * /media/networkMount.sh 192.168.0.10 "Cloud Music"

This tries to execute the script located at /media/networkMount.sh which then tries to connect to *192.168.0.10* every *5* minutes to mount the shares *Cloud* and *Music*.

The script also uses a credentials file with the following structure:

    :::bash
    > cat /media/.smbcred
    username=smbuser
    password=smbpassword

Be sure to create one yourself and make it only accessable by root with 

    :::bash
    > touch /media/.smbcred
    > chmod 600 /media/.smbcred

