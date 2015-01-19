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
