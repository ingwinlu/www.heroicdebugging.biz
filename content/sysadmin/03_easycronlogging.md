Title: Date prefixed log in bash
Tags: Arch, Server, cron, bash
Date: 2014-02-22 23:13:00
Summary: How to easily get date prefixed log output in bash for use with crond

#Script

Testing out [kuhbox](/tag/kuhbox.html) showed, that I needed to add date information to the output to determine possible bottlenecks. Instead of adding the feature to the .py script, I made a small bash script which adds the information to everything that it reads from stdin. This way the functionality is available for all my cron jobs and does not clog up the output when not run from cron.

    :::bash
    #!/bin/bash
    #timestomper.sh
    #
    while read x; do
        echo "`date +%d/%m/%Y\ %H:%M:%S`: $x";
    done
    


So what is the magic behind it? First a *while* loop, which is executed as long as *read* does not fail (no EOF or timeout). The *read* gets the whole line and assigns it to the variable *x*. Then there is an *echo* statement which first computes *date* with the format (see [here](http://unixhelp.ed.ac.uk/CGI/man-cgi?date) for more information) we want for our output (Note the \`command\`, which is used for [command substitution](http://tldp.org/LDP/abs/html/commandsub.html)) and after that the line we just read from stdin.

#Usage

    :::bash
    [winlu@micronuke pelican]$ crontab -l
    0 3 * * * cd /home/winlu/python/kuhbox && ./kuhbox.py /home/winlu/pelican | /home/winlu/bash/timestomper.sh &> ./kuhbox_`date +\%Y-\%m-\%d`.log

This is the cron entry I use for backing up my blog subdirectory with [kuhbox](/tag/kuhbox.html). It first changes to the working directory with cd, then calls the script and the directory to backup. The output then gets piped with | towards the stdin from the timestomper.sh script. Stdout and Stderr are then both redirected towards a logfile which name changes every day. The finished result looks then like this:

    :::bash
    23/02/2014 03:04:33: put small file /home/winlu/pelican/content/pi/00_installarchbase.md with size: 6950
    23/02/2014 03:04:33: put small file /home/winlu/pelican/content/pi/01_smbmount.md with size: 2228
    23/02/2014 03:04:33: put small file /home/winlu/pelican/content/misc/00_kickoff.md with size: 1032
    23/02/2014 03:04:33: put small file /home/winlu/pelican/content/misc/01_layout_change.md with size: 1184
    23/02/2014 03:04:33: Done
    
