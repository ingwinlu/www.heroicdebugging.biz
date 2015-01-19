Title: Dynamic DNS and namecheap.com
Tags: DNS, dynamic, namecheap, bash
Date: 2014-07-10 16:59:04
Summary: Some Commandline Fu to update your namecheap dns via wget

After digging through the help section on [namecheap][namecheap] I made some entries into my crontab to update dns entries automatically:

    :::bash
    */5 * * * * sleep 25 ; wget -O - "https://dynamicdns.park-your-domain.com/update?host=www&domain=heroicdebugging.biz&password=YOURPASSWORD"
    */5 * * * * sleep 25 ; wget -O - "https://dynamicdns.park-your-domain.com/update?host=*&domain=heroicdebugging.biz&password=YOURPASSWORD"
    
*   *host*: which entry you want to update, supports wildcards
*   *domain*: the domain you want to modify
*   *password*: the password you receive after you activate dynamic dns for your domain

[namecheap]: http://namecheap.com "namecheap"