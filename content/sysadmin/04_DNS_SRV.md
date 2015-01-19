Title: Adding SRV Records at http://freedns.afraid.org/
Tags: DNS, SRV, freedns, afraid.org, server
Date: 2014-03-05 17:24:54
Summary: Even though it is not really documented you can add SRV entries to your http://freedns.afraid.org/ account

Even though it is not really documented you can add [SRV](http://en.wikipedia.org/wiki/SRV_record) entries to your [http://freedns.afraid.org/](http://freedns.afraid.org/) account. To do so open up your [subdomain](http://freedns.afraid.org/subdomain/) page and press [Add](http://freedns.afraid.org/subdomain/edit.php). 

![Edit Dialog]({filename}/images/dyndns_SRV_edit.PNG)

A SRV record generally has this form[^1]: 

    :::no-highlight
    _service._proto.name. TTL class SRV priority weight port target.
    
    
*   **_service._proto.name.** is the part that is being queried in a DNS request. We put it into the *Subdomain* field.
*   For the *Destination* field we want **priority weight port target**
    *   *priority* is to determine the order in which same-named entries are being processed by a client (highest first, when unavailable fall back to next lower...)
    *   *weight* is used when two entries have the same priority
    *   *port* should be self explanatory
    *   *target* as well

Don't forget to set SRV as Type and press **Save!** and you should be good to go.  
    
    
    
[^1]: Source: [wikipedia.org](http://en.wikipedia.org/wiki/SRV_record#Record_format).