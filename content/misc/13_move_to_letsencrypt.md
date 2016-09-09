Title: Moving to Let's Encrypt
Date: 2016-09-01 08:00:00
Modified: 2016-09-01 09:00:00 
Tags: organisational, lets encrypt, ssl


I just finished incorporating letsencrypt into my hosting setup.
A mostly painless process that probably took me less time then renewing my old certificate.

As mentioned in my [docker quicktips article]({filename}/sysadmin/09_docker_quicktips.md) I use a mix of docker containers to serve my web content.
To move to [letsencrypt][letsencrypt] i simply needed to add another container in the mix.
[Letsencrypt-companion][letsencrypt-companion] takes care of acquiring all needed certificates that are proxied via [nginx-proxy][nginx-proxy].
A few edits to my systemd services that take care of the docker container management later everything looks fine and is running smoothly (so far!).

Overall a good experience and it feels good to cross the 'RENEW SSL CERTS' entry in my calendar off.


[letsencrypt]: https://letsencrypt.org/
[letsencrypt-companion]: https://github.com/JrCs/docker-letsencrypt-nginx-proxy-companion "Docker Let's Encrypt nginx proxy companion"
[nginx-proxy]: https://github.com/jwilder/nginx-proxy
