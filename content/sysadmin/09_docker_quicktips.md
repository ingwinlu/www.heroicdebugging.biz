Title: Docker quick-tips
Date: 2015-08-12 08:48:21
Tags: Server, Docker, nginx, reverse proxy

[Docker] is certainly interesting.
It adds an additional layer between operating system and application which can be used to ease deployment.
So it is somewhere between full blown virtual machines and a simple 'chroot'.
The most memorable description I heard is that 'Docker is just a fancy wrapper for LXC'.

There are some pitfalls however, coupled with a lot of bad docker-images and even worse tutorials (the kind that only says 'do X, then Y') it first turned me off.
However, the magic behind it is quite simple, and if you follow some basic guidelines, it is not hard to gain value from using it.


# Persistent data

First, treat all data inside containers as not persistent.
You will run into a giant headache when you need to upgrade / change an existing container.
To bypass this issue, use [data volumes](https://docs.docker.com/userguide/dockervolumes/) to 'mount' directories from the container to your host.
Now you can remove containers without worrying about your data.


# Systemd integration

We already can apply the persistent data section when we start up our containers via [systemd].
Of course you can just call `docker start` and `docker stop` in your systemd files, but then we are exactly where we don't want to be and experience the difficulties mentioned above if you want to upgrade your containers.
There is always bad after taste if you produce unit files which are not self sufficient (you manually need to docker run a container with a matching name, else docker start will fail).
Additionally all parameters you supply to your run command will get 'lost' that way if you decide to remove the container.

To work around this we stop and remove the old container and run a new one every time we start the unit.

```
[winlu@winlu-main system]$ cat docker_nginx_reverse.service
[Unit]
Description=docker nginx_reverse proxy container
After=docker.service
Requires=docker.service

[Service]
Restart=always
ExecStartPre=-/usr/bin/docker stop -t 2 nginx_reverse
ExecStartPre=-/usr/bin/docker rm nginx_reverse
ExecStart=/usr/bin/docker run --rm --name nginx_reverse -p 80:80 -p 443:443 -e DEFAULT_HOST=heroicdebugging.biz -v /docker/nginx_reverse/certs:/etc/nginx/certs -v /docker/nginx_reverse/vhost.d:/etc/nginx/vhost.d:ro -v /var/run/docker.sock:/tmp/docker.sock jwilder/nginx-proxy

[Install]
WantedBy=multi-user.target
```

The `=-` in the *ExecStartPre* Lines are to not abort if the command fails.

If you want automatic updates, add another *ExecStartPre* line pulling the latest image version.
I can however only recommend this if you are the person providing the update to the image (i.e. the image is under your control in docker hub).


# nginx-proxy

This is what actually got me interested into trying [Docker].
It sets up [nginx] as a reverse proxy.
The differentiating feature here is that you can now add other containers and proxy them via an *environment variable* defining their host name without editing a configuration file manually.


## Show me your moves

So if you want to serve `www.heroicdebugging.biz` via nginx, you would first set-up [nginx-proxy] as described on their github README, then add another nginx docker container with the additional variable `docker run -e VIRTUAL_HOST=www.heroicdebugging.biz -d nginx`.
As long as `www.heroicdebugging.biz` resolves to the docker host IP, it should forward the request to the appropriate container.

This makes it trivially easy to add other subdomains or host other domains on the same host.


## vhost specific configurations

Sometimes you don't want to directly modify the nginx containers you route via [nginx-proxy].
Sometimes you have to add specific configuration to the forwarding part of the reverse proxy configuration.

To handle those cases you should be using the feature provided by [nginx-proxy] to mount a `vhosts.d` directory on your host.
There you can create configuration files with the name of the `VIRTUAL_HOST` which then gets included into the matching server section in the reverse proxy configuration.


### error page handling

To catch errors on the reverse proxy level instead of the actual provider of the error you have to add `proxy_intercept_errors` to the vhost specific configuration.
So to add a custom 404 page located at the root serving directory with the name 404.html, we need to add the following: 
```
error_page 404 /404.html;
proxy_intercept_errors on;
```


### allowing big files

Other uses of the `vhosts.d` directory would be to set `client_max_body_size` to a higher value to allow something like [owncloud] to be reverse proxied as well.


### redirect to other URL

I want heroicdebugging.biz requests to be forwarded to the www sub domain.
To archive this I started another container with `-e VIRTUAL_HOST=heroicdebugging.biz`.
Finally I just need to add the following snippet to my `heroicdebugging.biz` file in the `vhosts.d` directory:
```
return 301 https://www.heroicdebugging.biz$request_uri;
```

[Docker]: https://www.docker.com/
[nginx-proxy]: https://github.com/jwilder/nginx-proxy
[nginx]: http://nginx.org/
[owncloud]: https://owncloud.org/
[systemd]: http://www.freedesktop.org/wiki/Software/systemd/
