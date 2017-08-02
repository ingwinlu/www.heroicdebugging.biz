Title: Docker quicktips #2
Date: 2017-07-22 12:48:22
Tags: Server, Docker
Summary: Heyyy just another small set of tricks I picked up over time when working with [Docker].


Heyyy just another small set of tricks I picked up over time when working
with [Docker].

# Pull new versions of existing images / tags
```
docker images | awk 'BEGIN{OFS=":"} {print $1,$2}' | grep -v 'none' | grep -iv 'repo' | xargs -n1 docker pull
```

This chain of commands reformats the output of `docker images` to pull a new
version of all tagged images on the host.

`docker images` should be self explanatory.
Afterwards we use `awk` to build `image:tag` for each image on each line.
We also want to skip images that are untagged (i.e. the tag reads 'none').
Obviously we also want to drop the first line of the `docker images` command
via inversely matching on *'repo'*.
Last we pipe in each line separately (`-n1`) into a docker pull call.

We can use a similar approach to delete untagged images
after upgrading and restarting all containers.
(I.e. just grapping 'none' lines and feeding it into `docker rmi`)
```
docker images | grep '<none>' | awk '{print $3}' | xargs -n1 docker rmi
```

Add them to your `.bashrc` for convenience (**NOTE:** you need to escape
special chars):
```
alias docker_update="docker images | awk 'BEGIN{OFS=\":\"} {print \$1,\$2}' | grep -v 'none' | grep -iv 'repo' | xargs -n1 docker pull"
alias docker_cleanup="docker images | grep '<none>' | awk '{print \$3}' | xargs -n1 docker rmi"
```

# Restart all systemd managed docker containers

If we expand the unit files listed in
[docker quicktips #1]({filename}/sysadmin/09_docker_quicktips.md)
with `PartOf=` lines we can achieve services that 'propagate' restarts.
See the [systemd] man pages for more info on `PartOf=`.

```
[winlu@winlu-main system]$ cat docker_nginx_reverse.service
[Unit]
Description=docker nginx_reverse proxy container
After=docker.service
Requires=docker.service
PartOf=docker.service

[Service]
Restart=always
ExecStartPre=-/usr/bin/docker stop -t 2 nginx_reverse
ExecStartPre=-/usr/bin/docker rm nginx_reverse
ExecStart=/usr/bin/docker run --rm --name nginx_reverse -p 80:80 -p 443:443 -e DEFAULT_HOST=heroicdebugging.biz -v /docker/nginx_reverse/certs:/etc/nginx/certs -v /docker/nginx_reverse/vhost.d:/etc/nginx/vhost.d:ro -v /var/run/docker.sock:/tmp/docker.sock jwilder/nginx-proxy

[Install]
WantedBy=multi-user.target
```

[Docker]: https://www.docker.com/
[systemd]: http://www.freedesktop.org/wiki/Software/systemd/
