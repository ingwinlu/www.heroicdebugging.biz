Title: Building and Hosting your static site in the Cloud
Tags: howto, pelican, cloud, github, travis, cloudflare
Date: 2015-01-29 16:44:00

This will be a short howto on how to host your static site for free via [gh-pages], build it on [travis-ci] and deliver it with [cloudflare]. I'll try to keep it simple, linking to the respective sites instead of doing a `whatdoIhavetotypehere` kind of guide, since those usually end up making problems as soon as one of the mentioned sites changes things.

You also don't have to do everything in this guide to benefit from other parts, it is completely reasonable if you want to host and build your files locally, but want to speed up delivery via [cloudflare] for example.

I am going to use [pelican], but you can substitute it with any other static site generator.

#Hosting
##gh-pages
First things first, hosting your content and the generated site.
I went with [gh-pages], since it is free you don't need to fiddle around with credit cards (looking at you, amazon s3).

You will have to decide if you want to use a project site or a user / organisation site before creating a repository.
The differences are not big, mainly the resulting domain if you don't want to use your own.

User sites have the predefined repository name `yourusername.github.io` and will be available under the same url.
Additionally your html output will land in master and content / build files will be in a arbitrary branch.

Project sites don't have a restriction on naming and your content will most likely be in master while the built site will need to be in a branch called `gh-pages`.

###domain
Your data will be available on a domain following these [patterns](https://help.github.com/articles/about-custom-domains-for-github-pages-sites/#how-github-pages-sites-use-custom-domains).

If you want to use a custom domain you will have to add a file called `CNAME` to your output.
In [pelican] you can do this via `EXTRA_PATH_METADATA` and manipulate its path.
For an example feel free to look at my [config](https://github.com/ingwinlu/www.heroicdebugging.biz/blob/master/pelicanconf.py#L31).

Note that custom domains have some implications if you want to use `ssl`. More on that later though.

###using travis-ci to build
Next step is to let [travis-ci] build your page for you instead of you doing so yourself.
Usually [travis-ci] is used to run automated test cases for projects hosted via github.
We can use that to our advantage and configure travis to install a static site generator of our choice, let it generate sites with our content and then push back to github into an appropriate repository / branch.

To prepare our content on github for remote building, we need to add themes and plugins to our repository.
Keep in mind that if you just copy them in there, you might hurt licensing and / or have trouble with updates.
I choose to link to the original repositories containing the data via git submodules.
For example, to add [pelican-twitchy] to our content repository, we use `git submodule add https://github.com/ingwinlu/pelican-twitchy.git`.
Use a public accessible url to add though, just to make sure you don't run into problems with authentication.
The same method can be used to add plugins as well.

Next we need a configuration file for `.travis.yml`.
It basically defines what we want travis to do and in what environment we want to do it.
[Mine](https://github.com/ingwinlu/www.heroicdebugging.biz/blob/master/.travis.yml) sets the language, installs requirements for [pelican] to run, and then executes the Makefile from my repository to build html and push back to github.

    :::yaml
    language: python
    branches:
      only:
      - master
    install:
    - pip install Markdown
    - pip install beautifulsoup4
    - pip install typogrify
    - pip install pelican
    - pip install ghp-import
    script:
    - make github

For the last part we somehow need to authenticate travis to access our repository.
We archive this by generating a [token on github](https://github.com/settings/applications).
For `scope`, just the `public_repo` is enough.

Now we just need to encode the token so it is only for travis to use.
To do this use the [travis ruby application](https://github.com/travis-ci/travis.rb#installation).
After you installed it you can generate a secret environment which contains your token.

    :::yaml
    env:
      global:
      - secure: "Y/rJPvggGyv19kwded6ZXPhuOr1RryBvwUBUOl+6e732gAKh/ox0sNtQB1XAObSZr7hkyONa+ulzOrBL4OcxjZlSfLLSWSzVqqiHn2St0AerpPHOsQ39dstP8uPzGb/ont7t9LF+igm3Jd8MLtUCsZqvyPSlf4JnWpEvP3nHfk0="

Simply append this to the rest of your `.travis.yml`.

That should be enough to get your site build.
For more information regarding [travis-ci] integration, have a look at this [blog post](http://blog.mathieu-leplatre.info/publish-your-pelican-blog-on-github-pages-via-travis-ci.html) which goes a bit more into detail then mine.

#Delivery - ssl / caching cloudflare
There is no way you can serve your own certificate via the methods mentioned above.
The reason for this is the nature of ssl itself.
Github serves everything under their own certificate, which of course, as soon as you set a CNAME for your site, will not be valid anymore (Certificate is issued for github.io subdomains, but you want to call your site via your own custom domain).

There is a way around that however.
Using [cloudflare] we can deliver our content using their servers and their certificate which will sign our domain as well.
Additionally you can let cloudflare optimize your site and use their caching to deliver your site faster.

Keep in mind however, now you are using all advantages of ssl anymore.
For example, if [cloudflare] decides do modify your content, it would not trigger an invalid ssl warning, since the connection is being made with them directly, encrypted using their certificate.
This negates a lot of the advantages of ssl.
However traffic is still encrypted, so third parties still have issues reading the traffic directly.



[gh-pages]: https://pages.github.com/ "Github Pages"
[travis-ci]: https://travis-ci.org "travis"
[cloudflare]: https://www.cloudflare.com/ "cloudflare"
[pelican]: http://pelican.readthedocs.org/en/3.5.0/ "pelican static site generator"
[pelican-twitchy]: https://github.com/ingwinlu/pelican-twitchy "pelican-twitchy theme"
