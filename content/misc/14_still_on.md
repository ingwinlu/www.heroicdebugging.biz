Title: Is this thing still on?
Date: 2021-01-31 10:00
Modified: 2021-01-31 10:30
Tags: organisational


Wow what a blast to look over the content of this page.
But the reality is, nearly everything is outdated - both content and technology wise.

I was motivated to dust of the current state of things due to a tweet of mine
asking for interest in a writeup about porting an old php app to serverless.

<blockquote class="twitter-tweet"><p lang="en" dir="ltr">Over Christmas we ported a 15year old php application to php7, and deployed it serverless using gcp. I learned a ton. Is that a topic that interests some of you?</p>&mdash; winlu (@derwinlu) <a href="https://twitter.com/derwinlu/status/1350348263686369280?ref_src=twsrc%5Etfw">January 16, 2021</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

Before adding this content I really want to cleanup the blog itself
and make it reflect what i learned over the last years.

I did some initial fixups by getting it to render again with an up2date version of pelican, but there is much more to do:

* plugins
    * update ci/cd pipelines
    * build with current pelican, drop py2 support
    * plugins (pip) installable (as this is the new pelican default)
* theme
    * Cleanup or even replace the theme as it's based on an old bootstrap version, no stripped css, slow and a bit clunky
    * integrations like shares do not work reliable anymore
    * i want something slim that is not depending on third party staying the same

Let's see how long it will take me, preferably less then 3 years this time.