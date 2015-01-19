Title: Writing Pelican Plugin tests
Date: 2015-01-16 14:37
Tags: python, pelican, unittest
Summary: Short intro on how to write tests for your pelican-plugins and rambling about the structure of pelican itself.

#A New Plugin
So you wrote a plugin for [pelican], good for you! But how do you test that it does what it should do and how do you handle `python2` / `python3` issues?

Looking at the [pelican-plugins] repository, only very little testing is done.
You might argue that for simple content modification no tests are necessary, but since you should cover both `python2` and `python3` it is highly likely that there is something out there that will make your plugin crash.

It is always a good idea to point the finger at yourself when describing 'bad' behaviour.
So let's look what could go wrong what you are not expecting.
For example, have a look at [pelican-toc#1].
I clearly did not think about `python2` behaviour, mixing Unicode with different encodings.

Of course it is not really viable to test your plugins on your own blog with different settings and it first looks like a huge task to setup unittests for your plugin, since you probably don't need to initialize and run the complete [pelican] application just to test some content formatting.
But what is really needed to test your plugins?

#Pelican strikes back
The [pelican-source] code looks kind of intimidating for someone new to `python` or even just new to [pelican].
There is a lot of clutter because of back wards compatibility and lots of (maybe to many) features.
It is an ongoing afford to clean it up and provide better documentation for potential contributors.

Basically, what [pelican] does is the following:

* Pull all settings from files and command line arguments and issue warnings where needed
* initialize plugins
* initialize Generators
* generate Generator context, this is where the heavy lifting gets done and your content get parsed via Readers
* Write the aggregated content into output, while for example, fixing URLs

So, what do we really need of this long list to write some tests for our context modifying plugin?

#Return of unittest
First get an example configuration, you can either go with the one provided in the [pelican-plugins] `test_data` folder or leverage the `get_settings` function from `pelican.tests.support`.
You probably also want to read some test files, most likely `Markdown` or `reStructuredText` formatted, since that is what most commonly used to input content into pelican, so you need some `Readers` from [pelican-readers].
Next step is to save the output from your reader.
In Pelican this is done by [pelican-contents].
Examples for `Contents` would be `Article` or `Page`.
That is all that is needed to simulate some parsed articles, now let us look at how it is done.

These are the imports for the pelican related stuff mentioned earlier.

    :::python
    from pelican.readers import MarkdownReader
    from pelican.contents import Article
    from pelican.tests.support import get_settings

Other imports needed are our testing environment `unittest` as well as our plugin `toc`.

    :::python
    import unittest
    import toc

And now for the main part, a Class extending `unittest.TestCase`, which contains the tests.
We can pull default settings and our reader class wide.
Then we can parse our test article and compare it to what it should look like.

    ::::python
    class TestToCGeneration(unittest.TestCase):
        settings = get_settings()
        md_reader = MarkdownReader(settings)

        def _handle_article_generation(self, path):
            content, metadata = self.md_reader.read(path)
            return Article(content=content, metadata=metadata)

        def test_toc_generation(self):
            article_with_headers_path = "test_data/article_with_headers.md"
            article_with_headers_toc_path = "test_data/article_with_headers_toc.html"
            article_with_headers = self._handle_article_generation(
               article_with_headers_path)
            toc.generate_toc(article_with_headers)
            with open(article_with_headers_toc_path, 'r') as f:
                test_toc = f.read()
                self.assertEqual(
                    article_with_headers.toc,
                    test_toc,
                    "bad toc generated")

    if __name__ == "__main__":
        unittest.main()

Sweet right?
Nothing complex, just pull some default settings, initialize a Reader, read from formatted source files and compare the output with what it should look like.
Now go and write some simple tests, no more excuses!


[pelican]: http://blog.getpelican.com/ "pelican static site generator"
[pelican-contents]: https://github.com/getpelican/pelican/blob/master/pelican/contents.py "pelican contents"
[pelican-readers]: https://github.com/getpelican/pelican/blob/master/pelican/readers.py "pelican readers"
[pelican-source]: https://github.com/getpelican/pelican "pelican source on github"
[pelican-plugins]: https://github.com/getpelican/pelican-plugins "pelican-plugins repository"
[pelican-toc]: https://github.com/ingwinlu/pelican-toc "pelican-toc plugin"
[pelican-toc#1]: https://github.com/ingwinlu/pelican-toc/issues/1 "pelican-toc plugin, issue 1"
