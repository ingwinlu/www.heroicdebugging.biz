Title: ssg_reloader, a static site generation development server
Date: 2015-01-17 17:47
Tags: python, flask, pelican

I wanted to make something better than the plain python http server for [pelican] site development and then thought that actually it wouldn't be too hard to build something which should work for all static site generators.

So i hacked together [ssg_reloader], a simple python / flask app which serves files via http with a twist.
All `.html` files get injected with a short ajax request, which queries the app if files have changed.
If the answer is yes, browsers currently viewing the page get reloaded.

The web stuff is powered by [Flask]. HTML manipulation is done via [beautifulsoup].

[Flask] really made the web side of the application extremly simple.
The code for the `ajax` request is around about the same length as the entire view module:

    :::python
    from ssg_reloader import app
    from flask import jsonify, redirect
    from urllib import url2pathname
    import os

    from utils import inject_js

    @app.route("/_ssg_reloader")
    def ssg_reload():
        return jsonify(reload=app._watchdog.next())

    @app.route("/", defaults={"path": "/"})
    @app.route("/<path:path>")
    def serve(path):
        if path.endswith("/"):
            path = path + "index.html"
        path = url2pathname(path)
        if path.startswith("/"):
            path=path[1:]
        if path.endswith("html"):
            file_path = os.path.join(app._static_folder, path)
            return inject_js(file_path)
        else:
            return app.send_static_file(path)
        return redirect("/", code=302)

You are welcome to give it a try, the code is available on [github][ssg_reloader].
There you can also find a bit more documentation about installation and usage.

[beautifulsoup]: http://www.crummy.com/software/BeautifulSoup/ "beautifulsoup"
[Flask]: http://flask.pocoo.org/ "python flask"
[pelican]: http://blog.getpelican.com/ "pelican static site generator"
[ssg_reloader]: https://github.com/ingwinlu/ssg_reloader/ "ssg reloader github"
