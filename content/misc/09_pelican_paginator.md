Title: Pelican Pagination 'Pager'
Date: 2014-09-19 14:25:24
Tags: pelican, jinja2
Summary: How to implement a Pager for Pelican Pagination


I have written a Pager for [pelican] and the the [pelican-bootstrap3] theme some time ago. This will display a simple Newer/Older link:
```
<ul class="pager">
    {% if articles_page.has_previous() %}
        <li class="previous"><a href="{{ SITEURL }}/{{ articles_previous_page.url }}">&larr; Newer</a></li>
    {% else %}
        <li class="previous disabled"><a href="#">&larr; Newer</a></li>
    {% endif %}
    {% if articles_page.has_next() %}
        <li class="next"><a href="{{ SITEURL }}/{{ articles_next_page.url }}">Older &rarr;</a></li>
    {% else %}
        <li class="next disabled"><a href="#">Older &rarr;</a></li>
    {% endif %}
</ul>
```
If you want to add a First/Latest as well, you can access their urls like this:
```
<li><a href="{{ SITEURL }}/{{ articles_paginator.page(1).url}}">First</a></li>
<li><a href="{{ SITEURL }}/{{ articles_paginator.page(articles_paginator.num_pages).url}}">Last</a></li>
```
You might want to add `if, else` here as well to disable or highlight them if they are currently active as well.

Happy designing :)

[pelican]: http://docs.getpelican.com/en/latest/ "Pelican"
[pelican-bootstrap3]: https://github.com/DandyDev/pelican-bootstrap3