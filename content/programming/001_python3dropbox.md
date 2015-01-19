Title: How to make the dropbox SDK work with python3
Date: 2014-02-18 23:31:22
Tags: python, dropbox, kuhbox
Summary: Or why you should always think horse when you hear hoof-beats, not zebras.

#The Problem

I mentioned in my post yesterday about [kuhbox](/tag/kuhbox.html) that I was having trouble with the *upload_chunked()* function of the dropbox python api. At first I thought it was a classical python2 -> python3 Bytes problem. Trying to invoke the function call results in the following error message:

    :::bash
    [winlu@micronuke kuhbox]$ ./kuhbox.py upload_test_160mb
    [loaded OAuth 2 access token]
    put big file /home/winlu/python/kuhbox/upload_test_160mb with size: 167772160
    uploaded 0 of 167772160 Bytes
    Traceback (most recent call last):
        File "./kuhbox.py", line 124, in <module>
          main()
        File "./kuhbox.py", line 111, in main
          kb.put_file(rootdirabs,rootdirabs)
        File "./kuhbox.py", line 93, in put_file
          self.put_file_big(from_file,to_file,size)
        File "./kuhbox.py", line 73, in put_file_big
          upload = uploader.upload_chunked(1024*1024)
        File "/usr/lib/python3.3/site-packages/dropbox/client.py", line 1019, in upload_chunked
          StringIO(self.last_block), next_chunk_size, self.offset, self.upload_id)
    TypeError: initial_value must be str or None, not bytes

#The Solution

If we look at the dropbox/client.py we see the following line:

    :::python
    (self.offset, self.upload_id) = self.client.upload_chunk
        (StringIO(self.last_block), next_chunk_size, self.offset, self.upload_id)

Of course this won't work with python3 as we are opening the files in 'Byte' Mode and BytesIO is something entirely different to StringIO in python3 (contrarely to python2). The easy fix to this is to simply remove the StringIO to get the following:

    :::python
    (self.offset, self.upload_id) = self.client.upload_chunk(
        self.last_block, next_chunk_size, self.offset, self.upload_id)
    
While this appears to be working for me on python3 on ArchARM, I am not too sure on the effects of this in an python2 environment and there should probably be an if on the python version or something similar. 

#The Trouble

Now why would this take a day(take a few hours where I quit because of frustration)? As stated above I thought way too complicated to even see the problem at first (aside from the obvious TypeError). I suspected some python3 incompability with the IOStream, first trying to replace it with a BytesIO Stream and digging through the ssl.py for a problem similar to [this one](http://docs.python.org/3/howto/pyporting.html#indexing-bytes-objects). 

Additionally I somehow thought that the *upload_chunked* function only uploads one chunk, not all of them. Somehow I jumped to it because the error handling is done there, giving it the looks of it.

Here is a [Link](https://forums.dropbox.com/topic.php?id=112358) to a discussion I started on the dropbox api forums.

#The Future

With one of the main issues of the [kuhbox](/tag/kuhbox.html) resolved I can now move on to the other ones on my list. But this definitely was the one thing I was most worried about. The rest should only be some code monkeying.