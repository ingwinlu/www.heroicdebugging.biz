Title: Presenting kuhbox v0.1
Slug: kuhbox_01
Date: 2014-02-17 21:48:23
Tags: python, dropbox, kuhbox
Summary: Introduction to kuhbox, a simple backup solution based on python and dropbox

I was looking for a way to backup my home directory on my pi in an easy way with dropbox. Since there is no 'real' client for the arm architecture I had some digging to do. The [dropbox-uploader](https://aur.archlinux.org/packages/dropbox-uploader-git/) available in the AUR comes pretty close to what I was searching for, but I did not want the backup script to have write access to the main area of my dropbox account. Reason behind this is that there is only very little version control and I did not want to overwrite important files due to carelessness. So I set up my [own app](http://www.dropbox.com/developers/apps) without access to the main area. 

Next step was to write up a small python script which takes the login routines from the official [client](http://www.dropboxwiki.com/tips-and-tricks/using-the-official-dropbox-command-line-interface-cli), combining it with a simple os.walk and naming it **kuhbox**.

    :::python:
    def main():
        #usage
        if(len(sys.argv)!=2):
            print("usage: %s path_to_upload" %(sys.argv[0]))
            return

        #try to convert the argument to an absolute path
        rootdirabs = os.path.abspath(sys.argv[1])
        
        #get the client object and try to login if necessary (similar to the example client)
        kb = Kuhbox()
        kb.do_login()
        
        #crawl over the directory or just upload the file that is given as argument
        file=""
        if (os.path.isfile(rootdirabs)):
            kb.put_file(rootdirabs,rootdirabs)
        else:
            for root, _, files in os.walk(rootdirabs, topdown=True, followlinks=True):
                for name in files:
                    file=os.path.join(root, name)
                    kb.put_file(file,file)

What you can not see in this code snippet is that I set the *overwrite flag to True*, meaning that there will be no check if the file available in dropbox is actually newer than the local copy. Also I did stumble about some problems with the dropbox python SDK and python3. There appears to be some problem with the *upload_chunked()* function for uploading large(>150MB) files.

I do plan to expand the script and eventually make it public when some core features are implemented(i.e. a basic form of version control, fixed chunked upload, logging, ...), but for now it is doing what I want from it in a cron job every day.

Overall I really like the python dropbox api. It is simple to use and yet very powerful.

