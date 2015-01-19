Title: Find the number of dir changes needed to get from path1 to path2
Date: 2014-05-12 22:31:22
Tags: python, common, path
Summary: Derping around with python and paths

First find a common path, afterwards split on `os.sep`. Handle the special case if the common path is actually root. Done. (If your code does not have to run on Windows that is...)

Check out the [Gist](https://gist.github.com/ingwinlu/20615105bba903ccf0d8).


    :::python
    import os
    
    path1 = os.path.abspath('/home/winlu/gitrepos/commonmagic/')
    path2 = os.path.abspath('/home/winlu/')
    path3 = os.path.abspath('/home/winlu/youtubefiles/')
    path4 = os.path.abspath('/var/log/')
    path5 = os.path.abspath('/')
    
    distance_1_2 = 2
    distance_1_3 = 3
    distance_2_3 = 1
    distance_1_4 = 6
    distance_1_5 = 4
    
    
    def _sub_dir_count(path):
        return len(path.split(os.sep))
    
    #wont work on windows, or some cases (1_5)
    def calc_dist(path1,path2):
        prefix = os.path.abspath(os.path.commonprefix([path1,path2]))
        if(prefix==os.sep):
            prefix=''
        dist_a = _sub_dir_count(path1) - _sub_dir_count(prefix)
        dist_b = _sub_dir_count(path2) - _sub_dir_count(prefix)
        return dist_a + dist_b
    
    #provided by saimn
    def calc_dist2(p1, p2):
        path = os.path.relpath(p1, p2)
        return path.count(os.sep) + 1
        # or: len(path.split(os.sep))
    
    assert (calc_dist(path1,path2) == distance_1_2)
    assert (calc_dist(path1,path3) == distance_1_3)
    assert (calc_dist(path2,path3) == distance_2_3)
    assert (calc_dist(path1,path4) == distance_1_4)
    #assert (calc_dist(path1,path5) == distance_1_5)
    
    assert (calc_dist2(path1,path2) == distance_1_2)
    assert (calc_dist2(path1,path3) == distance_1_3)
    assert (calc_dist2(path2,path3) == distance_2_3)
    assert (calc_dist2(path1,path4) == distance_1_4)
    assert (calc_dist2(path1,path5) == distance_1_5)


`This is just a quick hack, and will fail in some cases.`

`Use at own Risk.`
