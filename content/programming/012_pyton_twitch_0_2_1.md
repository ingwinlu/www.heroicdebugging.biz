Title: python-twitch 0.2.1
Date: 2014-05-24 22:23:25
Tags: python, xbmc, twitch
Summary: python-twitch has been upgraded to 0.2.1

Hey everyone,

just released an update to python-twitch which reworks the *save to playlist* functionality and also fixes a bug with restricted streams.

    :::python
    def test_speed_of_playlist_generator(self):
        #Speedtest Results: 3.674703420998412,
        #    on rpi at start
        #Speedtest Results: 1.4243742010003189s, 
        #    that is 0.0014243742010003188s per loop
        #Speedtest Results: 1.5008478810050292s, 
        #    that is 0.0015008478810050292s per loop
        #Speedtest Results: 1.186028003692627s, 
        #    that is 0.001186028003692627s per loop
        #    with python2
        
        loops = 1000
        result = timeit.timeit(
            lambda: self.test_playlist_playlist_1_quality_0(),
            number=loops)
        print('\nSpeedtest Results: ' +
            repr(result) + 
            's, that is ' + 
            repr(result/loops) + 
            's per loop')
        
As you can see, my rewrite sped up the function by a minimum of **60%**. Most of this is achived by only running through the downloaded data once and building an array which then can be concatinated, depending on quality settings.

The old implementation also wasn't very readable, jumping around, skipping some lines, etc. Now it should be much easier to understand and be therefore be more maintainable.

The actual code changes can be found in [this commit on github](https://github.com/ingwinlu/python-twitch/commit/f04c6084e2a70f03e3c4f7b34389b7776b8cd147) and will hopefully also be merged into the twitch.tv XBMC plugin soon.

