Title: Installing Arch Linux ARM on a Raspberry Pi Model B
Date: 2014-02-07 18:37:21
Tags: Arch, ARM, Installation, Server, RPi
Summary: A guide of the Basic Installation and Setup process of Arch Linux ARM on a Raspberry Pi

#Intro
##Who is this guide for?
This Guide is written for people who have worked with Linux before and know their way around the console (can use nano to edit files and so on) and need some guidance to setup an Arch Linux ARM Server.
##What is the Idea behind this guide?
There are many, many guides about installing various operating systems on the Pi out there. This one will try to point you to wikis and articles which will answer the most questions and get you to the point where you know where you can find documentation about problems you are experiencing yourself.

##Why Arch Linux ARM?
There are innumerable blogs and articles about what OS is the best for the Pi. All with different outcome. Thankfully, as a long time arch user, I did not have to bother to look at other available distributions. Here are the reasons why I personally would always recommend Arch:

Arch Linux follows the philosophy of simplicity. You get exactly what you need, nothing more. This suits the Pi very well, since you have to work with 'limited' resources.

Chances are you will also learn a lot more about Linux in general if you work with Arch since you have to configure and setup a lot yourself. This is often viewed as a drawback of Arch, but lines up perfectly with the original idea of the Pi. Taken from the [Raspberry Pi FAQ](http://www.raspberrypi.org/faqs#introWhatIs):
> We want to see it being used by kids all over the world to learn programming. 

Granted, programming might be a bit different then wrestling with Arch but close enough. 

Additionally the Arch Linux wiki is very, very good and if something fails there is usually a quick solution found in the forums with Google.
#Installation
##SD Card Creation
This part is fairly straight forward and you can simply follow the installation instructions on the [archlinuxarm.org](http://archlinuxarm.org/platforms/armv6/raspberry-pi) Website.

What you should not forget though is that you might be using a bigger SD Card then the Image(2GB) is made for. You can use [gparted](https://wiki.archlinux.org/index.php/GParted) or a similar tool to resize your root partition. Google brought up [this helpful guide](http://elinux.org/RPi_Resize_Flash_Partitions#Manually_resizing_the_SD_card_using_a_GUI_on_Linux).

If you insert your SD-Card into your Pi now and power it up, you can log into your brand new arch system locally to configure it further.
##Pacman (packet management)
After login in you are greeted by the standard bash shell. If you need a refresher on how to use Pacman or maybe it is your first time using Arch, have a look at [Pacman - An Introduction](https://wiki.archlinux.org/index.php/Pacman_-_An_Introduction) and the slightly more advanced [pacman#Usage](https://wiki.archlinux.org/index.php/pacman#Usage).
##Following the Beginners Guide
Even after my third Arch Installation I still use the [Beginners' Guide](https://wiki.archlinux.org/index.php/Beginners'_Guide#Locale) to make sure I don't forget an essential step. You can skip the beginning up to the part where the guide asks you to chroot into the new installation, because you are already in the right environment.

Just follow the guide to configure the following things:

* locale - for rendering Text and special characters
* font and keymap - Default font is fine but if you don't use a qwerty keyboard you definitely want to set a suitable keymap
* time zone - self explanatory
* hostname - a little personality for our little pi
* network
    * lan - skip if you want to work local only
    * wlan - if you have a usb wifi dongle (google for a compatibility list if you plan on buying one)
* root password

##Arch Linux specific settings that make sense
Now you might want to turn your attention on general things that improve performance. The [Archlinux Wiki](https://wiki.archlinux.org/index.php/Raspberry_Pi) contains a lot of articles how to improve performance on various hardware like laptops or the RPi.

###Maximizing SD Card Performance and Life
####/etc/fstab
Mounting is the act of making a file system usable on Unix systems. To improve performance you can set special flags that make more sense then the default ones since you are working on an SD Card and not a standard hard disc drive. [Link](https://wiki.archlinux.org/index.php/Raspberry_Pi#Tips_for_Maximizing_SD_Card_Performance)

Keep in mind that your /etc/fstab entry might look different. You are looking for the line which has a single / in the second column which represents your root partition. Once you located the right line you want to change the options in the 4th column to [noatime,discard](https://wiki.archlinux.org/index.php/fstab#Field_definitions). 
####Using a tmpfs for /var/log
If you keep following the [Arch Linux wiki article for the Raspberry Pi](https://wiki.archlinux.org/index.php/Raspberry_Pi#Move_.2Fvar.2Flog_to_RAM) you will see that the next course of action is to create a ram based filesystem for your logfiles so further reduce write cycles to your SD Card. This makes sense and is not hard, but keep the warning in mind that all system logs are lost on reboot!
###Overclocking
Even without additional cooling and a cheap housing i had no heat problems at all. So if you have the feeling you need a bit more oomph you can [edit](http://elinux.org/RPiconfig#Overclocking) the /boot/config.txt file to your liking.

Keep in mind that an overclocked cpu needs higher voltage and can lead to strange issues with your usb peripherals (like connection loss with a wifi dongle)
###Hardware random number generator
The Pi comes with a Hardware RNG. To use it you have to edit /etc/conf.d/rngd as described [here](https://wiki.archlinux.org/index.php/Raspberry_Pi#Hardware_Random_Number_Generator).
#What is next?
Before you start tinkering with your install and add a lot of packages and configurations you might want to consider backing up your rasperry pi sd card so you can reset it easily if you screw something up.

Maybe add some users (and learn something about Linux user and group management) so you don't have to use root all the time.

Sick of using console only? Install a [desktop environment](https://wiki.archlinux.org/index.php/Desktop_Environment). Though you should probably stay lightweight (LXDE or XFCE) for performance reasons.

If you want to go down the server route you might want to look at setting up a [SSH Server](https://wiki.archlinux.org/index.php/Ssh).

Or maybe you want to use it as a media player connected to your TV and install one of the media center applications like [xbmc](https://wiki.archlinux.org/index.php/XBMC).

Whatever you want to do, you are now in posession of a great base system to tinker with! 

Have fun with it!
