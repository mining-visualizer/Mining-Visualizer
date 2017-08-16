# Mining-Visualizer

### Introduction


![](wiki/images/widget_miners.png)

Mining Visualizer is a [suite of programs](#software-components) with some special features for solo miners.

#### Solo Miners!!!  Seriously, dude ... nobody solo mines anymore!!!

It's true there are not many solo miners out there.  The vast majority of hashing power is being directed to pool mining operations (currently about 94%).  If you have only 1 GPU, the average time to mine a block is currently something ridiculous like **750 days**! So if that's you, I'd recommend sticking to pool mining.  However, larger mining operations may be interested in some of the features that I've added to this program.  

#### Motivation

I started solo mining myself about a year ago with 1 GPU.  As you can imagine, the biggest downside to solo mining is the **waiting**.  I would often sit there wondering, "Is the miner even doing anything?  How do I know it's still working?"  Doubts started creeping into my mind. "What if I never mine a block!"  

Being a programmer, ideas started coming into my mind about ways to make the experience of solo mining a little less painful.  Here's what I came up with:

#### Desktop Widgets

Mining Visualizer comes with 4 desktop widgets that display various data items that let you monitor your mining operation at a glance.  These are powered by **Rainmeter** for Windows, and **GeekTool** for the Mac.  Unfortunately there is nothing available for Linux in this category (but see below for the Web App).  Most of the items are pretty self explanatory, but I will comment on a few.

![](widget_miners.png)


* **Hash faults**: the total number of hash faults that have been reported from your mining farm over the last 24 hours.  This line will display in Yellow if any of your miners has exceeded the user defined hash fault limit.  The miner is a fork of Genoil's ethminer, and it checks for hash faults **much more often** then Genoil's did (1 hash check per kernel run), so don't be surprised if the numbers seem high.  This will give you an early indicator if one of your GPUs is failing, or you're pushing it too hard with overclocking.


### Mining Activity

![](widget_activity.png)

* **Last Solution** - the last time you mined a block
* **Next Solution** - an estimate of when you will mine your next block, based on your current hash rate and the network difficulty. Remember,  though, there is no guarantee that you will mine a block by this time.  The generation of hashes (mining) is a completely random process.  It is like rolling a dice.  Variance will affect your mining results.  Sometimes you will mine a block sooner,  sometimes later.
* **Target** - the upper 64 bits of the current network Target.  If your miner finds a hash value *lower* than the target, you will become the miner of the next block, and receive the appropriate reward.     
* **Best Hash**: the upper 64 bits of the best hash found by your mining rigs, since you last mined a block. Remember, lower is better.

**Note:** - I was mining on the ETC network when I took these screenshots, which has a much lower difficulty level.

### Network

![](widget_network.gif)

* **Balance** - Of course, mining a block is the **big, exciting event** that every solo miner waits for, so I wanted to make that stand out as much as possible. This line displays the current balance of your mining account.  It will cycle through one of five colors every time you mine a block to give you an easy-to-see, visual indicator of this momentous event.  You can customize the colors, or even disable it if you think it's tacky.


### Close Hits

"If mining a block is what we are all waiting for", I thought, "why not keep track of when you ***almost*** mined a block".  The miner keeps track of hashes computed that are within a certain range of the Target value, and reports them as *Close Hits*. The desktop widget shows the last 5 close hits found by your mining farm, with a little accompanying bar graph. Smaller values are better.

![](widget_closehits.png)


## Web App

MVis comes with a 3 page web application to display information pertaining to your mining operation.  There is some overlap between this and the desktop widgets, but the web app  generally shows more detail.

#### Mining Rigs

![](web_app_miners2.png)

<br>

#### Graphs

* This shows *Solutions*, *Close Hits* and *Work Units*. 
* Work Units are the same idea as Close Hits, except with a much lower level of difficulty.  You could also compare them to 'shares' (from pool mining), except there is no reward when you find one. Their purpose is simply to give you a way to verify that your mining rigs are performing as they should.
* Close Hits are graphed individually, whereas Work Units are grouped into 4 hour intervals and displayed as a histogram.


![](web_app_graph.png)

<br>

#### Hash Plots

* This allows you to see a scatter plot of real-time, selected hash values from your miner.
* The purpose of this feature is part educational (a nice visual representation of what miners actually do), part psychological (provides reassurance that the miner is still working and hasn't locked up or frozen), and also just because I thought it would look pretty cool!
* The hashes are scaled down to values between 0.0 and 1.0.
* The Current Target value (similarly scaled) is shown for reference.


![](scatter.gif)


<br>

### Software Components

This software suite is made up of two programs.  The first is the program in this repo, which I refer to as Mining Visualizer proper, or just MVis.  It is basically a relay / data collection program that communicates with the miner and drives the desktop widgets and the web application.  The second component is [the miner](), which is a fork of Genoil's ethminer.  I often refer to this program as *MVis-ethminer*.

For software downloads, please visit the Github repo.  There is full documentation in the wiki.  If you find any problems, please report them here, or file an issue on Github.
