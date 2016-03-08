## Tuesday, August 17, 2010

### [Where Should I Host My Family Videos?](http://www.adomokos.com/2010/08/where-should-i-host-my-family-videos.html)

Wow, it's been a while since I last posted anything on my blog! A lot has happened: I spent an entire month with my family in [Hungary](http://en.wikipedia.org/wiki/Hungary). I even took a short trip to [Austria](http://en.wikipedia.org/wiki/Austria) as well. I enjoyed many [Black Forest cakes](http://en.wikipedia.org/wiki/Black_forest_cake) with great [cappuccinos](http://en.wikipedia.org/wiki/Cappucino).
Returning to the US has its ups and downs: on one hand we are back in our comfortable home, on the other we miss all our relatives living on the Old Continent.

We've been posting short - 30 seconds long - videos on our family web site that I host on my home server. We have a LAMP setup with [Gallery2](http://gallery.menalto.com/) on it. It has served us well, however, our cable provider noticed that there is significant data download through our 8080 port. I received their first warning letter right after we got back. It was pretty obvious that I had to look for some kind of a cloud hosting solution.

Some people recommended using [Amazon S3](http://aws.amazon.com/s3/) but I wanted to stick with Google. Posting photos to a Picasa web album is very simple but it has one shortcoming: you can't upload a video in flv format. You can upload them to YouTube and then somehow link that into the album but I just was not ready to go with that hybrid solution.

We chose Blogger.com to host our photos and videos. We can easily upload photos there plus adding a little more text is always welcomed by our family. But what about the videos? Well, Google Sites is there for the rescue.

I use the unbelievably awesome [ffmpeg](http://www.ffmpeg.org/) to convert my ~60 MB 30 seconds clip into a 1 MB flash movie.

For you, dear reader, here is the script I use:

<pre class="brush: ruby">ffmpeg -i [your_avi_movie.AVI] -ar 11025 -ab 32 -f flv -s 320x240 [your_output_movie.flv]</pre>

This might seem cryptic when you look at it first, but google ffmpeg and you should find all the info you need. For example the "-f" switch is the format, "-s" is the size.

Once the movie is converted, I just upload it to my Google Site. It was not straightforward where and how to upload it, so here are the steps I took:

1) Go to your Google site (you should have one with a Google account)

2) Go to More Actions => Manage Site

3) Under "Site content" select "Attachments"

4) You should see the "Upload" button to upload your movies

Having the movie there is all fine, but I needed a Flash Player to properly play them in the web page. After a quick search I settled with [OS Flv](http://www.osflv.com/). I uploaded the player swf file to my site and I had everything ready to post my videos.

Here is the short HTML code I used to link in the flv movies through the [OS Flv](http://www.osflv.com/) player into our blog page.

<pre class="brush: html"><object height="240" id="flvPlayer" width="320">
  <param name="movie" value="[link_to_os_flv_player.swf]">
  <param name="FlashVars" value="&movie=[link_to_your_movie_file.flv]">
  <embed src="[link_to_os_flv_player.swf]" flashvars="&movie=[link_to_your_movie_file.flv]" width="320" height="240" type="application/x-shockwave-flash"></embed>
</object>
</pre>

And here is a quick movie clip I made in July, 76 MB compressed to around 1 MB. Enjoy!
