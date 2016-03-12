## Friday, November 16, 2012

### [A (Mini) Programming Tour](http://www.adomokos.com/2012/11/a-mini-programming-tour.html)

The TL;DR version

If you can't go to conferences, try to visit companies. Even in your own home town. You get to know many people and learn a lot from fellow developers.

* * *

I spent the last week in Chicago visiting four different companies, shadowing, talking and pairing with fellow developers. A good friend of mine shared his condo in downtown for a couple of days which made this trip very affordable for me. Here are the companies I visited and a brief summary of what I saw there:

**:: TrunkClub**

I did not know much about [Trunk Club](https://www.trunkclub.com/) up until a couple of months ago. I reached out to [Corey Haines](http://coreyhaines.com/) seeking companies I could visit and he suggested them. They have a small but very talented group of developers building their - mostly internal - apps in Ruby on Rails. I shadowed [Corey Ehmke](http://www.idolhands.com/) on the project he was currently working on: he tried to come up with a recommendation engine using MongoDB and Ruby. Having seen him exercising the different algorithms I was gently reminded that I should probably brush up on my statistical skills.
Have you seen a company that has its own beer tap and wine cellar? Well, Trunk Club is one of them! At the end of the day we enjoyed the different variety of beers right in their offices. Now how cool is that?!

![at_trunkclub](/resources/2012/11/IMG_0011.jpg)

**:: Hashrocket**

I spent the next day at the [Hashrocket](http://hashrocket.com/) Chicago office. They have a cute little space converted from a condo with a couple of bedrooms attached so people visiting from the Jacksonville, FL office can stay there. A large video screen is linked up with their home office where people stop by and say hello to the folks in Chicago.
I shadowed [Matt Polito](http://hashrocket.com/people/matt-polito) first who remote paired with another rocketeer. They used [tmux](http://tmux.sourceforge.net/) for sharing their terminal sessions. I heard about tmux before, but I have not tried it yet. I noticed that developers even used it locally when they were not pairing with anybody else for the benefit of being able to suspend and resume sessions.
Interestingly the guys at Hashrocket are using a strategy pattern based solution to solve complex problems which is very similar to what I described in my [Refactoring Workflows to Chain of Actions](http://www.adomokos.com/2012/04/refactoring-workflows-to-chain-of.html) blog post.
They also used Google Plus for video conferencing with multiple people. I am not a big fan of social media but I'll definitely check out Google Plus for this.

**:: 8th Light**

The company's new office is very close to [Union Station](http://www.chicagounionstation.com/), which makes it easy for the commuter employees to get there. Had I not checked their current address on their web site Google Maps would have sent me to their former office.
I spent the morning shadowing [Colin Jones](https://twitter.com/trptcolin), who worked on a file uploader web app in Clojure. I noticed how much more readable is [speclj](https://github.com/slagyr/speclj) compared to Clojure test, I am going to switch to that! He also wrote a [multi-method](http://clojure.org/multimethods) implementation that I only read about before.
The web app used [joodo](http://www.joodoweb.com/) as the underlying web framework which seems very clean to me, but the views were built using [hiccup](https://github.com/weavejester/hiccup) which can be a bit too cryptic for a developer who spent a long time in HTML land.
I wrapped up my day pairing with an other engineer on some Backbone.js code test-driving it with Jasmine.

**:: Groupon**

I only visited [Groupon](http://www.groupon.com/), I did not sit down and paired with anybody there. Our host, [Michael "Doc" Norton](http://www.docondev.com/) showed us around. Their office seems like a fun place and I have never seen so many 27" Cinema Displays in one room. Developers are working in small groups and everybody can pretty much find the project they want to work on.

What's my takeaway from all this?
I met with many talented developers. I learned how they work, what tool they use, how they develop software. I will give joodo and Clojure a try and will build a web app using them just to learn the language and the paradigm.
I know people in the US don't have a lot of vacation. But if you can do it, maybe just one day a year, visit other companies. The benefits are enormous!

I'd like to thank my current employer, [Dimple Dough](http://dimpledough.com/), sponsoring and helping me with this trip!


POSTED BY ATTILA DOMOKOS AT 9:21 AM

#### 2 comments:

[Peter Zsoldos](https://www.blogger.com/profile/08451051975901266470) said...

_Hi,_

_thanks for sharing your experiences!_

_I wonder if you could tell more about how you prepared for it - selling the idea to your company, finding hosts, how you found Cory (you met him before? or just via twitter/email?), etc. These may be obvious/easy to you, but likely would help those who haven't done it before._

_Also, if you have attended a coderetreat or dojo, I would be curious how a company visit compares to that._

November 16, 2012 at 10:17 AM

[Attila Domokos](https://www.blogger.com/profile/09067995287578229487) said...

_@Peter: Find my answers to your questions in my [follow-up blog post](/blog/2012/11/preparing-for-my-visit-in-chicago.md)._

November 21, 2012 at 9:02 AM
