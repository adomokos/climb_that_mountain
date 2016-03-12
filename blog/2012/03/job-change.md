## Monday, March 5, 2012

### [Job Change](http://www.adomokos.com/2012/03/job-change.html)

After about a year of employment I decided to leave [my employer](http://www.within3.com/).
We have had great times together: I worked on their software rewrite, helped them move from 23 (most of them failing specs) to some 5700 passing specs and we started on the curvy path of automated acceptance tests. I had worked with some amazing people there whom I'll miss in the future.

I wasn't looking for a new job. No, but an opportunity came up and I did not want to miss it.

A good friend of mine - [Dave Speck](http://www.linkedin.com/in/davespeck) - joined a small startup in Independence, OH late last year. I talked to him a couple of times and it seemed I could do a lot of things there. A few meetings and some beers later I made up my mind: I joined [Dimple Dough](http://dimpledough.com/) in the middle of February.

I have only worked there for a little over a week but we have already done so much!
Here is one of them:

We can provide basic translation for our international clients, but some of them want to further customize it. The only way they can make translation changes is sending us what they want to change and one of our engineers has to do the updates in our database. Our customers would be happy to do it themselves, if there was a tool they could use. Hence the translator idea was born. We had a vague idea what it will look like but we did not know how the tool SHOULD exactly WORK.

We started by prototyping the tool in pure HTML with CSS and JavaScript. The benefit of doing this is the low cost of change. Imagine how far less expensive it is to modify a raw prototype than the fully functioning product. There are no domain objects, data models, data migrations to change when the client wants to tweak the preview version. It's just a dummy HTML with very simple jQuery that allows us to demonstrate it to the client who can provide us feedback well before development begins.

Once we knew our prototype was close to what we wanted to build and our customer was happy with it we sat down with our business and quality focused team members. In this ["three amigos"](http://jonkruger.com/blog/2012/01/04/the-three-amigos/) meeting (BA, QA and Developer) we wrote scenarios in [Gherkin](https://github.com/cucumber/gherkin/wiki) syntax using our prototype and other documentation the team had collected by then.

![cucumber](/resources/2012/03/cucumber.jpg)

It made me smile to realize that after the first three or four scenarios we were discussing edge cases nobody thought about before. The scenarios we came up with are short and are not tightly coupled to the User Interface, they explain how this new tool should behave.

I tried using Gherkin and cucumber at my previous employer, but I don't think it really caught on there. After talking with [@chzy](https://twitter.com/chzy) ([Jeff Morgan](http://www.cheezyworld.com/)) on a cold December morning I understood why: we used Gherkin for automated system testing and not to discover functionality with BAs and QAs prior to development.


POSTED BY ATTILA DOMOKOS AT 9:16 AM

#### 2 comments:

[Josh Schramm](https://www.blogger.com/profile/04179432092617833919) said...

_Woah man i had no idea you moved. You work right downstairs from me now one of these days we should grab lunch. I'm on on 7 at stone crossing,. Congrats by the way_

March 5, 2012 at 5:47 PM

[Attila Domokos](https://www.blogger.com/profile/09067995287578229487) said...

_Thanks Josh! I did not know we work so close. I'll stop by after my trip to FL._

March 5, 2012 at 7:47 PM
