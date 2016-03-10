## Sunday, April 10, 2011

### [Rapid Feedback](http://www.adomokos.com/2011/04/rapid-feedback.html)

I am not a [WPF](http://en.wikipedia.org/wiki/Windows_Presentation_Foundation) expert. In fact, I don't know it well enough. But let me tell you what it was like working on a WPF project last year.

![desk](/resources/2011/04/IMG_7152.JPG)

It was 10 o'clock in the morning and I sat in my cubicle. I had to pull the latest changes from the source control server. I had to run a couple of batch files to compile all the code that took about 3 minutes. In the mean time I fired up Visual Studio 2010 and ran my latest unit tests to make sure everything was in good shape.

I started up the [WCF](http://en.wikipedia.org/wiki/Windows_Communication_Foundation) services, 86 of them, and a little later I was ready to run the UI app. It took another 30 to 40 seconds to load and get to the login screen. I logged in and selected from the menu where I wanted to get to. That page was a list of items, I had to select one of them just to get to the detail page. Finally, I was there!

Let me sum it up:
* 180 seconds to compile the app
*   50 seconds to fire up all WPF services
*   20 seconds to start the WPF UI App
*   60 seconds to log in and go to the page I had to modify

*TOTAL:* 310 seconds

My task was adding a new TextBox to this page. Simple. I opened up the [XAML](http://en.wikipedia.org/wiki/Xaml) file which was an ugly xml file with weird namespaces and special attributes all over. I grabbed a TextBox XAML code from somewhere, pasted it in, made sure all the namespaces were fine and I was ready to run it.

I had to shut down the UI app, compile the UI project, start it up again, log in, select the menu option to get to the list page and choose one item to see the detail.

Here is how long this took:
* 30 seconds to compile the UI app
* 20 seconds to start the WPF UI App
* 60 seconds to log in and go to the page I had to modify

*TOTAL:* 110 seconds

And it turned out that I did not set up the Grid for this TextBox properly, so I had to do some minor tweaks to the XAML page. I did that, killed the UI app, complied the code, ran the WPF UI app, logged in, went to the page and 110 - or one hundred and ten - seconds later I verified that all look good.

But this was the fast part. Once I had all the UI set up properly, I had to get under the hood and modify the domain object. The change was "simple": just add a text field to the database, modify the domain object, set up the NHIbernate mapping, change the Data Transfer Object, add this field to it and set up its mapping if I had to.
Now to make sure all this worked I had to shut down the UI app, the WPF services. Compile the code, regenerate the NHibernate mappings, fire up the WPF services, run the UI, log in, select the page and pick an item to get to its detail. Simple, right?

Here is the break down:
* 30 seconds to compile the Data Access Code
* 30 seconds to regenerate the NHibernate mapping xml
* 50 seconds to fire up all WCF services
* 20 seconds to start the WPF UI App
* 60 seconds to log in and make sure that all looks good

*TOTAL:* 190 seconds

Wait! 3 minutes just to see if everything is working properly?

Give me a break.

What company with a tight budget and ever approaching deadlines could afford spending 3 minutes just to see if a simple change is functioning properly or not? Who would dare to touch the existing code to clean it up a bit?

One of the great things I like about working with Ruby and Rails is the rapid feedback. No, I am not talking about how long it takes to execute my - Rails disconnected RSpec - tests. (I'll try to write about that in an upcoming post.) I just change the code, hit the browser's refresh button and about 5 seconds later I have the page loaded, the session preserved and I have the answer.

I am talking about 5 seconds and not a couple of minutes.

POSTED BY ATTILA DOMOKOS AT 1:12 PM

#### 5 comments:

[Alex](https://www.blogger.com/profile/00169666883152173097) said...

_As someone intimately familiar with the system you worked on, let me say that you are comparing apples and oranges here. The quality of rapid feedback you are describing here has nothing to do with Ruby on Rails, as you can achieve the same feedback with any web-based HTML/HTTP application._

_A REPL environment would provide instant feedback, so would I be incorrect in using it to blame Ruby for being too slow because it takes 5 seconds to render my code?_

_The reason a web application is much faster is that you are relying on tons of finely tuned infrastructure to achieve that rapid feedback. The web browser has been finely tuned to render HTML payload. The web server running Ruby on Rails has been finely tuned to translate HTTP requests without coming down after a change to a view file, controller file, or the model's database mapping._

_The combination of a custom thick client and a custom server you described simply hasn't had the benefit of the same kind of fine-tuning achieved by millions of man-hours by people and organizations working on improving Linux, Mac OS, Apache, MySql, Ruby and Rails. You were simply able to slide into a smooth groove that was set up for you._

_The friction you describe in this article has nothing to do with WPF, WCF, or NHibernate, nor any combination thereof. The requirements of the application you worked on were different at the time they were selected. (For the record, these requirements and designs were laid down way before Ruby on Rails, Monorail or ASP.NET MVC was in any usable shape.)_

_The friction decreases over time as effort is applied to it, much like sanding a piece of wood. No offense, but you simply chose not to apply any effort to that improvement, and that's fine. But do admit that before you blame the individual frameworks that may or may not have been misused in that particular application._

_Just sayin'._

April 10, 2011 at 4:24 PM

[adomokos](https://www.blogger.com/profile/09067995287578229487) said...

_Alex,_

_I believe that WPF is a great tool for a very complex, high performant, UI intensive application. I could see it being used as a view engine for a real time, stock trading and monitoring software. We both know that the application you are currently working on is not like that. A web UI would be just fine._

_When companies see that the solution is too expensive to use or develop with and they also recognize that other technologies have emerged then they react. They gradually start moving in that direction. They apply "progressive widening", replacing UI parts or pages one at the time, eventually killing the old application (Martin Fowler's StranglerApplication - http://www.martinfowler.com/bliki/StranglerApplication.html)._

_I know you love what you do and I am glad I had the chance to work with you in the past. Good luck and thanks for your comment._

April 10, 2011 at 9:59 PM

[Gáspár](https://www.blogger.com/profile/03913317482022172511) said...

_My thoughts on this: http://gasparnagy.com/2011/04/re-rapid-feedback/_

April 11, 2011 at 3:13 AM

[David](https://www.blogger.com/profile/01359234754306565494) said...

_This post hit a very painful / damaged part of my heart. Between Silverlight and WPF, the biggest issue I had with UI development was needing to rebuild all of the UI assemblies to see minute changes within the app._

_MS has tried to remedy the issue through blend and visual studio, but I have never been able to get them to work with the XAML and display 100% like the application does. This leads you back to the pit of unholy and depressing slow UI development._

_Web dev is so much more satisfying._

August 24, 2011 at 8:09 AM

[adomokos](https://www.blogger.com/profile/09067995287578229487) said...

_Yup, I was seeing the same bottleneck. Thanks for your comment!_

August 24, 2011 at 8:28 AM
