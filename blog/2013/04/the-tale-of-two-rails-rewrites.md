## Monday, April 8, 2013

### [A Tale of Two Rails Rewrites](http://www.adomokos.com/2013/04/the-tale-of-two-rails-rewrites.html)

#### The TL;DR Version

Watch the video "[So You Think You Need a Rewrite](http://vimeo.com/16285681)" by [Chad Fowler](http://chadfowler.com/) and Rich Kilmer. Kill your old app with a [Strangler App](http://martinfowler.com/bliki/StranglerApplication.html) as you always, constantly release new code.

#### Preface

I worked for a software company that tried to replace its aging software with a new one. Incremental releases were not possible, it was an attempt to do a "Big Bang" rewrite. The customers received the preview 5 years after development started and due to the lack of smooth upgrade nobody was willing to take the brand new software. The economy turned sour, people were laid off and tens of millions of dollars later - after I left the company - the project was canned.

#### JRubyConf in 2010

I attended JRubyConf in Columbus, Ohio in 2010 and watched the talk "[So You Think You Need a Rewrite](http://vimeo.com/16285681)" by [Chad Fowler](http://chadfowler.com/) and Rich Kilmer. I had read Chad's book "[The Passionate Programmer](http://pragprog.com/book/cfcar2/the-passionate-programmer)" before I heard his talk there. Little I knew about rewriting Rails applications at that time, but the main message of their talk sticked with me for a long time: "Never, ever do a big bang rewrite!"

#### The First

Both of the startups I worked for recently had two things in common: the codebase was fairly old (3-5 years) and neither of them had automated tests when I started on the project.

I knew rewriting the entire app was not possible. It would have taken too long, it would have been too expensive, and we wouldn't have been able to release anything to production until the project was done. What we attempted was an "in-place", partial rewrite. Instead of taking the risk of rewriting the entire app we decided to do it in piecemeal: rewriting the app one product at the time.

We kept the database and the Active Record ORM layer intact and rewrote everything above it just for that product. This meant about 15% of the entire app. The new app lived side by side to the old Rails app, the routing engine took the users to the old or to the new product depending on which one they were set up with.

This approach was less risky, but we could not deliver the project until the entire product was rewritten in the app even if it meant just 15% of the codebase. It was still a minified all-or-nothing approach where the code finally made it into production, but deadlines were missed a couple of times.

![first_rails_rewrite](/resources/2013/04/first_rails_rewrite.jpeg)

#### The Second

We have a 2 years old Rails app with about 17k LOC at my current company - [Hireology](http://www.hireology.com/). It has served its purpose and we are now at a point where we are ready to "refactor" the app. The question is how? We have to face several constraints:

*   We only have two engineers
*   We have to develop new features and enhancements
*   We have to deploy our code several times a week
*   We constantly have to increase the code quality

I read about the "[Strangler App](http://martinfowler.com/bliki/StranglerApplication.html)" idiom by Martin Fowler years ago. I really loved this concept: you start with a new app, as it grows it has more and more of the business logic and functionality of the old app and it takes over everything in the end, slowly but surely strangling the old app.

Progressive Insurance built their Claims application this way. They wanted to get off of the mainframe and rewrite the app with ASP.NET. The first thing they did was replacing the outdated black and green terminal UI with similarly looking web page talking to the mainframe. This change went smoothly for the employees who used this application since they barely noticed they were using a web page instead of a terminal screen. The same key bindings worked and it looked just like the terminal app.

Then every two or four weeks a tiny new feature went into production. First just a select box with country codes came from the app server, everything else was fed by the mainframe. Then a form was replaced by .NET web forms. And this went on for a year or more and eventually the same web UI was talking entirely to app servers and SQL Server databases cutting off its reliance on the mainframe, slowly strangling it to "death".

We will try something similar. Since all our clients are asking for our APIs, exposing our business logic through them makes a lot of sense. The old app runs on an older version of JRuby and this brand new Rails API app will be using Rails 4 with Ruby 2.0\. It will be properly tested, when a new version of Rails comes out this app should be running on it shortly after.

We start with a tiny feature: there is one page in the app where we have to load all the different job profiles a client has. The old app receives the request, its controller wraps the params and sends it over to this new API app. The API app is sitting on top of the exact same database as the old Rails app. It collects the open, closed and pending jobs, serializes the collections to JSON. Passes it back to the old app which loads up the models from the received JSON data. It renders the view with data it just received from the API app without talking to the database again.

This feature is small, the users will never notice we started on this project and we will be able to release it in a week or two. If we keep doing this, moving the business logic into our new Rails API app, little by little we will shrink the legacy Rails app to just a dumb presentation engine. I am pretty sure that will be further reduced by building a rich HTML app - using a client side MVC or MVVM framework like [Ember.js](http://emberjs.com/) or Spine.

I was asked how long this will take. Maybe a year. Maybe less, maybe more. It's hard to say without having any kind of baseline data.
We will constantly move logic to our new API app where the code will have supporting automated tests. This API application - if it's structured properly - can feed not only our own app, but other third-party applications as well if they want to integrate our business logic into their own apps.

In the end, everybody wins.

![second_rails_rewrite](/resources/2013/04/second_rails_rewrite.jpeg)</div>


POSTED BY ATTILA DOMOKOS AT 12:57 PM


#### 1 comment:

[Nitish](https://www.blogger.com/profile/16088246505578115469) said...

_Nitish, a student rails developer from India. I am strive to become a better coder by listening to experienced developers._

_I saw your talk "Simple and Elegant Rails Code with Functional Style" and have a couple of questions. Can I have your email please. Tried contacting you on twitter as well._

May 29, 2013 at 4:38 PM
