## Tuesday, June 19, 2012

### [Frequent Job Change?](http://www.adomokos.com/2012/06/frequent-job-change.html)

I've been making a living writing software for the past 12 years (and I am still not in management yet :-). I've worked for small startups and Fortune 500 companies with more than 2,000 people just in IT.

All together I have worked for 8 different companies which averages out at 1.5 year spent at each, the longest being 3 years at one place:

![work_history_graph](/resources/2012/06/work_history_graph.png)

I interviewed with a small startup a couple of years ago and I still remember how disappointed the interviewer was when he learned about my job history. He asked me: "So can I expect you to work for me a year, or two if I am lucky?" What should I have said? We clearly were not a match.

Whenever I start a new job the most important thing for me is to be productive from day one. I am sure I won't understand all the business logic right away. It might take a few months to pick it up, but I strive to add value as soon as possible. If it's only by adding automated tests, it doesn't matter: I contributed.

I have clear goals of what I want to learn or practice in the future. But there were times when I picked up skills by chance.

I had to take courses on data warehousing and [dimensional modeling](http://en.wikipedia.org/wiki/Dimensional_modeling) when I worked for a large corporation. First I was not very interested in it, but as I learned more about the topic I realized that you can't use your [OLTP](http://en.wikipedia.org/wiki/Online_transaction_processing) database for data mining and reporting: you need to lay out data in a different format to make it efficient.

My views have changed on [progressive enhancement](http://en.wikipedia.org/wiki/Progressive_enhancement) thanks to the bright UX folks I had the pleasure to work with. I did not like the idea of making a web app "gracefully degrade" for a more solid architecture but they pushed me to develop applications that way. I thought this is crazy: why would I build the same logic twice with- and without JavaScript? But that's nonsense. If you architect your application properly, building your web app without JavaScript and adding it after is not all that hard. Besides, you don't have to write much more code.  
Of course I wouldn't consider progressive enhancement if I was building a one-page web app, like the excellent [Trello](https://trello.com/).

It's been only four months since I started working for my current employer and look what we have accomplished already:

*   We moved our source control repository to git and Github from Subversion, so it's easier for us to release code into different environments.
*   We began carving out business logic and putting it in a business logic gem, so we can easily share logic between different Rails apps.
*   We can drop, recreate and seed our development database in one, single script (yeah, I know, that did not work when I started), so our acceptance tests will have a set database state before they are executed.
*   Introduced [Gherkin](https://github.com/cucumber/cucumber/wiki/Gherkin) to the team so we make sure we build the right thing.
*   We are automating acceptance tests using Cucumber with [capybara-webkit](https://github.com/thoughtbot/capybara-webkit) so our regression cycle is shorter.
*   We set up a build process using [Jenkins](http://jenkins-ci.org/) so we can run both unit- and acceptance tests after each push to Github.
*   We started building new features with progressive enhancement so our application will graceful degrade on devices that can't handle JavaScript.

Dear Employer: please don't be afraid of hiring those "job-hopping" individuals. Just make sure you are selecting the right person and <span style="font-weight: bold;">let</span> them produce value from the very beginning. They might have picked up skills - you will need - here and there. When they write tested and clean code they can leave the job any day knowing that somebody else should be able to jump in and continue the work they've started.


POSTED BY ATTILA DOMOKOS AT 9:02 PM

#### 1 comment:

[Josh Schramm](https://www.blogger.com/profile/04179432092617833919) said...

_Honestly I think job hopping is becoming more and more common in our field. I've been doing this about 7 years and I'm on job 3, so I have a bit more stable track record than you but not by much :-)_

_Also just about everyone I know is the same way. I only know 1 or 2 people in IT who are still in the same position as they were when they graduated from college 7 years ago._

June 19, 2012 at 9:20 PM
