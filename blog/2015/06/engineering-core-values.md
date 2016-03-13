## Tuesday, June 30, 2015

### [Engineering Core Values](http://www.adomokos.com/2015/06/engineering-core-values.html)

I worked for several startups over the years, but none of them had core values. Only well-established companies have core values, - so is the myth - and even there, they might have been only a cute decoration on the wall, nothing more. Nobody knows about them, nobody lives by them.

At [Hireology](http://www.hireology.com/), we know our company's core values by heart. Every single leadership team meeting on Monday starts out with reciting our company's core values. Here they are:

1.  Pathological Optimism
2.  Create Wow Moments
3.  No A$holes
4.  Eager to Improve
5.  Own the Result

A company's core values might not describe the Engineering Team's core values when it comes to writing software. I felt our team needed something more specific to guide our decisions. Here is the letter I wrote about a year ago, when I announced our Engineering Core Values.

_Team,_

_The company core values define the culture of our company, but it does not describe our engineering values._

_The goal of the Engineering Core Values is to keep our team focused on what makes a product great from our perspective. When you write or review code, when you evaluate a change, try to consider these core values. If we follow these three simple guidelines, I am confident our application will be an outstanding one, and we will succeed._

_Here they are, our Engineering Core Values:_

_1.  Performance is paramount_
_2.  We collect data_
_3.  Trusted code_

_**1\. Performance is paramount**)_
_How did Facebook become the leader of the social networking sites? The answer is simple: speed. While MySpace got more and more popular, it couldn't handle its traffic. Users became increasingly frustrated by seeing the "fail whale". In the early days of Facebook, Mark Zuckerberg did not allow a code change to go into production if the request processing time took longer than 2 seconds (from the book - [The Facebook Effect](http://www.amazon.com/The-Facebook-Effect-Company-Connecting/dp/1439102120)). I'd like to follow that example! We should be looking at our application monitoring software and analyze what we see there. If a page takes longer than 2 seconds to process, we have to work on it during the next Engineering Monday._

_**2\. We collect data**_
_We believe in the value of capturing data. An event never happened if we have no data about it. As our business grows, data will be increasingly important to us. Capturing all the changes would be an overkill, but our data analytics engine will collect more and more data as we grow it. Our Rails app's database will always be an online transaction processing (OLTP) database, it will never store historical data for analytical purposes. The data analytics engine will do that._

_**3\. Trusted code**_
_When we took over the app from contractors the application had no tests at all. Zero! Look how far we have come!! Today we have more than 1600 specs and 89 automated scenarios! Whenever you check in code, make sure the code is something you trust. What does trusted code mean? You feel confident about changing a routine you wrote two weeks ago. Specs and acceptance tests are surrounding your code, you know your change will not have unwanted and unexpected ripple effects. You trust that code, knowing your change will not break two other things later in QA or in Production._

_Thank you, and please keep these Engineering Core Values in mind._

_Attila_

I printed our Engineering Core Values announcement and put it on the wall outside of my office, where all our employees can see it. We need to live by them, it can't be just an ornament on that wall.


POSTED BY ATTILA DOMOKOS AT 9:27 PM


NO COMMENTS
