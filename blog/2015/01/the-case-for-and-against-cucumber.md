## Thursday, January 15, 2015

### [The Case For and Against Cucumber](http://www.adomokos.com/2015/01/the-case-for-and-against-cucumber.html)

#### The TL;DR version

[Cucumber](http://cukes.info/) has 3 benefits:

1.  Feature Discovery
2.  Automated Acceptance Testing
3.  (Executable) Documentation

In order to use Cucumber successfully within your organization, you need to take advantage of at least 2 of these benefits.

#### The Case For and Against Cucumber

Last week I gave a [talk on Cucumber](http://www.adomokos.com/2014/12/cucumber-aint-bad-but-you-might-be.html) at CodeMash. I was glad to see the roughly 40 people who came to hear me despite being scheduled as one of the last sessions of the conference.

I ended my talk with this very personal story. I had worked in the Microsoft .NET space for 8 years, but I wanted to do something else. I was fascinated by the Ruby community, the innovation, the sharing I had seen among its members. I lived in Cleveland, OH, and there were only a handful of companies working with Ruby at that time.

My ticket to the Ruby World was my familiarity with Cucumber. My good friend - [Joe Fiorini](https://twitter.com/joefiorini) - approached me if I'd be interested in joining their company as a QA Engineer, helping them with QA automation with Cucumber. I was eager to say yes and joined them shortly after.

I wrote the first couple of features, showed them how to write Gherkin. Our test suite happily grew during the first few months of my employment. However, as more and more engineers joined, the case against Cucumber increased. Some of the engineers said they are not against acceptance testing, but those acceptance tests should be written in RSpec and not in Cucumber. Cucumber seemed to them an unnecessary extra layer they did not need.

I felt sad and disappointed. Why my fellow engineers were not seeing the value of Cucumber? What did I do wrong? Should I have spent more time explaining the values of executable documentation? I felt helpless. I asked [Jeff "Cheezy" Morgan](https://twitter.com/chzy) - who knows a lot more about the values and application of Cucumber at various organizations - to have breakfast with me and one of the engineers.

![yours_truly_beechwood](/resources/2015/01/yours_truly_beachwood.png)

We met with Cheezy a few weeks later. I told him: "Cheezy, I think Cucumber is a fantastic tool, it expresses business logic like nothing else. Our company should use it. Please, be the judge here, what are we doing wrong?" Cheezy had one question: "Who is reading your Gherkin?" I said: "Who? It's us, the engineers, and maybe our QA folks." He said: "You should not use Cucumber, you would be better off with just RSpec. Cucumber is a tool for discovering requirements." "Huh?!"

I went back to work feeling a bit disappointed. I used Cucumber for acceptance testing, I did not want to hear about any other tools to do that.

It took me a few months to realize that Cheezy was right. I blindly used Cucumber for its expressiveness, and not for its value as a feature discovery tool.

Fast forward a few years to today and I wonder, why Cucumber or Gherkin is useful to us at [Hireology](http://www.hireology.com/). The answer is clear now: the entire Product, QA and Engineering team values and leverages Cucumber for feature discovery. Product will try writing a couple of scenarios when they brainstorm on a new feature. Those scenarios will be fine-tuned, extended with new ones during our 3 Amigos Meeting (a meeting to flush out feature requirements with Product, QA and Engineering). We just happen to automate those specifications during the development process.

I love how we start thinking about edge-cases well before the development begins with the help of Cucumber and Gherkin. What if the external system is not responding? Where will the user be redirected after a successful form submission? The benefit of doing this kind of planning is a more accurate estimation. Estimating engineering effort of a feature is hard, but if you know what you need to build, then at least you can take a decent stab at it, it won't be a complete swag.

We successfully use Cucumber for (1.) feature discovery and for (2.) automated acceptance testing. Now on to its third benefit: documentation.

Our Cucumber (Gherkin) scenarios are living together with our code base. Looking at them is still hard and not available for everyone at our company. I'd like to make all our features accessible to everybody, from our [CEO](https://twitter.com/adrobins) to all our sales folks. "How does feature X work?" "I don't know, go through the feature document by clicking on this hyperlink."

Have you tried reading up on RSpec's mocking and stubbing functionality. In case you have, I am sure you have visited the [Relish app](https://www.relishapp.com/). Take a look at the page that describes a basic functionality of [RSpec mocking](https://relishapp.com/rspec/rspec-mocks/docs/verifying-doubles/using-an-instance-double). Does it look familiar? Well, there is a Given/When/Then text in there. The most important question: is that useful? Can you learn the tool just by reading through that? That text is coming from RSpec's [own source code](https://github.com/rspec/rspec-mocks/blob/master/features/verifying_doubles/instance_doubles.feature). The RSpec developers packaged up their Cucumber scenarios and presented it in an elegant, nicely formatted, searchable app. Relish app is the prime example of executable documentation.

Publishing our more than 200 scenarios is my next goal. We use Cucumber for feature discovery, automated acceptance testing, we should use it for documentation as well.


POSTED BY ATTILA DOMOKOS AT 9:48 PM


#### 2 comments:

[Aslak Hellesøy](https://www.blogger.com/profile/05425581666293755697) said...

_Great article Attila. Leslie Brooks, who helped Cucumber Ltd organise a BDD Kickstart course (http://kickstartacademy.io/courses/bdd-kickstart) in Atlanta last year told me over dinner he thought of Cucumber as bringing 3 benefits. They were:_

_1) Bridging the *communication* gap between the business, developers and testers by giving them a common language for specifications: Gherkin_

_2) Bridging the *collaboration* gap between developers and testers by giving them a common programming language: The language used for step definitions (Java, Ruby, C#, JavaScript etc)_

_3) Removing inconsistencies between specifications, automated tests and documentation by merging them into a single source of truth: Feature documents written in Gherkin._

_I always find it fasinating to learn about the benefits various teams get from Cucumber and BDD. Most teams I work with take a while before they figure out how to use Cucumber for specification, discovery, collaboration and design. When they do they usually experience significantly fewer defects and shorter lead time for new features._

January 19, 2015 at 10:33 AM

[Dave](https://www.blogger.com/profile/11755320188910974068) said...

_Good piece. My experience is consistent with Cheezy's advice as well as Aslak's comment._

_I suggest the notion of a "case for and against" might cause friction. When we mention a tool early in a discussion, we end up defending the tool. That isn't usually a useful discussion. I prefer to discover what the client wants to achieve, and then explore that space. Tools tend to fall into place naturally, with no need to "sell" them._

_In the context of this topic, I might ask them how they currently achieve the goals of clear communication, collaboration, and useful documentation. Often they aren't achieving those goals at all. Sometimes they achieve the goals by using multiple techniques and tools and maintaining multiple artifacts, which get out of sync (requirements doc, test plan, test/requirements matrix, source code comments, etc.)._
_That's the moment to let them know a tool exists that can help them achieve all those goals with a single artifact. Then you aren't selling anything, you're just helping; and they aren't "resisting," they're just addressing their own needs._

January 23, 2015 at 7:29 AM
