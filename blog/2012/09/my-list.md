## Saturday, September 8, 2012

### [My List](http://www.adomokos.com/2012/09/my-list.html)

I was asked a couple of weeks ago to come up with a list of engineering guidelines at my [current employer](http://dimpledough.com/). Not something we want to enforce but an initial collection that all the team members could agree upon and would follow. Here is the list I provided:

*   Leave the code better than you found it
*   Simplicity rules (aka YAGNI)
*   Try to do TDD, or at least cover the code you write with unit tests
*   Develop automated acceptance tests
*   Strive for "simple" code

**→ Leave the code better than you found it**

Uncle Bob twisted [the boy scout rule](http://en.wikiquote.org/wiki/Robert_Baden-Powell) - "Leave this world a little better than you found it" - a bit in his book: [Clean Code](http://www.amazon.com/Clean-Code-Handbook-Software-Craftsmanship/dp/0132350882/ref=sr_1_1?ie=UTF8&qid=1346446375&sr=8-1&keywords=clean+code).
When you see a variable name "ou" change that to "organization_user" so next time when you look at it you'll know what that variable represents. Or you can just add tests to a routine that is untested or not covered with any kind of test. This is a great way to get started with open source software, by the way.

**→ Simplicity rules aka YAGNI**

Oh, it's so hard to resist creating the next gem, npm package or framework. But be lazy! When your customers want a feature think about what is the least amount of functionality you need to build? Do that first and check usage. If you see that this feature is heavily used by your end users invest in it. Make it more rich, maybe a bit more responsive. But first think like you're participating in a [startup weekend](http://startupweekend.org/) every day trying to get your product out there as fast as you can so your idea is validated on the field.

**→ Try to do TDD, but at least cover the code you write with unit tests**

[Avdi Grimm](http://about.avdi.org/)'s brilliant [tweet](https://twitter.com/avdi/status/127206092055920640) sums this up for me:

![avdi_tweet](/resources/2012/09/avdi_tweet_smaller.jpg)

I know TDD is hard for newcomers. In fact, I've only found very few people doing it at a highly skilled level. Most of the code I inherited from other developers are without any tests.
If you don't do TDD, please cover your work with tests. Your test coverage will not be even close to the level of code written with TDD, but at least you'll help the next developer who has to maintain your code.

**→ Develop automated acceptance tests**

I've been frequently asked to define the difference between unit tests and acceptance tests? I found the best answer in [The Cucumber Book](http://pragprog.com/book/hwcuc/the-cucumber-book):

"Unit tests ensure you build the thing right, while acceptance tests ensure you build the right thing".

I found that [Gherkin](https://github.com/cucumber/cucumber/wiki/Gherkin) helps us (BAs, QAs and Developers) describe a feature in a way that is easily understood by everyone. Automating them with cucumber is a bit more work, but it's well worth the effort.
I follow the [GOOS](http://www.amazon.com/Growing-Object-Oriented-Software-Guided-Tests/dp/0321503627) model and I stub out external dependencies in my units tests. Since most of the code I write these days is in dynamic languages, having full stack tests is literally priceless.

**→ Strive for "simple" code**

I wrote about the benefit of simple code in my [previous blog post](http://www.adomokos.com/2012/07/just-make-it-small-please.html). It's not only easy to understand but easy to reuse and test. Have you spent time understanding what a 60 line method does obscured with nested conditionals and iterators? I have. It's frustrating to me and very expensive for my employer.

I am sure I am not stating anything new here. In fact, some of the items on this list might sound like a broken record already. But it's good to write them down and look at them once in a while.

What would be the items on your list?


POSTED BY ATTILA DOMOKOS AT 7:19 PM

NO COMMENTS
