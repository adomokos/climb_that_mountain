## Saturday, December 6, 2014

### [Cucumber Ain't Bad, But You Might Be Doing It Wrong](http://www.adomokos.com/2014/12/cucumber-aint-bad-but-you-might-be.html)

My talk got selected for [CodeMash 2015](http://www.codemash.org/), and I was invited to speak there in January 2015\. The best way to prepare for the talk is by practicing it a couple of times before the conference. I've worked hard on my message, on my slides, I even watched [Ben Orenstein](https://twitter.com/r00k)'s presentation on "[How to Talk to Developers](https://www.youtube.com/watch?v=l9JXH7JPjR4)".

This past Tuesday I gave the talk's initial version at the [Chicago Ruby User Group](http://www.chicagoruby.org/videos/archives/2014/12/02/cucumber-aint-bad-youre-just-doing-it-wrong/). A good crowd came out to listen to what I had to say about the topic despite the cold, wintery weather. I received a couple of questions, had good conversations with some of the folks afterwards.

Are you unsure about the topic? Here is the pitch for my talk:

The cry from developers is unanimous: “I don’t like Cucumber. My acceptance tests are brittle and hard to maintain!” And in fact, they can be.

This presentation will show you what tools and techniques you should use to write independent, stateless step definitions that can be weaved together to form a testing DSL.

You can review the code examples in this [repo](https://github.com/adomokos/cfp-app/blob/cucumber-aint-bad/features/step_definitions/profile_steps.rb).

And view the slides by clicking on the image below:

[![here](/resources/2014/12/cucumber-aint-bad.png)](https://speakerdeck.com/adomokos/cucumber-aint-bad-you-are-just-doing-it-wrong).


POSTED BY ATTILA DOMOKOS AT 10:07 AM

#### 7 comments:

[George Diaz](https://www.blogger.com/profile/08133938675479670831) said...

_Great talk. I remember you mentioning you used warden to get through the login page Can you send me a gist or example of how you were able to do that?_

December 7, 2014 at 7:49 PM

[Attila Domokos](https://www.blogger.com/profile/09067995287578229487) said...

_Thanks George!_

_Check out this article, it should describe Warden's test hook: https://github.com/plataformatec/devise/wiki/How-To:-Test-with-Capybara_

_We have this in our features/support/warden.rb file:_

```ruby
Warden.test_mode!
World(Warden::Test::Helpers)

After { Warden.test_reset! }
```
_Hth!_

December 7, 2014 at 9:11 PM

[Daniel DeStefano](https://www.blogger.com/profile/12318227761579374289) said...

_Great talk Attila, I was in the front row and gained some great knowledge. I have been learning all this stuff on my own over the last year, so this talk explained a lot of "why we do this" and it was awesome!. However I slightly have forgot some of the points around Uncle Bob's quote at the end... I know that we should not use cucumber to define our test cases, but instead to describe the behavior, but I was wondering if you could re-iterate or elaborate again on this final piece/philosophy._

January 11, 2015 at 8:32 AM

[Attila Domokos](https://www.blogger.com/profile/09067995287578229487) said...

_Daniel, thank you for your comment and for coming to my talk._

_When and why to use cucumber is so important, that I'll blog about it in the coming days._

January 11, 2015 at 8:41 AM

[Daniel DeStefano](https://www.blogger.com/profile/12318227761579374289) said...

_Looking forward to it, really trying to start understanding the economy of QA and all the surrounding concepts/tools. Thanks again!_

January 11, 2015 at 4:07 PM

[Attila Domokos](https://www.blogger.com/profile/09067995287578229487) said...

_Daniel, [here is the blog post](/blog/2015/01/the-case-for-and-against-cucumber.md) I wrote to elaborate on my closing thoughts from the talk. Hope it helps, let me know if you have other questions!_

January 15, 2015 at 9:53 PM

[r11runner](https://www.blogger.com/profile/01265499439221292019) said...

_Thanks a lot for this talk. I've been using Cucumber in my private projects for some months, and I find it really amazing. This talk gave me several hints how to use it more efficiently - and perhaps one day also in larger projects. :-)_

January 17, 2015 at 2:08 PM
