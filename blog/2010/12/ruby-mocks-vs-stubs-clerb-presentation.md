## Sunday, December 26, 2010

### [Ruby Mocks vs Stubs - CleRB Presentation](http://www.adomokos.com/2010/12/ruby-mocks-vs-stubs-clerb-presentation.html)

I talked to somebody a while ago about a line of code that had tremendous beauty in my eyes:

```ruby
setup_response = stub(:token =>'xyz_some_token')
```

This might seem strange to someone new to dynamic languages. The stub object returns the string 'xyz_some_token' when the "token message" is sent to it. I have no idea - and I don't really care - what type of object it is. What really matters is that is has a canned response for the "token message".

He suggested that I should do a talk on this. I submitted the idea to our [local Ruby user group](http://www.meetup.com/ClevelandRuby/) and [Michael "Doc" Norton](http://www.docondev.com/) - the user group organizer - asked me to present it.

Preparing for a presentation is hard - takes time and effort - but I learned so much from it that I would and I will do it again!

I used the Order - Warehouse example from [Martin Fowler](http://martinfowler.com/)'s [Mocks Aren't Stubs](http://martinfowler.com/articles/mocksArentStubs.html) writing. I also wrote a Twitter client where I used mocking/stubbing in the controller tests and [Fakeweb](http://fakeweb.rubyforge.org/) to stub out http calls from Cucumber.

After the talk we had the following conclusions:

* Although Stubs are not as sophisticated as mocks, they are really powerful and reflect clean code
* Try to use stubs over mocks
* Abused mocking could be a code smell -&gt; introduce abstraction and use stubs

The examples from the talk are in [my github repository](https://github.com/adomokos/mocks_vs_stubs).

[Mock vs Stubs CleRB Presentation](http://www.slideshare.net/AttilaDomokos/mock-vs-stubs-clerb-presentation)

I'd like to thank [Joe Fiorini](http://joefiorini.com/) for meeting with me a couple of days before my talk. He had great ideas that I used in my presentation. Thanks for it!


POSTED BY ATTILA DOMOKOS AT 7:51 PM
