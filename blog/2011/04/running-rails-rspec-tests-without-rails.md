## Friday, April 15, 2011

### [Running Rails Rspec Tests - Without Rails](http://www.adomokos.com/2011/04/running-rails-rspec-tests-without-rails.html)

I opened up my twitter client this afternoon and I saw "54 Messages, 28 Mentions". I tell you honestly, the first thought I had was: my twitter account had been hacked. Then I started to comb through the messages and I found out what happened. It all started with a tweet from [Joe Fiorini](http://blog.densitypop.com/).

![joe_tweet](/resources/2011/04/joe_tweet.jpg)

We both worked together on a large Rails application. The application was a little light on tests, so I asked the other developers why they are not writing more specs? The answer was all too familiar: "it just takes forever to run them". Yup, Rails had to load up, schema needed to be verified, the entire universe had to be included and 30 seconds later our specs were executed.

We started creating POROs - Plain Old Ruby Objects - as pure services and put their RSpec tests into APP_ROOT/spec/units directory. Our goal was to keep the execution time under or around 2 seconds. Sure, it's easy when you don't have to load Rails controllers or active record models. But what happens when you have to?
This post will explain that.

The controller I used for this example is simple:

```ruby
class TracksController < ApplicationController
  def index
    signed_in_user
  end

  def new
    @track = Track.new
  end

  def create
    feed = params[:track]["feed"]
    @track = TrackParserService.parse(feed)

    unless @track.valid?
      render :action => 'new'
      return
    end

    @track.save_with_user!(signed_in_user)

    render :action => 'index'
  end

  def destroy
    Track.find(params[:id]).destroy

    @user = User.first
    render :action => 'index'
  end

  private

  def signed_in_user
    # No authentication yet
    @user ||= User.first
  end
end
```

The first controller action I wanted to test was "index".

I created the directory structure `APP_ROOT/spec/units/controllers` and saved my file in this directory under the name tracks_controller_spec.rb.

I started out with this code:

```ruby
APP_ROOT = File.expand_path(File.join(File.dirname(__FILE__), "..", "..", ".."))
$: << File.join(APP_ROOT, "app/controllers")

require 'tracks_controller'

describe TracksController do

end
```

You could move the first two lines into a spec_helper, I wanted to keep it here for clarity.

I received the following error:

<div style="color: #c00">`const_missing': uninitialized constant Object::ApplicationController (NameError)</div>

No worries: TracksController inherits from ApplicationController, it's part of my app, I just had to require it.

```ruby
require 'application_controller'
```

And the error:

<div style="color: #c00">`const_missing': uninitialized constant Object::ActionController (NameError)</div>

This was the point where I had to require Rails.

Instead of doing that, I just defined the class myself so the controller was aware of it. I also needed to declare the class method "protect_from_forgery", but I left the implementation blank. Please note that the class declaration is above the require statements.
Here is the entire spec after my changes:

```
APP_ROOT = File.expand_path(File.join(File.dirname(__FILE__), "..", "..", ".."))
$: << File.join(APP_ROOT, "app/controllers")

# A test double for ActionController::Base
module ActionController
  class Base
    def self.protect_from_forgery; end
  end
end

require 'application_controller'
require 'tracks_controller'

describe TracksController do

end
```

Running the spec:

Finished in 0.00003 seconds

<div style="color: #090">0 examples, 0 failures</div>

The first test just ensures that the User active record model will load the first user if the @user instance is nil.

```ruby
describe TracksController do
  let(:controller) { TracksController.new }

  specify "index action returns the signed_in_user" do
    # setup
    user = stub
    User.stub(:first).and_return user

    # execute action under test
    returned_user = controller.index

    # verify
    returned_user.should == user
    controller.instance_variable_get(:@user).should == user
  end
end
```

The test is straightforward. User model is returning a stub - I don't really care what that returned object is, I just check if they're the same object. In the verification part I made sure that the instance variable was set properly. Great that your can check an un-exposed field on a object with a little bit of metaprogramming?

I executed the spec and received the following error:

Failures:

  1) TracksController index action returns the signed_in_user

<div style="color: #C00">    Failure/Error: User.stub(:first).and_return user     NameError:       uninitialized constant RSpec::Core::ExampleGroup::Nested_1::User</div>

Well, I need to require the User model to fix this error. Or do I? I am not using any functionality of the User class - whatever I am using is stubbed out. I just defined the class without any implementation.

This line was added to the spec right above the describe block.

```ruby
class User; end
```

I execute the test and it's all green.

TracksController
  <span style="color: #090">index action returns the signed in user</span>

Finished in 0.00079 seconds <span style="color: #090">1 example, 0 failures</span> 1.26s user 0.28s system 99% cpu <span style="font-weight: bold;">1.546 total</span>

1.5 seconds is not all that bad to run a controller action test.

Let me describe how I tested the "create" action.
Take a look at the controller code above and review what it does. The @track instance is constructed by the TrackParserService class' parse method. Then active record validates it and if the model is invalid the controller's "new" action is rendered.

Here is the spec for that:

```ruby
context "when the model is not valid" do
  it "renders action => 'new'" do
    # define a method for params - TracksController is unaware of it
    controller.class.send(:define_method, :params) do
      {:track => "feed"}
    end

    track = stub(:valid? => false)
    TrackParserService.stub(:parse).and_return(track)

    render_hash = {}
    # hang on to the input hash the render method is invoked with
    # I'll use it to very that the render argument is correct
    controller.class.send(:define_method, :render) do |hash_argument|
      render_hash = hash_argument
    end

    controller.create

    # verify the render was called with the right hash
    render_hash.should == { :action => 'new' }
  end
end
```

I used Ruby's metaprogramming again to set up the params hash. It really doesn't matter what's in it, since I stub out the TrackParserService. The method "render" comes from Rails, I had to define that as well. Please note that I record what the render method was invoked with, this way I can verify that the input hash was correct.
I also had to define - with no implementation - the Track and TrackParserService classes.

When I executed the specs, all of them passed:

TracksController
<span style="color: #090">  index action returns the signed in user</span>
<span style="color: #090">  new action returns an instance of Track</span>
  when the model is not valid
    <span style="color: #090">renders action => 'new'</span>

Finished in 0.00203 seconds
<span style="color: #090">3 examples, 0 failures</span>
bundle exec rspec spec/units/controllers/tracks_controller_spec.rb -fd 1.32s user 0.29s system 99% cpu <span style="font-weight: bold;">1.614 total</span>

You can review the entire example in this [gist](https://gist.github.com/921111).

This code is rough. I just used it to show you how we try to keep our test execution fast. I acknowledge that I am doing some very dangerous stubbing here. However, I have the higher level cucumber tests to protect me against unexpected errors.

I can't tell you what it means to run all of my 150+ specs within 2 seconds. I think it's a little bit of an extra work, but it's well worth the effort!


POSTED BY ATTILA DOMOKOS AT 8:18 AM

#### 24 comments:

[Corey Haines](https://www.blogger.com/profile/06863615802688642075) said...

_Good post. i prefer to just draw most of my stuff out of the controller. However, this is similar to what I'm doing with my other stuff._

_I'd highly recommend getting rid of the bundle exec and just running rspec. This will cut out about 1.5s of time. You'll drop to sub-second!_

April 15, 2011 at 8:28 AM

[adomokos](https://www.blogger.com/profile/09067995287578229487) said...

_Great idea with bundle exec, I'll give that a try. Thanks!_

April 15, 2011 at 8:31 AM

[Steven Harman](https://www.blogger.com/profile/04988908140962422361) said...

_Hmm... it seems like there is an interaction-test approach, w/many of the implementation details of the controller duplicated in the specs. I realize you often don't care about the exact implementation, because you have integration tests elsewhere and/or trust the framework to do its job, but doesn't it make for brittle tests?_

_Dunno, maybe not - it just triggered my Spidey sense as I've seen similar approaches before (in other languages & frameworks, granted) and the result was brittle and unclear tests._

_@Corey,_
_as you draw things out of the controller, what do you do about the model - in particular, ActiveRecord models, where I presume a good deal of behavior would otherwise naturally live? (this is your semi-weekly reminder about that blog post you were going to write :P)_

April 15, 2011 at 9:55 AM

[Corey Haines](https://www.blogger.com/profile/06863615802688642075) said...

_Steven,_

_I pull the behavior out of my models into other objects that wrap the models. I prefer to make the AR objects simple wrappers around the db-access stuff in AR._

_I have a fairly strict rule that controller actions cannot use AR finders or, in fact, interact with AR at all. AR should be accessed within api methods inside your model, not from the outside._

April 15, 2011 at 10:08 AM

[adomokos](https://www.blogger.com/profile/09067995287578229487) said...

_Steven,_

_I'd like to emphasize that this is just a rough prototype demonstrating that controller testing can be done without Rails. I'd have helpers to verify renders, set up params, etc. My goal would be to create Rspec matchers and macros for these tests so they read well and are not brittle. I wanted to describe how I got started with writing controller specs without Rails._

_I'd also like to point out the role of the TrackParserService. We all have seen many Rails controllers where the parsing logic is in the controller action. I moved that logic into the service and I am stubbing it out. In this case the controller is orchestrating the moving pieces and I am just verifying that. Imagine what the spec would be like if it had the parsing code in the action._

_Active Record is responsible for validation, association and CRUD operation only in this tiny app. All other logic is in little service classes, like the TrackParserService._

_But I am doing something similar to what Corey is describing in our big Rails app. Wrapping active record models and hiding them from the rest of the application logic. This abstraction just makes testing a heck of a lot easier._

April 15, 2011 at 10:31 AM

[Steven Harman](https://www.blogger.com/profile/04988908140962422361) said...

_Corey and Adomokos,_
_Awesome, and thanks!_

_Wrapping the real model, from a domain perspective, around an AR Model (which is just an API to persistence, basic validation, etc) makes sense to me._

_Now, if only we could clean up the ambiguity around some of the language - like "model" - to make it easier to discuss these ideas w/o having to qualify everything. meh... I guess that's where a simple Gist comes in._

April 15, 2011 at 10:41 AM

[Scott Walker](https://www.blogger.com/profile/13359065765276977720) said...

_Was thinking about this some this morning and wondering if another valid approach would be to pull out all interesting model functionality into a module that was included into the model? Then for testing you could just include the module into a stub object that subbed whatever methods from the AR model your module depended on..._

_Haven't put any code to this approach, but seemed reasonable at first blush. Thoughts?_

April 15, 2011 at 10:14 PM

[Joakim](https://www.blogger.com/profile/06741642450577678085) said...

_Most files in lib/ should be safe to test without loading rails or mocking anything. Tried a few in our project just now. You might need to load activesupport in some cases._

April 23, 2011 at 2:47 PM

[E.T](https://www.blogger.com/profile/04487344955229981294) said...

_What I do to speed up the tests:_

_1\. use Spork (much start-up time reduced)_
_2\. stub heavy parts (Paperclip, for example)_
_3\. stub models in controller tests_

_By doing these, the most time-consuming parts are the model tests (in which I don't want to use any stubbing)._

_And it takes about 10 seconds (much slower than yours!) to run all tests (over 200)._

_Now I'm thinking about whether I can make Spork and Hydra work together, which may speed up tests further._

May 2, 2011 at 8:23 PM

[adomokos](https://www.blogger.com/profile/09067995287578229487) said...

_E.T.: I just ran my specs: 204 examples, 0 failures in 0.797 seconds._

_I don't test ActiveRecord models at all in these fast running tests - I leave that to the Automated Acceptance Tests with cucumber. I stub out all Rails for my controllers tests, but these controllers are light, writing tests without Rails is very easy for me._

_Most of the application logic is in Rails independent service and model classes just like the TrackParser class used in the example._

May 2, 2011 at 8:57 PM

[Rob Zolkos](https://www.blogger.com/profile/05586336616961885931) said...

_Awesome post. Thanks for this. One question - how do I handle a before_filter in the controller? I have a variable being set in a before_filter for the index action. How would I put that in the faux controller in the spec?_

July 20, 2011 at 5:03 AM

[adomokos](https://www.blogger.com/profile/09067995287578229487) said...

_Rob,_

_somebody has already asked about testing before_filters. I only have a screen shot and not a gist, but here it goes:_

[http://twitpic.com/4y5p16](http://twitpic.com/4y5p16)

Let me know if you have any questions.

July 20, 2011 at 8:16 AM

[Rob Zolkos](https://www.blogger.com/profile/05586336616961885931) said...

_Thanks for the reply @adomokos. That pic helped in understanding how to test that certain before filters are run. What I am after, is (using your controller example) how to test if @user is set in the index action, if it is set in a before_filter._

July 21, 2011 at 8:50 AM

[adomokos](https://www.blogger.com/profile/09067995287578229487) said...

_This might go a little bit too deep - some people might not like it - but you could do this in rspec:_

```ruby
controller.instance_variable_set(:@user, Object.new)
```

_This code sets the private field - @user - to Object.new._
_Since you're testing the index action, you should not call the method defined in the before_filter._

July 21, 2011 at 9:00 AM

[J. B. Rainsberger](https://www.blogger.com/profile/16213943899864372362) said...

_Well, it looks like the Rails/TDD community is finally moving in the direction I expected, but only about five years later. When I took my first steps in Rails, I tried to do what I did in Java: write everything in plain classes, then wire framework extension points to each other and have them delegate everything non-platformy to my plain objects. I wondered why people didn't do that back then; I guess it took this long for test suite execution to become a bottleneck for a critical mass of people._

_So I was ahead of my time, then. Good to know. :)_

August 31, 2011 at 3:42 PM

[Evan Light](https://www.blogger.com/profile/08602063083939292247) said...

_JB: I can only presume that others, including myself, haven't been doing it because not only has test exec time not been as poor until now but also that added decoupling increases developer cognitive load when considering the whole app._

_I, too, found this pattern ironic. Coming from Java years ago, it uses AR much like DAOs: as just another layer for accessing the database rather than a place to put application logic._

_It feels like a more mature way to write apps, yes. However, I feel it is important to recognize that the immaturity, by way of its (perhaps former) simplicity is part of what made it great._

August 31, 2011 at 4:56 PM

[Michael Guterl](https://www.blogger.com/profile/03402602391708279988) said...

_Thanks for sharing your insight into this matter. You mention in the comments that you don't test ActiveRecord in your fast tests, but you rely on acceptance tests. What is your typical ratio between acceptance tests and unit tests on one of these projects?_

_You mention spec/units, do you still keep a spec/models directory and if so, do you hit the database in those specs? I guess I'm looking for a high-level spec folder structure to use as a guideline._

August 31, 2011 at 6:13 PM

[adomokos](https://www.blogger.com/profile/09067995287578229487) said...

_I have a new blog post on how we are abstracting away from Rails controllers and Active Record models. It pertains to this topic, you might want to check it out:_

[Get out of my Controllers! And from Active Record, too!](http://www.adomokos.com/2011/09/get-out-of-my-controller-and-from.html)

September 7, 2011 at 3:12 PM

[Senthil](https://www.blogger.com/profile/18417829128593838828) said...

_Great post adomokos, been doing model tests without Rails the past couple days and been working how to do controller ones. This post def. helped._

_But, I've noticed my Rails models are now, nothing but holder for bunch of include and key (using mongomapper)._

_Do you simply accept your app code will live in lib/ from now on, or create an empty ActiveRecord::Base/MongoMapper::Document module in your tests and stub out AR/MM methods like create!, save! etc._

September 30, 2011 at 3:40 AM

[adomokos](https://www.blogger.com/profile/09067995287578229487) said...

_@Senthil: I have a different folder structure, but yes, most of the application logic lives outside of the controller and AR - in my case._

_Yes, I create empty classes in my tests - check out this [newer blog post](http://www.adomokos.com/2011/09/get-out-of-my-controller-and-from.html), it might be helpful._

October 2, 2011 at 8:45 AM

[Glenn Roberts](https://www.blogger.com/profile/01863353047661764810) said...

_Great stuff Attila. I like your approach_

_Gave it a go myself, and for the trivial cases, it works rather nicely, and of course, super fast._

_However I got caught shaving a yak when trying to stub out the controller respond_to() so I could test my flashes/redirects properly._

_Any advice/gists for that problem?_

March 16, 2012 at 2:27 PM

[Attila Domokos](https://www.blogger.com/profile/09067995287578229487) said...

_@Glenn Roberts: I've been moving most of the logic from controllers and AR into service objects and modules. This way the controller and active record classes are fairly light, there is not much to test there for me any more. It's only "glue code" that connects the request to services._

_Whatever is needed the acceptance tests will most likely cover them._

_Sorry, I have no gist or advice for the problem you are describing, but a bit of stubbing and/or Ruby metaprogramming trick would do it I am sure._

March 22, 2012 at 9:46 AM

[Ryan Castillo](https://www.blogger.com/profile/18327677236500144678) said...

_Thanks for sharing! Now that you extract service objects do you even unit test the extremely skinny controllers anymore?_

August 2, 2012 at 7:56 PM

[Attila Domokos](https://www.blogger.com/profile/09067995287578229487) said...

_@Ryan Castillo: I do very little - if any - unit tests for the controllers. Most of my business logic is in Rails independent service objects and I use cucumber to do acceptance testing._

August 6, 2012 at 8:53 AM
