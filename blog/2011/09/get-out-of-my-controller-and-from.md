## Wednesday, September 7, 2011

### [Get out of my Controller! And from Active Record, too!](http://www.adomokos.com/2011/09/get-out-of-my-controller-and-from.html)

I wrote about [Running Rails Rspec Tests Without Rails](http://www.adomokos.com/2011/04/running-rails-rspec-tests-without-rails.html) a couple of months ago. The examples I used were very high level and focused on stubbing out Rails in my tests in order to achieve rapid feedback.

A couple of months have passed and the topic is getting more and more buzz thanks to [Corey Haines](http://twitter.com/#!/coreyhaines) and [Robert "Uncle Bob" Martin](http://cleancoder.posterous.com/framework-prudence).

I've been getting many questions on how I abstract away from Rails' Active Record, how do I use service objects to lighten up my controllers. I'll try to describe all that in this blog post.

Imagine an application where you have topics that people can comment on. The Active Record models are something like this:

```ruby
class User < ActiveRecord::Base
  # These fields are defined dynamically by ActiveRecord
  attr_accessor :id, :full_name
end

class Discussion < ActiveRecord::Base
  # These fields are defined dynamically by ActiveRecord
  attr_accessor :id, :title, :body, :comments
end

class Comment < ActiveRecord::Base
  # These fields are defined dynamically by ActiveRecord
  attr_accessor :id, :text, :entered_by
end
```

And here is their relationships:

![AR_records_relationships](/resources/2011/09/AR_records_relationships.jpg)

This is pretty simple: Discussion has many comments and a comment was entered by a user. Fantastic!
But what do you do when your customer comes to you and asks you to get not only the comments for a given discussion but she would like to see each user with their comments made on the specific discussion.

Here is the page layout:

![discussion_layout](/resources/2011/09/discussion_layout.jpg)

The Active Record models will perfectly match the view hierarchy on the left. But you are looking at the same data from a different angle on the right hand side.
How are you going to get that data into the view?

Here are some of your options:

1.  Create a view helper that grabs the user's comments from the DB
2.  Add a new field to the User AR model to hold the comments
3.  Use Plain Old Ruby Object (PORO) models on top of AR models and use service objects

Number one is beyond bad. You are actually iterating through the users and hitting the database for every single user to get their comments. BAD! Never do that! It's a very expensive operation: connection is opened, query is executed, AR models are built up from the result set. You already have the data in memory. Use it!

Number two is better but I don't like that either. By adding a field to the User AR model you can do all the data processing in the controller and present that data to the view. This way the view iterates over the users and for each user it iterates over its comments. There is no lookup from the view but you are polluting the AR model with a field that is specific to one particular view. The User AR model is a core object in your application, you want to keep it very clean. Other developers should not be puzzled by an attr_accessor called :comments.

Here is what I'd do: create small model objects that wrap the AR models. Use service objects to populate these POROs and prepare them exactly as the view needs it. Then the view is very simple: it iterates over these model objects and uses their properties.
I call these PORO objects DTOs or [Data Transfer Objects](http://martinfowler.com/eaaCatalog/dataTransferObject.html). They serve custom data from the model to the view.

Here is how a UserDTO looks:

```ruby
module DTO
  class User
    attr_reader :source_object, :id, :full_name
    attr_accessor :comments
    def initialize(source_object)
      @source_object = source_object
      @id = source_object.id
      @full_name = source_object.full_name
    end
  end
end
```

I keep a reference to the original AR model through the @source_object variable. Whatever field I can populate from the source object I do that in the object's initializer. But in our case there is an extra field that does not exist in the source model: comments. This field is declared but not yet populated. The service object will take care of that.

The controller's index action has to do three things:

*   Get the currently viewed discussion from the database
*   Retrieve all the users
*   Find the users' comments under the current discussion

You could place all the code into the controller's action, but you'll have a bloated controller thats very hard to test and the logic will be impossible to reuse.
I use very granular service objects from the controller.

```ruby
# Services used in the app
module Service
  class FindsDiscussion
    def self.for(id)
      # This is very high level
      ::DTO::Discussion.new(Discussion.find(id))
    end
  end

  class FindsUsers
    def self.all
      User.all.map { |user| ::DTO::User.new(user) }
    end
  end

  class SetsComments
    def self.on_users(users, comments)
      # There is no trip to the DB!
      users.each do |user|
        user.comments = comments.select do |comment|
          user.source_object.id == comment.source_object.entered_by
        end
      end
    end
  end
end
```

Look at how small they are! The first and second service looks up data in the database, but the third one is using an in memory lookup. <span style="font-weight: bold; text-decoration: underline;">This is how I am saving the trip to the data store.</span>
[SRP](http://en.wikipedia.org/wiki/Single_responsibility_principle) is strictly followed, these little logic classes are super easy to test and using them from the controller is straightforward:

```ruby
class DiscussionsController < ApplicationController
  attr_reader :users, :discussion

  def index
    @users = Service::FindsUsers.all
    @discussion = Service::FindsDiscussion.for(params[:id])
    Service::SetsComments.on_users(@users, @discussion.comments)
  end
end
```

You are creating many more small classes, but that's OK. They are easy to understand, easy to test and you can use them like little LEGO blocks to construct the logic your controller needs.  

You can find the examples I used in the blog post in [this Gist](https://gist.github.com/1201507).


POSTED BY ATTILA DOMOKOS AT 2:53 PM

#### 15 comments:

[andre goncalves](https://www.blogger.com/profile/11943430191379567050) said...

_DTOs? Seriosly?_
_Why not just a method in User called find_comments_in(discussion)?_
_It would save db's access and avoid all the services and dto stuff. The way suggested is too complex for so small case._

September 7, 2011 at 8:57 PM

[TechScruggs](https://www.blogger.com/profile/00886142686411780764) said...

_This is great and I agree with the principle of skinny controller skinny models, but DTO's doesn't smell quite right here._

_Have you considered using the Decorator Pattern to accomplish this instead? In particular, I have been wanting to use this gem: https://github.com/jcasimir/draper I think this would solve your problem in a more Ruby way._

September 8, 2011 at 12:24 AM

[adomokos](https://www.blogger.com/profile/09067995287578229487) said...

_@andre goncalves: Putting it in the User model is so tempting, isn't it? What do you do when you already have 1700 lines of code in your User model? These 5 lines will certainly not hurt._

_Also, if you put it there: could you do [this](http://twitpic.com/6dsumv)? The concept might seem overhead to you but I have the benefit of fast specs. I can execute a single spec in 0.3 second since I don't need AR and Rails to verify the logic they represent._

September 8, 2011 at 1:18 AM

[adomokos](https://www.blogger.com/profile/09067995287578229487) said...

_@TechScruggs: I did not know about draper, thanks for posting [the link](https://github.com/jcasimir/draper)._

_But using draper your data retrieval logic will be tied to a particular decorator. How can it be reused? With the small services code reuse becomes super simple._

_Rapid feedback is also very important to me. I am curious to know if you could isolate your code from AR and test your code that way._

_We were led to DTOs by using [Liquid](https://github.com/shopify/liquid) in our presentation layer. The DTO models are Liquid drop objects, strained from all non-exposed methods and fields._

September 8, 2011 at 1:33 AM

[shoorik](https://profiles.google.com/shoorik) said...

_Instead of all this, create two tables: comments_by_user and comments_by_discussion and populate them when a comment is entered. Then just pop a couple of "select *" straight from your controller. Problem solved._

September 8, 2011 at 8:46 PM

[Daniel](https://www.blogger.com/profile/12528026446236560291) said...

This comment has been removed by the author.

September 8, 2011 at 10:19 PM

[Daniel](https://www.blogger.com/profile/12528026446236560291) said...

_Thanks for sharing this Attila! I wanted to add - while I think this does add some architectural overhead, I believe the benefit of this becomes more apparent in a large performant application. It provides clear separation of concerns and allows the code to be be easily tested._

_@andre goncalves - while you could add this to the user model, is this really the concern of that model? In most mvc architectures I think people are too fast to throw view concerns, etc on their models. This cuts down greatly on reuse of that code and I think is not a correct architecture - again, you have to way this against your application and domain._

_@shoorik - I hope you were joking about this as now you have data redundancy that makes life miserable when you have to update it and you now have additional relationships to save and worry about - I would then argue that the final outcome is more overhead than what is stated above_

September 8, 2011 at 10:20 PM

[shoorik](https://profiles.google.com/shoorik/) said...

_@Daniel: Not joking at all. In the scenario described, this is the simplest thing I could think of. Comments are usually not editable, and could easily be updated in both places—from a single update_comment() method—if it really was needed. You mentioned performance... can you think of anything more performant than a straight SELECT against flat tables (caching nonwithstanding)?_

September 9, 2011 at 8:05 AM

[Jimmy](https://www.blogger.com/profile/02607990127643246940) said...

_Very interesting I think. I'm going to have a go at this way of working. One thing I can't get my head around though is how, without loading AR, you test whether your queries are correct? Don't you ever use something like this? :_

```ruby
def latest_companies
  Company.where("created_at > ?", 30.days.ago).all
end
```

_How do you test that? I know you can stub/mock it, but that supposes implementation details of the method, which I think a test should not do._

September 15, 2011 at 2:43 AM

[adomokos](https://www.blogger.com/profile/09067995287578229487) said...

_Jimmy:_

_I have both fast running specs without Rails and slow, full-stack Rails specs as well. I'd test the latest_companies routine in the slow running Rspec suite with state verification tests, talking to the database and executing the query. This test would take a long time to execute - in my case it's around 30 sec - but that's OK, I don't run them that often._

_I also have watir-webdriver driven cucumber tests as well to make sure everything is working fine from the outside-in perspective._

September 15, 2011 at 7:31 AM

[Pablo Vivera](https://www.blogger.com/profile/05419421228918428602) said...

_Nice!_
_How do you do with before_filters in the controller code? Full stack or any hack?_

September 26, 2011 at 8:17 AM

[adomokos](https://www.blogger.com/profile/09067995287578229487) said...

_@Pablo Vivera: I'd recommend verifying before_filters using the full stack with Rails._

September 26, 2011 at 8:20 AM

[Ryan Castillo](https://www.blogger.com/profile/18327677236500144678) said...

_Thanks for the sharing this @adomoko! Where are you putting your plain old ruby objects? lib? I tried going that route but finding myself constantly having to restart rails to see the effects. I'm considering just throwing them into the app directory._

August 9, 2012 at 8:50 PM

[Attila Domokos](https://www.blogger.com/profile/09067995287578229487) said...

_@Ryan Castillo: I put them in the 'lib' directory. Sure Rails has to restart to pick up the changes, but if you do proper BDD, this shouldn't slow you down._
_I use cucumber to drive out a scenario and drop down to rspec to develop the objects or - more recently - class functions. I just restart the server and verify that everything works before I check in the code._

_The beauty of this process is the rapid feedback you get while you're test-driving your service objects._

August 10, 2012 at 9:52 AM

[Scott Kroll](https://www.blogger.com/profile/04258506040681810522) said...

_Just an FYI, you don't have to restart the rails app every single time, it's very easy to fix. If it's a Rails engine, I put this in my engine:_

```ruby
config.before_configuration do
  config.autoload_paths += [ root.join('lib') ]
end

config.after_initialize do |app|
  if Rails.env.development?
    ActiveSupport::Dependencies.explicitly_unloadable_constants += %w[
      MyApp::SomeModel MyApp:AnotherModule etc etc
    ]
  end
end
```

_If you just put "MyApp" in above, it breaks the routes when you change a file, so don't do that ,you need to explicitly name each submodule in your lib path, or give it a name other than that of your app._

_If it's NOT an engine, you can put that in the application initialization._

October 12, 2012 at 11:29 AM
