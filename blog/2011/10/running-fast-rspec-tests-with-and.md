## Tuesday, October 11, 2011

### [Running Fast RSpec Tests With and Without Rails](http://www.adomokos.com/2011/10/running-fast-rspec-tests-with-and.html)

So you [got out of the controller and from Active Record](http://www.adomokos.com/2011/09/get-out-of-my-controller-and-from.html) and you're ready to test your services without Rails?

I'll describe how you can trust your fast Rails specs by defining classes safely and ways you can execute them with or without Rails. All of my examples are a continuation of my [previous blog post](http://www.adomokos.com/2011/09/get-out-of-my-controller-and-from.html), I recommend reading that first before you proceed with this one.

The FindsUsers service is very simple:

```ruby
# lib/service/finds_users.rb
module Service
  class FindsUsers
    def self.all
      User.active.map { |user| ::DTO::User.new(user) }
    end
  end
end
```

And this is how I created the first spec without Rails:

```ruby
# spec/units/service/finds_users_spec.rb
APP_ROOT = File.expand_path(File.join(File.dirname(__FILE__), "..", "..", ".."))
$: << File.join(APP_ROOT, "lib")
$: << File.join(APP_ROOT, "spec/units")

module ActiveRecord
  class Base; end
end
class User < ActiveRecord::Base; end

require 'ostruct'
require 'service/finds_users'
require 'factory/for_user'
require 'dto/user'

describe Service::FindsUsers do
  let(:users) { Factory::ForUser.make_two }

  describe "converts the found users to DTO::User" do
    before { User.stub(:active).and_return users }
    subject { Service::FindsUsers.all }

    its(:size) { should == 2 }
    its(:first) { should be_instance_of ::DTO::User }
    its(:last) { should be_instance_of ::DTO::User }
  end
end
```

Please take a look at line 9, where I declared the User class. I need to do this since I don't reference the application's Active Record models in these specs. I don't need to, all I care is that it's some kind of User class that has an :active class method on it.

I also declared a test dummy for ActiveRecord::Base. It doesn't matter what it does, I just want to make sure my User class declaration is as close to the original Active Record model as possible.

When I run the specs they all pass:

<div style="padding-top: 10px;"><span style="color: #090">...</span>
Finished in 0.00223 seconds  
<span style="color: #090">3 examples, 0 failures</span>
rspec spec/units/service/finds_users_spec.rb 0.29s user 0.09s system 96% cpu 0.392 total</div>

It works great, but there are a few lines that will be used in other specs. I move those into the spec/units/spec_helper.rb file.

```ruby
# spec/units/spec_helper.rb
APP_ROOT = File.expand_path(File.join(File.dirname(__FILE__), "..", ".."))
$: << File.join(APP_ROOT, "lib")
$: << File.join(APP_ROOT, "spec/units")
$: << File.join(APP_ROOT, "spec/units/factory")

require 'ostruct'

# Defining an ActiveRecord::Base dummy for models
module ActiveRecord
  class Base; end
end
```

Now my finds_users_spec.rb file is shorter and cleaner:

```ruby
# spec/units/service/finds_users_spec.rb
require 'units/spec_helper'

# ActiveRecord::Base is defined in spec/units/spec_helper.rb
class User < ActiveRecord::Base; end

require 'service/finds_users'
require 'factory/for_user'
require 'dto/user'

describe Service::FindsUsers do
  let(:users) { Factory::ForUser.make_two }

  describe "converts the found users to DTO::User" do
    before { User.stub(:active).and_return users }
    subject { Service::FindsUsers.all }

    its(:size) { should == 2 }
    its(:first) { should be_instance_of ::DTO::User }
    its(:last) { should be_instance_of ::DTO::User }
  end
end
```

Testing the FindsDiscussion service is just as simple:

```ruby
# spec/units/service/finds_discussion_spec.rb
require 'units/spec_helper'

# ActiveRecord::Base is defined in spec/units/spec_helper.rb
class Discussion < ActiveRecord::Base; end

require 'service/finds_discussion'
require 'factory/for_discussion'
require 'dto/discussion'
require 'dto/comment'

describe Service::FindsDiscussion do
  let(:discussion) { Factory::ForDiscussion.make_one }

  describe "looks up a discussion and converts it to DTO" do
    before { Discussion.stub(:find).and_return discussion }
    subject { Service::FindsDiscussion.for 24 }

    it { should be_instance_of ::DTO::Discussion }
  end
end
```

I also need to declare the Discussion class here, so I can stub it out for my service.

They all pass when I execute the entire spec/units suite:

<span style="color: #090">....</span>
Finished in 0.00513 seconds
<span style="color: #090">4 examples, 0 failures</span>  
rspec spec/units 0.28s user 0.09s system 96% cpu 0.387 total  

<span style="font-weight: bold;">BUT WAIT!!</span>  

My User Active Record model has the scope :active that I verify it by loading up Rails in this spec:

```ruby
# spec/models/user_spec.rb

# This spec is using the spec/spec_helper.rb file that loads up Rails with Active Record!
require 'spec_helper'

describe User do
  it { should respond_to :active }
end
```

I run its slow AR spec and a unit spec with this command in the terminal:

```shell
$: time rspec spec/models/user_spec.rb spec/units/service/finds_users_spec.rb
```

It takes a little while - 4 seconds - but everything passes.

<span style="color: #090">....</span>
Finished in 0.04386 seconds
<span style="color: #090">4 examples, 0 failures</span>
rspec spec/models/user_spec.rb spec/units/service/finds_users_spec.rb 3.49s user 0.59s system 100% cpu 4.083 total

But when I change the files around - executing the spec that does not need Rails first and the model spec that uses Rails second:

```shell
$: time rspec spec/units/service/finds_users_spec.rb spec/models/user_spec.rb
```

The specs are executed fast, but the AR model spec failed:

<span style="color: #900">...F</span>
Failures:

  1) User
    <span style="color: #900">Failure/Error: it { should respond_to :active }
      expected #<User:0x00000100a53538> to respond to :active</span>
    # ./spec/models/user_spec.rb:5:in 'block (2 levels) in <top (required)>'

Finished in 0.00248 seconds
4 examples, 1 failure

You might be puzzled why this spec failed, but the explanation is rather simple: in the first case we ran the AR spec first. It loaded up and used the AR User model, the spec passed. Then we opened the User class in our fast spec, stubbed out a method on the User Active Record model and the service spec passed as well.

In the second case we defined our User class for our fast spec, executed the spec and they all passed. Then the AR model spec picked up the already declared User class - which was not the AR User model - and since it did not have the :active scope defined, it failed.

This is exactly what happened when we started executing all our specs - both non-Rails and Rails specs together - on our build server. The spec execution order was different on CentOS and different on our local OS X development environment. Everything passed locally, but had quite a few errors on the build server. We obviously had to find a solution.

First of all, redefining classes all over the specs just wasn't a good idea. I moved all my redefined classes into spec/units/spec_helper.rb from the different specs.

```ruby
# spec/units/spec_helper.rb
APP_ROOT = File.expand_path(File.join(File.dirname(__FILE__), "..", ".."))
$: << File.join(APP_ROOT, "lib")
$: << File.join(APP_ROOT, "spec/units")
$: << File.join(APP_ROOT, "spec/units/factory")

require 'ostruct'

# Defining an ActiveRecord::Base dummy for models
module ActiveRecord
  class Base; end
end

# I moved the redefined classes here
class User < ActiveRecord::Base; end
class Discussion < ActiveRecord::Base; end
```

Look at line 15 and 16 in the spec_helper, the redefined classes are now in one single place as opposed to having them scattered all over the specs.
I ran the fast specs without Rails again and they were all passing.

All I had to do to get the specs passing regardless of file order was including the Rails-aware spec_helper into the spec/units/spec_helper.rb file that loaded up Rails with the real Active Record models (line 2 below):

```ruby
# Including the full stack spec_helper, loads the AR models with Rails
require 'spec_helper'

# spec/units/spec_helper.rb
APP_ROOT = File.expand_path(File.join(File.dirname(__FILE__), "..", ".."))
$: << File.join(APP_ROOT, "lib")
$: << File.join(APP_ROOT, "spec/units")
$: << File.join(APP_ROOT, "spec/units/factory")

require 'ostruct'

# Defining an ActiveRecord::Base dummy for models
module ActiveRecord
  class Base; end
end

# I moved the redefined classes here
class User < ActiveRecord::Base; end
class Discussion < ActiveRecord::Base; end
```

Now when I execute the specs starting with the fast spec first it loads up Rails and in about 4 seconds I know that all the specs are passing regardless of what file order was used at execution time.

This change alters the User and Discussion class declarations as well. They are not redefined classes any more, they are open classes in the execution context. I am not modifying their behavior, I am just opening up the classes and leaving them unchanged.

A script in the build process can change the spec/units/spec_helper.rb file to include the full stack spec_helper.rb file.

This might seem like a lot of voodoo for some, but I am working on a fairly large Rails app and it takes about 23 seconds to execute one spec with Rails. I believe with just a little bit of meta programming trick you can enjoy very fast feedback loop making you more effective at writing software.


POSTED BY ATTILA DOMOKOS AT 8:50 AM


#### 8 comments:

[e3matheus](https://www.blogger.com/profile/09617198683313524852) said...

This comment has been removed by the author.

February 16, 2012 at 10:18 AM

[wulftone](https://www.blogger.com/profile/18444014434333094811) said...

_I think I have this right, but it would be nice to get some verification: when you run tests in isolation (one at a time, perhaps with something like the watchr gem), you don't include the `spec/spec_helper.rb` file in your `spec/units/spec_helper.rb` file, but when you deploy to your staging machine (or wherever), you add the `spec/spec_helper.rb` file as stated in the last example._

_It seems pretty obvious, I know, but the article didn't really highlight that too well at the end there. Otherwise, nice job! I like this abstraction railsy people are doing these days._

February 25, 2012 at 9:14 PM

[Édipo Luis Féderle](https://www.blogger.com/profile/08767810142998909384) said...

_Hi, very nice post._

_I have a question, I trying follow this post to a project. My question is about the attributes on the classe User. When I try run spec:_

_undefined method `name=' for #_

_This because I have on spec:_

```ruby
class Client < ActiveRecord::Base
end
```
_something wrong here?_

_Thanls_

March 12, 2014 at 9:23 PM

[Attila Domokos](https://www.blogger.com/profile/09067995287578229487) said...

_Édipo Luis Féderle: You need to stub out the call to `name=` on the class or instance of `Client`. This technique involves heavy stubbing or mocking._

March 12, 2014 at 9:27 PM

[Édipo Luis Féderle](https://www.blogger.com/profile/08767810142998909384) said...

_Hi Attila, sorry, class User < ActiveRecord::Base, not Client._

March 12, 2014 at 9:29 PM

[Édipo Luis Féderle](https://www.blogger.com/profile/08767810142998909384) said...

_Right, so I need to stub all attributes of my model?_

March 12, 2014 at 9:30 PM

[Attila Domokos](https://www.blogger.com/profile/09067995287578229487) said...

_Yes, that's how you don't rely on ActiveRecord and can speed up your tests that way._

March 12, 2014 at 9:33 PM

[Édipo Luis Féderle](https://www.blogger.com/profile/08767810142998909384) said...

_Understood. Just on last thing, the part of attributes stubbing was omitted from the example of the post?_

_Thanks for the quick replies._

March 12, 2014 at 9:37 PM
