## Monday, June 6, 2011

### [Spec it or not?](http://www.adomokos.com/2011/06/spec-it-or-not.html)

I had a conversation with my friend [Joe Fiorini](http://twitter.com/#!/joefiorini) a couple of days ago about this particular code:

```ruby
module ApplicationHelper
  def gravatar_for(email)
    image_tag Utils::Gravatar.for(email), size: "30x30"
  end
end
```

He showed me this code and my first question was: "Did you write tests for it?" He said: "Why? This is a simple method. What should I test?"
We both agreed that there isn't much that could go wrong there.

But:

1.  Writing a spec against this code is quick and easy.
2.  I'd much rather look at a spec documentation that describes this code than the code itself.
3.  A spec describes intent.

It took me about 4 minutes to write this spec:

```ruby
require 'units/spec_helper'
require 'application_helper'

describe ApplicationHelper do
  context "#gravatar_for(email)" do
    specify "provides an image tag with Gravatar url" do
      dummy = Object.new.extend ApplicationHelper # It's a module

      Utils::Gravatar.stub(:for).and_return("some url")
      dummy.stub(:image_tag).and_return("some image tag")

      dummy.gravatar_for("some_email").should == "some image tag"
    end
  end
end
```

I am not using mocks, I like to keep my tests "loose". I want to make sure when I call this helper I get the result I expect through the canned responses provided by the stubs.

The image_tag helper needs a url. This url is provided by a utility class method, stubbed out on line 9.

It's important to mention that I am not testing Rails' [image_tag](https://github.com/rails/rails/blob/master/actionpack/lib/action_view/helpers/asset_tag_helper.rb#L344) helper. I leave that to the Rails core developers and contributors. I want to make sure that the method image_tag is recognized in the given context and it returns the string I expect.

Once I execute the spec, this is the output:

<pre style="color: #707070">ApplicationHelper
  #gravatar_for(email)
    <span style="color: #090">provides an image tag with Gravatar url</span>

Finished in 0.00151 seconds
<span style="color: #090">1 example, 0 failures</span>
</pre>

What did I mean by "communicating intent"?
Let's say a new developer comes to the team and decides to change the gravatar_for method like this:

```ruby
def gravatar_for(email)
  #image_tag Utils::Gravatar.for(email), size: "31x30"
  url_for some_kind_of_named_route(email)
end
```

As soon as he runs the spec the error is obvious:

<pre style="color: #707070"><span style="color: #C00">F</span>
Failures:

  1) ApplicationHelper#gravatar_for(email) provides an image tag with Gravatar url
     <span style="color: #C00">Failure/Error: dummy.gravatar_for("some_email").should == "some image tag"
     NoMethodError:
       undefined method `some_kind_of_named_route' for #<Object:0x01008e8838></span>
     # ./app/helpers/application_helper.rb:7:in `gravatar_for'
     # ./spec/units/helpers/application_helper_spec.rb:12:in `block (3 levels) in <top (required)>'

Finished in 0.00139 seconds
<span style="color: #C00">1 example, 1 failure</span>
</pre>

To me this is a good warning sign that suggests the following: "You can change the method behavior, but the original developer meant it the way it's described in the spec. Now please fix the spec so it's green again. Oh, and make sure you run the full stack automated acceptance tests before you push your code."

Knowing metaprogramming, stubbing and mocking makes it easy to write specs.
I would have a totally different opinion if it took a lot more code and ceremony to do it.


POSTED BY ATTILA DOMOKOS AT 7:48 AM
