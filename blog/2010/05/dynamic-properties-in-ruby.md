## Sunday, May 23, 2010

### [Dynamic Properties In Ruby](http://www.adomokos.com/2010/05/dynamic-properties-in-ruby.html)

I've been working on this Rails 3 app where users can define what fields they want to have for their track records. The problem I was facing: how can I dynamically add properties to an object?

I have this class:

```ruby
class Track
  attr_accessor :value

  def initialize(value)
    @value = value
  end
end
```

The value field is populated by the application, it'll always be valid JSON data.

After the track object is initialized, I'd like to be able to call "distance" and "running" properties on my track object. The following RSpec code describes what I need:

```ruby
describe Track do
  it "should add distance and running as read-only properties" do
    track = Track.new('{"distance":2,"what":"running in the park"}')
    track.distance.should == 2
    track.what.should == 'running in the park'
  end
end
```

The question is: how can I do that?

First I thought about hooking into the method_missing method in the Track object. It worked, but I was unhappy with the solution. It seemed clumsy and it's not going to provide the best performance either. I exactly know what my methods are going to be called since it'll be set from the value field.

After googling the topic I found the solution: [define_method](http://ruby-doc.org/core/classes/Module.html#M001654).

I had to parse the JSON data which was easy with the [json gem](http://flori.github.com/json/).

```ruby
require 'rubygems'
require 'json'

data = '{"distance":2,"what":"running"}'
parsed_data = JSON.parse(data)
puts parsed_data["distance"] # => 2
```

Once I knew how I'll parse the JSON string, adding the define_method calls to the initialize method was easy.
You can find the final solution here:

```ruby
require 'rubygems'
require 'json'
require 'spec'

class Track
  attr_accessor :value

  def initialize(value)
    @value = value

    parsed_values = JSON.parse(value)
    fields = parsed_values.keys.inject([]) do |result, element|
      result << element.to_sym
    end

    fields.each do |field|
      self.class.send(:define_method, field) do
        parsed_values[field.to_s]
      end
    end
  end
end

describe Track do
  it "should add distance and running as read-only properties" do
    track = Track.new('{"distance":2,"what":"running in the park"}')
    track.distance.should == 2
    track.what.should == 'running in the park'
  end
end
```

There are a couple of things worth mentioning here.
See how elegantly the symbol array is populated from the hash keys on line 12 with the array's "inject" method.
All the magic with dynamic methods happens on line 15\. Please note how define_method message is sent to the object's class reference. All it does is returns the value from the parsed JSON data.

I found this [article](http://www.vitarara.org/cms/ruby_metaprogamming_declaratively_adding_methods_to_a_class) very helpful. I went through the examples and now I know what's going on behind the scene when I use "has_many :addresses" is my Rails models.


POSTED BY ATTILA DOMOKOS AT 4:06 PM
