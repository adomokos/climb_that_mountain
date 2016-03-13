## Tuesday, March 4, 2014

### [Group By In Plain Ruby](http://www.adomokos.com/2014/03/group-by-in-plain-ruby.html)

I had to work with this code recently, and it just did not feel right:

```ruby
class FindsEmployeesByLocations
  def self.from(employee_locations=[])
    locations = Hash.new
    employee_locations.each do |location|

      if !locations.key?(location.city)
        locations[location.city] = Array.new;
      end

      locations[location.city].push(location);
    end

    return locations
  end
end
```

A few things stood out:

1.  There is a locations hash
2.  The `each` iterator adds items to the hash based on a condition
3.  Returns the locations hash

Declaring a collection and adding items to it by iterating over the source collection is an infection that people with imperative languages have. With a little bit of functional thinking I was certain this code can be simplified.

Just to be on the safe side I added specs around this before I refactored it:

```ruby
describe User do
  ...

  context "when the employee locations collection is empty" do
    specify "employees by locations is empty" do
      expect(FindsEmployeesByLocations.from([])).to be_empty
    end
  end

  context "when there are 3 employees with 2 locations" do
    subject { FindsEmployeesByLocations.from(employee_locations) }

    it "has only 2 locations" do
      expect(subject.keys.count).to eq(2)
    end

    specify "first locations has 2 users" do
      expect(subject.keys.first).to eq("Chicago")
      chicago_location = subject["Chicago"]

      expect(chicago_location.count).to eq(2)
      expect(chicago_location.map(&:user).map(&:name)).to eq(%w(John Kate))
    end

    specify "last locations has 1 user" do
      expect(subject.keys.last).to eq("New York")
      new_york_location = subject["New York"]

      expect(new_york_location.count).to eq(1)
      expect(new_york_location.first.user.name).to eq("James")
    end

  end
end
```

Everything passed, I was ready to play with the code.

First I thought I need to use the `reduce` method. I tried that, but it did not work. Then I started to think about what this piece of code is doing. I realized it's grouping entities based on keys. I was certain Ruby has a `group_by` method and I was right: [there it was](http://ruby-doc.org/core-2.1.0/Enumerable.html#method-i-group_by).

All that code can be replaced with one beautiful line:

```ruby
class FindsEmployeesByLocations
  def self.from(employee_locations=[])
    employee_locations.group_by(&:city)
  end
end
```

The method, or even this class is unneeded for calling something as simple and elegant as Ruby's `group_by` method.

Be skeptical when you see an empty collection followed by an iterator. If you look closely, you will see that the code can be simpler by using one of Ruby's enumerable functions.

You can find the entire example in [this gist](https://gist.github.com/adomokos/9361006).


POSTED BY ATTILA DOMOKOS AT 10:20 PM


NO COMMENTS
