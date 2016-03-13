## Wednesday, May 14, 2014

### [Expects and Promises in LightService](http://www.adomokos.com/2014/05/expects-and-promises-in-lightservice.html)

I received [an issue](https://github.com/adomokos/light-service/issues/30) a few weeks ago for the [LightService](https://github.com/adomokos/light-service) project:

**@adomokos**, I saw an internal tool used at Github that is a lot like LightService. One nice they had was this notion of Promises and Expectations. Used the following way:

```ruby
class FrothMilk
  include LightService::Action
  expects :milk, :cup
  promises :frothed_milk_in_cup

  executed {...}
end
```

You can immediately tell what this action expects from the context, and what this action promises to set in the context. An organizer would find this useful because it can see the chain of actions and check if they're compatible by looking at the expectations and promises.  
What do you think? Would you like this included in LightService?

Oh wow! Github is using something similar to LightService? I'd like to see that! The concept of `expects` and `promises` sounded like a very good idea, I asked the person to submit a Pull Request for it. I found it pretty good when it arrived a few weeks later, but I wanted to polish it up a bit more.

This is how I had been writing actions with LightService:

```ruby
class FooAction
  include LightService::Action

  executed do |context|
    baz = context.fetch :baz

    bar = baz + 2
    context[:bar] = bar
  end
end
```

When I look at this action - and many others I have written over the years - I notice a common pattern: I have to pull items from the context I want to work with, and I need to push the items I need to expose back into it. Although I try to keep actions small, doing this is repeated all over.

I prefer to call `fetch` when I pull an item from the context hash as the call throws a KeyError when the hash does not have the key I am asking for. Using the accessor `baz = context[:baz]` would force me dancing around `nil`s in the code which I try to avoid.

The `expects` macro will help you no matter how you access the value in the hash as the LightService action will throw an `ExpectedKeysNotInContextError` if the key is not found in the context. The action expects the key to be in the context and when it's not provided, the call will blow up right there, no further processing will take place.

The action can be changed this way after the `expects` macro is introduced:

```ruby
class FooAction
  include LightService::Action
  expects :baz

  executed do |context|
    baz = context.fetch :baz

    bar = baz + 2
    context[:bar] = bar
  end
end
```

Having the `expects` macro solved the problem of making sure the key is in the context before execution. However, I wanted to see if this macro could be used to generate accessors in the Context on the fly with the given key. This way I don't need to pull the items from the context one by one, I could just call this "magic" reader that fetches the value for me. With a bit of metaprogramming I was able to do just that. The action can be changed like this:

```ruby
class FooAction
  include LightService::Action
  expects :baz

  executed do |context|
    # Notice how I use `context.baz` here
    bar = context.baz + 2
    context[:bar] = bar
  end
end
```

This is all great, but what about promising what the context hash will have after execution? The `promises` macro does that; when the promised key is not in the context, a `PromisedKeysNotInContextError` is thrown. This is how the action looks after using the `promises` keys:

```ruby
class FooAction
  include LightService::Action
  expects :baz
  promises :bar

  executed do |context|
    # Notice how I use `context.baz` here
    bar = context.baz + 2
    context[:bar] = bar
  end
end
```

Setting the value of "bar" under the `:bar` keyword in the context is repetitive. I tried to see if I could do something similar to the `expects` macro and use it to set value in the context hash besides verifying the key is there. Since blocks in Ruby have well defined closures, peeping in and setting values are not feasible. I chose Context accessors with the promised keys, this way when a value is being set through these, LightService will put it in the context for you.  
The above example can be further simplified:

```ruby
class FooAction
  include LightService::Action
  expects :baz
  promises :bar

  executed do |context|
    context.bar = context.baz + 2
  end
end
```

Now look how we went from dealing with items in the hash explicitly to focusing on the logic itself:

```ruby
class FooAction
  include LightService::Action
  expects :baz
  promises :bar

  executed do |context|
    # No need to fetch item from the context
    # baz = context.fetch(:baz)

    context.bar = context.baz + 2

    # No need to add the item to the context
    # context[:bar] = bar
  end
end
```

Feel free to follow the same refactoring in [this spec](https://github.com/adomokos/light-service/blob/master/spec/action_expects_and_promises_spec.rb).

You could argue that this forces you to use a DSL and it's not saving a line of code for you. I hear you, but keep in mind, the `expects` and `promises` macro does a whole lot more work than just pulling and putting items in and out of the context: it provides an input/output contract for the action.

With the knowledge of what the inputs and outputs of actions are, the time when I can verify the keys in the Organizer pipeline is not far. I should be able to ask the Organizer to verify the order of actions, as actions might need a product from an action that was executed earlier in the Organizer dictated pipeline. Dependency check FTW!


POSTED BY ATTILA DOMOKOS AT 8:21 PM


NO COMMENTS
