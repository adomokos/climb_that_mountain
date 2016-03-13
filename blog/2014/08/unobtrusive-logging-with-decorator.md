## Thursday, August 21, 2014

### [Unobtrusive Logging with a Decorator](http://www.adomokos.com/2014/08/unobtrusive-logging-with-decorator.html)

I've recently added [logging](https://github.com/adomokos/light-service#logging) to LightService. The organizers pipe log messages to whatever you define as your logger.

```
I, [LS] - calling organizer TestDoubles::MakesTeaAndCappuccino
I, [LS] - @keys in context: :tea, :milk, :coffee
I, [LS] - executing TestDoubles::MakesTeaWithMilkAction
I, [LS -   expects: :tea, :milk
I, [LS] -   promises: :milk_tea
I, [LS] -     keys in context: :tea, :milk, :coffee, :milk_tea
I, [LS] - executing TestDoubles::MakesLatteAction
I, [LS] -   expects: :coffee, :milk
I, [LS] -   promises: :latte
I, [LS] -     keys in context: :tea, :milk, :coffee, :milk_tea, :latte
```

This blog post tells you the story of how I added logging to this [gem](https://github.com/adomokos/light-service) I maintain.

What do you think of this code?

```ruby
class WithReducer
  attr_reader :context

  def with(data = {})
    @context = LightService::Context.make(data)
    self
  end

  def reduce(*actions)
    raise "No action(s) were provided" if actions.empty?
    actions.flatten!

    actions.reduce(context) do |context, action|
      action.execute(context)
    end
  end
end
```

It's not that hard to figure out what's going on in this object. It has 2 methods, `#with` and `#reduce`. The `#with` method is simple, it creates a Context object through the factory method and returns `self` for chaining. `#reduce` takes an arbitrary number of actions and reduces them by calling their `#execute` method.

Ok, let’s add logging around this:

```ruby
class WithReducer
  attr_reader :context

  def with(data = {})
    logger.info("[LS] - calling organizer <#{organizer.to_s}>")

    @context = LightService::Context.make(data)

    logger.info("[LS] - keys in context: #{extract_keys(decorated.context.keys)}")

    self
  end

  def reduce(*actions)
    raise "No action(s) were provided" if actions.empty?
    actions.flatten!

    actions.reduce(context) do |context, action|
      result = action.execute(context)

      logger.info("[LS] - executing <#{action.to_s}>")
      if defined? action.expects and action.expects.any?
        logger.info("[LS] - expects: #{extract_keys(action.expects)}")
      end
      if defined? action.promises and action.promises.any?
        logger.info("[LS] - promises: #{extract_keys(action.promises)}")
      end
      logger.info("[LS] - keys in context: #{extract_keys(context.keys)}")

      result
    end
  end

  private

  def extract_keys(keys)
    keys.map {|key| ":#{key}" }.join(', ')
  end
end
```

This is the exact same core logic as the one in the first example, except it got polluted with logging. This code works, but seeing what the two instance methods do is not obvious, far from simple. What's wrong here? First, two separate concerns (logging and the core functionality) got squashed into one. Second, when the logging logic changes the underlying core logic will be touched violating the Single Responsibility Principle.

Logging and the core functionality of this code should be separated out. The logging needs to wrap around (decorate) the core functionality. It should look something like this:

![decorator](/resources/2014/08/decorator.png)

A [decorator](http://en.wikipedia.org/wiki/Decorator_pattern) - the WithReducerLogDecorator in this case - publishes the exact same interface as the object it decorates, it has the `with` and `reduce` methods. I started with those when I went about refactoring this:

```ruby
class WithReducerLogDecorator
  def with(data = {})
  end

  def reduce(*actions)
  end
end
```

Then I "wrapped" the original logic with this decorator. The log decorator delegates the calls to the wrapped object, which is `WithReducer` in this case:

```ruby
class WithReducerLogDecorator
  attr_reader :decorated

  def initialize(decorated = WithReducer.new)
    @decorated = decorated
  end

  def with(data = {})
    decorated.with(data)
  end

  def reduce(*actions)
    decorated.reduce(*actions)
  end
end
```

At this point it does not matter which `WithReducer` class I'd use. Using the original one or the decorated should produce the exact same functionality.
I started adding logging around the decorated methods, refactoring the `#with` method was first:

```ruby
class WithReducerLogDecorator
  attr_reader :decorated, :organizer

  def initialize(decorated = WithReducer.new, organizer)
    @decorated, @organizer = decorated, organizer
  end

  def with(data = {})
    logger.info("[LS] - calling organizer <#{organizer.to_s}>")

    decorated.with(data)

    logger.info("[LS] - keys in context: #{extract_keys(decorated.context.keys)}")
    self
  end

  ...
end
```

Adding logging around the `#reduce` method was a bit harder. It reduces the provided actions by calling their execute method. There is no easy way to hook into that process just yet. However, calling the provided block within the reduce block opens up the routine for extension ([Open/closed principle](http://en.wikipedia.org/wiki/Open/closed_principle)). I changed the original reduce method like this:

```ruby
class WithReducer
  ...

  def reduce(*actions)
    raise "No action(s) were provided" if actions.empty?
    actions.flatten!

    actions.reduce(context) do |context, action|
      result = action.execute(context)
      # ::: Invoke the provided block :::
      yield(context, action) if block_given?
      result
    end
  end
end
```

The logging logic can now wrap around the `#reduce` method like this:

```ruby
class WithReducerLogDecorator
  ...

  def reduce(*actions)
    decorated.reduce(*actions) do |context, action|
      # ::: All this code is executed within the decorated reduce block :::
      logger.info("[LS] - executing <#{action.to_s}>")
      if defined? action.expects and action.expects.any?
        logger.info("[LS] - expects: #{extract_keys(action.expects)}")
      end
      if defined? action.promises and action.promises.any?
        logger.info("[LS] - promises: #{extract_keys(action.promises)}")
      end
      logger.info("[LS] - keys in context: #{extract_keys(context.keys)}")
    end
  end

  private

  def extract_keys(keys)
    keys.map {|key| ":#{key}" }.join(', ')
  end
end
```

And with that I wrapped the [original](https://github.com/adomokos/light-service/blob/897af83462377adfa22e55f8fba40135f53b86e9/lib/light-service/organizer/with_reducer.rb) `#with` and `#reduce` methods with logging from outside with a [decorator](https://github.com/adomokos/light-service/blob/897af83462377adfa22e55f8fba40135f53b86e9/lib/light-service/organizer/with_reducer_log_decorator.rb). I separated the two concerns: the core logic and the logging around it. I only need to touch the decorator object when the logging logic needs to be changed and I don't even need to look at the logging code when the core logic changes.

I learned about these concepts 9 years ago when I used [Aspect Oriented Programming](http://en.wikipedia.org/wiki/Aspect-oriented_software_development) (AOP). I was fascinated by Around Advices, where logging was applied around a routine, beautifully separating the concerns.


POSTED BY ATTILA DOMOKOS AT 10:07 PM


NO COMMENTS
