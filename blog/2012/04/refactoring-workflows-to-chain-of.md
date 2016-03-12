## Monday, April 9, 2012

### [Refactoring Workflows to Chain of Actions](http://www.adomokos.com/2012/04/refactoring-workflows-to-chain-of.html)

Everything we do in life is a series of actions. Just to get to work I need to wake up, eat something, brush my teeth and drive to work. Or when the data is sent to the server your code has to validate the user input, it has to create a new object with the attributes, this new data has to be saved and a response data needs to be generated in JSON format if your request happens to be an Ajax request.

When I write code for a series of tasks I start out with a "coordinator" object that has several private methods internally. The main and public method orchestrates the calls for the private methods. The tests around this object start out pretty nice, but as the complexity grows I need to stub more and more external objects. The complexity of the tests are soon becoming indicators of the worsening design and I need to start pulling out objects before the whole thing turns into an [iceberg class](http://deviq.com/iceberg-class).

The example I use in this writing is a simple one: The "[Dude](http://en.wikipedia.org/wiki/The_Big_Lebowski)" wants to go to the park to enjoy the nice weather. He has to go through several steps to get there:

1.  Leave the house
2.  Close the door
3.  If he has a car - Jump in the car
4.  If he has a car - Drive to the park
5.  If he has a car - Park the car
6.  If he has NO car - Jump on the bike
7.  If he has NO car - Ride to the park
8.  If he has NO car - Park the bike
9.  Enter the park
10.  Take a walk

All my examples are in CoffeeScript. I use CS for brevity and for its concise format.

In my example the coordinator object is called "GoesToThePark". It interacts with the House, Car, Bicycle and Park models like this:

![sequence_no_actions](/resources/2012/04/sequence_no_actions.png)

And all this described in CoffeeScript:

```coffeescript
House = {
  leave: (dude) ->
    'leaves the house'
  closeTheDoor: (dude) ->
    'closes the door'
}
Car = {
  jumpIn: (dude) ->
    'jumps in the car'
  driveToThePark: (dude) ->
    'drives to the park'
  parkTheCar: (dude) ->
    'parks the car'
}
Bicycle = {
  jumpOn: (dude) ->
    'jumps on the bike'
  rideToThePark: (dude) ->
    'rides to the park'
  parkTheBike: (dude) ->
    'parks the bike'
}
Park = {
  enter: (dude) ->
    'enters the park'
}

class GoesToThePark
  constructor: ->
    @messages = []

  toEnjoyTheWeather: (dude)->
    @messages.push House.leave(dude)
    @messages.push House.closeTheDoor(dude)
    if dude.hasCar()
      @messages.push Car.jumpIn(dude)
      @messages.push Car.driveToThePark(dude)
      @messages.push Car.parkTheCar(dude)
    else
      @messages.push Bicycle.jumpOn(dude)
      @messages.push Bicycle.rideToThePark(dude)
      @messages.push Bicycle.parkTheBike(dude)
    @messages.push Park.enter(dude)
```

Please check out [this gist](https://gist.github.com/1654233#file_no_actions_goes_to_the_park_spec.coffee) to see the specs. I used [mocha](http://visionmedia.github.com/mocha/) to test-drive my code.

It's all nice and sweet. Except we have that nasty "if statement" in the middle of the GoesToThePark#toEnjoyTheWeather method.

Whenever I see a conditional block in the middle of a function call I immediately assume the violation of the [Single Responsibility Principle](http://en.wikipedia.org/wiki/Single_responsibility_principle).
I tolerate guard conditions in methods, but that "if statement" must die.

I remembered in my early Java and C# days reading about the [Chain of Responsibility](http://en.wikipedia.org/wiki/Chain-of-responsibility_pattern) design pattern. Every little command object is linked together with a linked list, the first one is called from the "coordinator" object and they each check if there is anything to do with the arguments. If there is, the action is executed and at the end of the method call the next command in the chain is being called.

I found them especially helpful in workflows similar to the example described above. The coordinator object only knows about the action objects and its only responsibility is to call the one and only method on them in order. There is no conditional in the method any more, the actions are smart enough to figure out if they have to deal with the object in the context or not.

I introduce the four new action objects:

1.  LeavesTheHouse - delegates calls to the House object
2.  DrivesToThePark - invokes the methods on the Car object if the dude has a car
3.  RidesToThePark - sends messages to the Bicycle object if the dude has no car
4.  EntersThePark - executes the enter method on the Park object

Only the DrivesToThePark and RidesTheBikeToThePark protects itself with guard conditions, their execution is dependent on the fact of the Dude having a car or not. But those are simple return statements at the very beginning of the method call.

```coffeescript
LeavesTheHouse = {
  execute: (messages, dude) ->
    messages.push House.leave(dude)
    messages.push House.closeTheDoor(dude)
}

DrivesToThePark = {
  execute: (messages, dude) ->
    return unless dude.hasCar()

    messages.push Car.jumpIn(dude)
    messages.push Car.driveToThePark(dude)
    messages.push Car.parkTheCar(dude)
}

RidesToThePark = {
  execute: (messages, dude) ->
    return if dude.hasCar()

    messages.push Bicycle.jumpOn(dude)
    messages.push Bicycle.rideToThePark(dude)
    messages.push Bicycle.parkTheBike(dude)
}

EntersThePark = {
  execute: (messages, dude) ->
    messages.push Park.enter(dude)
}

class GoesToThePark
  constructor: ->
    @messages = []

  toEnjoyTheWeather: (dude)->
    for action in [LeavesTheHouse, DrivesToThePark, RidesToThePark, EntersThePark]
      do =>
        action.execute(@messages, dude)

...
```

You can review the entire file in [this gist](https://gist.github.com/1654233#file_with_actions_goes_to_the_park_spec.coffee).

The beauty of this code lies in the toEnjoyTheWeather() method. It is simple and now it's super easy to test.

```coffeescript
...

  toEnjoyTheWeather: (dude)->
    for action in [LeavesTheHouse, DrivesToThePark, RidesToThePark, EntersThePark]
      do =>
        action.execute(@messages, dude)

...
```

In fact, I worked on a Ruby code where the coordinator object called a dozen different objects through it's private methods. Tests were brittle, I had to stare at the code to figure out why something was failing after a simple change. My specs were a clear indication that the code needed serious refactoring. I changed my code using the pattern above and I eliminated all the private methods - they became simple action objects - and testing became much simpler.

[Here is](https://gist.github.com/1654233#file_with_actions_and_stubs_goes_to_the_park_spec.coffee) what it takes to test the coordinator object's method with stubs:

```coffeescript
should = require 'should'
sinon = require 'sinon'

...

describe 'GoesToThePark', ->

  it 'calls the actions in order', ->
    goesToThePark = new GoesToThePark
    messages = goesToThePark.messages
    dude = {}

    leavesTheHouseStub = sinon.stub(LeavesTheHouse, 'execute') \
                              .withArgs(messages, dude)
    drivesToTheParkStub = sinon.stub(DrivesToThePark, 'execute') \
                               .withArgs(messages, dude)
    ridesToTheParkStub = sinon.stub(RidesToThePark, 'execute') \
                              .withArgs(messages, dude)
    entersTheParkStub = sinon.stub(EntersThePark, 'execute') \
                             .withArgs(messages, dude)

    goesToThePark.toEnjoyTheWeather(dude)
    entersTheParkStub.called.should.be.true
    drivesToTheParkStub.called.should.be.true
    ridesToTheParkStub.called.should.be.true
    entersTheParkStub.called.should.be.true
```

I leave you the exercise of writing the specs with stubs [for the example](https://gist.github.com/1654233#file_with_actions_and_stubs_goes_to_the_park_spec.coffee) prior to using the action objects.

Listen to your tests, they tell you the story (or the quality) of your code. Don't be afraid of creating tiny classes or objects with only 6-10 lines of code. They are super easy to test and I consider them the building blocks of reliable and maintainable software.

Big thanks to [websequencediagrams.com](http://www.websequencediagrams.com/) for their tool I used to create the sequence diagram in this blog post.


POSTED BY ATTILA DOMOKOS AT 7:38 PM

#### 4 comments:

[blessYAHU](https://www.blogger.com/profile/05553071198046693232) said...

_Nice post. Cool Web Sequence diagram tools. I was encouraged to take a crack at coding up an implementation. Bike and Car use the same method names (use,travelToPark,Park). A vehicle variable is used to store Bike or Car depending on whether Dude.hasCar, and use,travelToPark,Park methods are called on vehicle. Let me know what you think: http://jsbin.com/anakul/8._

April 12, 2012 at 12:25 AM

[Attila Domokos](https://www.blogger.com/profile/09067995287578229487) said...

_It's a nice alternative to what I described. And it works when both the Bike and the Car has the same methods. What do you do when the Car has an extra step called "turnOnTheEngine()"?_

_Also, I made a gist from your code so other people can see easily what you were trying to do: https://gist.github.com/2367479._

April 12, 2012 at 9:02 AM
