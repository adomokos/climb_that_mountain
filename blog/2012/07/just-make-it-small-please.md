## Monday, July 30, 2012

### [Just make it small, please!](http://www.adomokos.com/2012/07/just-make-it-small-please.html)

I have seen - and had to maintain - so many messed up, bad code in my carrier that it makes me wonder why I still work in this profession. In fact, I have rarely seen good, clean code. However, I can learn a ton going through open source code repos on Github.

The best definition I have found for clean code is by [Michael Feathers](http://michaelfeathers.typepad.com/) captured in the book [Clean Code](http://www.amazon.com/Clean-Code-Handbook-Software-Craftsmanship/dp/0132350882/ref=sr_1_1?ie=UTF8&qid=1342632156&sr=8-1&keywords=clean+code): "Clean code always looks like it was written by someone who cares."

![small_cup](/resources/2012/07/small_cup.jpeg)

Do you really <span style="text-decoration: underline;">care</span> about the code or the craft, when:

*   you put 2844 lines of code in the model?
*   you have 167 lines of code in one function?
*   you have several deep nested if statements in for-each loops?
*   you have 1354 lines of code in a js file that drives business logic?
*   you have no tests at all?

I attended the fantastic [Simple Design and Testing Conference](http://sdtconf.com/) a while ago. One of the topics we discussed there was the most important principle we'd ask a developer should follow. [DRY](http://en.wikipedia.org/wiki/Don't_repeat_yourself) was the absolute winner.
We all know about and follow the DRY principle but I question if that's enough?

Quite frankly I don't have the patience to analyze a 60 line function loaded with iterators and conditionals. Even the code's author can not understand it 5 minutes after it was written.

My coding style has changed in the last 4-5 years. I tend to write code in a functional style, class objects with a single function that are not longer than 5-10 lines.
Here is one of those:

```ruby
module Services; module Utils
  class DecryptsData

    class << self

      def execute(service_result)
        return service_result unless service_result.success?

        encrypted_data = service_result.fetch(:encrypted_data)
        decrypted_data = ::Encryption.decrypt(encrypted_data)
        data_key = service_result.fetch(:data_key)

        service_result[data_key] = decrypted_data

        service_result
      end

    end
  end
end; end
```

Look at this piece of code for a moment. Try to understand what I am doing here.

1.  A guard condition (line 7)
2.  Pulling the encrypted data from the context (line 9)
3.  Decrypting data (line 10)
4.  Pulling the key I save the decrypted data with (line 11)
5.  Saving the decrypted data in the context (line 13)

All I am doing is decrypting data. That's it. I am not querying the database, I am not validating data, I am not calling an external service and I am not looping through items and set properties based on some kind of predicate.

People might just shove this into the controller. I won't. I think about software as a collection of functions that's weaved together by organizer functions.

The benefits are enormous:

*   One function
*   -> which is short
*   Easy to understand
*   -> which is easy to test

You could say that I am doing something in Ruby that resembles to [functional programming](http://en.wikipedia.org/wiki/Functional_programming). I call functions on class objects, but the functions I am constructing are not immutable. And I new up an object and maintain state if I need to, but I try to avoid that for the sake of <span style="font-weight: bold;">simplicity</span>. I don't want anybody - who maintains the code I write - to spend a lot of time trying to understand what I am doing.

Is functional programming far for me? I don't think so, I believe I am just taking the first steps in that direction.


POSTED BY ATTILA DOMOKOS AT 8:12 AM


#### 7 comments:

[Unknown](https://www.blogger.com/profile/00169666883152173097) said...

_This brought a tear to my eye. You will understand this: http://www.infoq.com/presentations/Simple-Made-Easy/_

July 30, 2012 at 8:47 PM

[Unknown](https://www.blogger.com/profile/00169666883152173097) said...

_(That was Alex, btw, the login thing is not simple!)_

July 30, 2012 at 8:56 PM

[Attila Domokos](https://www.blogger.com/profile/09067995287578229487) said...

_Thanks Alex! Yup, I've seen that presentation. Highly - even higher - recommended!!_

July 30, 2012 at 9:33 PM

[Kevin](https://www.blogger.com/profile/02350876287962972955) said...

_If you aren't going to use instances of the class, why not just use modules?_

https://gist.github.com/3218066

July 31, 2012 at 11:03 AM

[Attila Domokos](https://www.blogger.com/profile/09067995287578229487) said...

_@Kevin: You could use modules as well, but I tend to use modules only when I consider the logic to be mixed into other classes or objects._

July 31, 2012 at 12:25 PM

[Joakim](https://www.blogger.com/profile/06741642450577678085) said...

_I do understand what the code do, and I agree it's good code in that way._

_I don't however fully understand in what context this would be used. Seems VERY domain specific._

_What kind of a role does service_result play? Seems to be some kind of object that responds to "success?", "fetch" and "[]=" where you can assume that "fetch" returns the right thing for ":encrypted_data" and ":data_key"... (it's not just data and it's not a well defined domain object)._

_Also you seem to both assign data to the reference that is passed in and return a value?_

October 20, 2012 at 11:25 AM

[Attila Domokos](https://www.blogger.com/profile/09067995287578229487) said...

_Joakim,_

_service_result is the context for the call chain. I only show one item in here, however I tend to call multiple services - similar to this one - as a chain of actions._
_This class might resemble to the Chain of Responsibility and the Command pattern. The service_result both holds the data the service has to work with and the command adds its calculated result to it._

_And you're correct, I both alter the passed in service_result object and return it for convenience. Returning a boolean value as an execution result would not provide much value, since the service_result has the success? flag in it anyway._

October 20, 2012 at 4:27 PM
