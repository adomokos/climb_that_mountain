## Sunday, April 27, 2014

### [Tracking Progress of a Rails Rewrite](http://www.adomokos.com/2014/04/tracking-progress-of-rails-rewrite.html)

I inherited a challenging codebase about [14 months ago](http://www.adomokos.com/2013/02/crossroads.html). The code did not have tests, there were methods with 100-200 lines of code in them filled with conditionals and iterators. It was obvious: we needed a rewrite. Ok, we were not thinking about [The Big Rewrite](http://vimeo.com/groups/87639/videos/16285681), but we knew the code had to be cleaned up. I had all the support from the leadership team, but I was asked for one thing: provide a metric to track progress.

![speedometer](/resources/2014/04/speedometer.jpg)

A few months went by and I did not have an answer. I couldn’t tell how fast or slow we can get done with the cleanup. Is it going to be six months, one year, maybe two? I did not know what to say.

My goal was extracting business logic from Rails controllers and models and putting them into `lib/services`. I wanted to structure the code into stateless, reusable actions with [LightService](https://github.com/adomokos/light-service), decoupling it from Rails as much as possible. I trusted these services as test coverage for them was fairly high. I knew no matter what business logic I had to implement, the small service classes were able to take anything I would throw at them.

The app used - the now unsupported - Data Mapper library that we wanted to get out of and transition over to Active Record. We put all our AR classes in the `app/models/ar` directory under the "AR" namespace. We kept these models clean, well tested. Now we trusted everything under `lib/services` and `app/models/ar`.

We also started to extract logic into RESTful controllers that had very little logic in them. These controllers were only a handful, maybe half a dozen. I realized we did not do much action clean up in the existing controllers. In fact, we shifted the responsibility to new controllers and services that we trusted.

One of my favorite Ruby code analysis tool is [flog](http://ruby.sadi.st/Flog.html). Flog gives you complexity points based on "ABC" metrics: assignments, branches and conditionals. You can run flog against your entire controller and model code and you get a complexity point. If you don't have much logic in the views or JavaScript, that number is your application's complexity.

Comparing trusted and untrusted total lines of code can provide a number, but I am not sure how much I could trust that. 10 lines of really crappy code loaded with iterators and conditionals compared to 10 lines of clean, readable and tested code just does not provide a one-to-one ratio. Why not looking at this problem from the complexity side? 5 point flog complexity can be on 1 single line in a crappy code, but it can be on 4 different lines in a method in the cleaned up code. Amount of coded logic should be expressed in complexity points and not in the number of lines of code.

I realized I could actually trust the flog complexity points to compare the trusted and untrusted code ratio. I easily calculated the total controller complexity. By subtracting the trusted controller complexity from the total complexity I had the untrusted complexity for controllers. I did the same for models. I put all the complexity from `lib/services` under the trusted bucket. Deviding the total trusted complexity by the untrusted code complexity provided the trusted/untrusted code ratio.

Here is an example of how the calculation worked:

<style>#example.table { border-collapse: collapse; } #example.td { padding: 4px; } td.trusted { color: #488214; } td.untrusted { color: #CD2626; } td.total { color: #838B8B; }</style>

<table id="example">

<tbody>

<tr>

<td class="total">Total controller complexity:</td>

<td class="total">8945</td>

</tr>

<tr>

<td class="trusted">Trusted controller complexity:</td>

<td class="trusted">489</td>

</tr>

<tr>

<td class="untrusted">Untrusted controller complexity (8945 - 489):</td>

<td class="untrusted">8456</td>

</tr>

<tr>

<td class="total">Total model complexity:</td>

<td class="total">1498</td>

</tr>

<tr>

<td class="trusted">Trusted model complexity:</td>

<td class="trusted">249</td>

</tr>

<tr>

<td class="untrusted">Untrusted model complexity (1498 - 249):</td>

<td class="untrusted">1249</td>

</tr>

<tr>

<td class="trusted">Trusted services complexity:</td>

<td class="trusted">845</td>

</tr>

<tr>

<td class="untrusted">Untrusted total complexity:</td>

<td class="untrusted">9705</td>

</tr>

<tr>

<td class="trusted">Trusted total complexity:</td>

<td class="trusted">1583</td>

</tr>

<tr>

<td>Trusted/Untrusted code ratio:</td>

<td>1583/9705 * 100 = 16.3%</td>

</tr>

</tbody>

</table>

_The numbers in the example above are made up, they don't reflect the code of my [current employer](http://www.hireology.com)._

A month later you can do the same calculation and you’ll see that the Trusted/Untrusted code ratio is 19.5%. Well, look at that, 3% of your code just gained trust! That new 3% of the code is easy to change as it's small, passes all the tests, communicates intent and has no duplication.

By yielding 3% more trusted code in a month, you will need more than 2 years to clean up the existing code base unless you can accelerate the code clean up somehow.


POSTED BY ATTILA DOMOKOS AT 9:52 PM


NO COMMENTS
