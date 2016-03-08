## Sunday, September 12, 2010

### [Making Your Tests More Readable with MSpec](http://www.adomokos.com/2010/09/making-your-tests-more-readable-with.html)

After playing with [MSpec](http://github.com/machine/machine.specifications) on somebody else' machine a couple of weeks ago I decided to give it a try today and tweak it a bit. I worked on the [Movie Tickets kata](http://codingkata.org/katas/unit/movie-tickets) with someone who was not an experienced TDD/BDD-er and this is how far we went.

Getting MSpec is pretty easy. Pull the source code from the github repo and build it. Once you have everything built you should just reference the dll in your C# project: Machine.Specifications.
I wanted to keep my solution as simple as possible so I kept both the TicketCalculator and its specs in the same .cs file.

![TryMSpecSolution.png](/resources/2010/09/TryMSpecSolution.png)

The kata specifies the method names that you start with so I went ahead and created the class:

<pre class="brush: csharp">public class TicketCalculator {
  public void StartPurchase(int runtime, DayOfWeek day, bool isParquet, bool is3D) {}
  public void AddTicket(int age, bool isStudent) {}
  public decimal FinishPurchase() {return 0m;}
}
</pre>

I put my first spec right below the TicketCalculator class:

<pre class="brush: csharp">[Subject("Basic admission rates")]
public class Purchasing_general_ticket_as_an_adult_non_student{
  private static TicketCalculator _calculator;

  Establish setup_expectation = () => {
    _calculator = new TicketCalculator();
    _calculator.StartPurchase(115, DayOfWeek.Monday, true, false);};

  Because trigger = () =>
    _calculator.AddTicket(33, false);

  It verify = () =>
    _calculator.FinishPurchase().ShouldEqual(11m);
}
</pre>

It compiled fine, however, when I executed the spec this is the error I got:

<div style="color: #CC0000"><trymspec>(1 test), 1 test failed<br/>
TryMSpec (1 test), 1 test failed<br/>
Basic admission rates, Purchasing general ticket as an adult non student (1 test), 1 test failed<br/>
verify, Failed: Machine.Specifications.SpecificationException: Should equal [11] but is [0]<br/>
</trymspec></div>


Since refactoring in the "red" is forbidden, I did the simplest thing that would make my test to pass: I returned 11 from the "FinishPurchase()" method.

I am looking at both the MSpec code and the output and they are ugly: it's really hard to read. To me a test is readable when I can show it to someone and can pretty much tell what's going on in there.

My spec passed so I started cleaning up my code. The first thing I did was introducing new aliases for the delegate names. Establish, Because and It felt awkward. I always think about Given-When-Then state transitions in my tests and this change just felt more natural to me.

<pre class="brush: csharp">using Given = Machine.Specifications.Establish;
using When = Machine.Specifications.Because;
using Then = Machine.Specifications.It;

[Subject("Basic admission rates")]
  public class Purchasing_general_ticket_as_an_adult_non_student{
    private static TicketCalculator _calculator;

    Given setup_expectation = () =>{
      _calculator = new TicketCalculator();
      _calculator.StartPurchase(115, DayOfWeek.Monday, true, false);};

    When trigger = () =>
      _calculator.AddTicket(33, false);

    Then verify = () =>
      _calculator.FinishPurchase().ShouldEqual(11m);
  }
</pre>

This was a good start, but the code is far from readable. I tweaked the delegate names a little bit and I ended up with this:

<pre class="brush: csharp">[Subject("Basic admission rates")]
  public class Purchasing_general_ticket_as_an_adult_non_student{
    private static TicketCalculator _calculator;

    Given i_buy_a_standard_movie_ticket = () =>{
      _calculator = new TicketCalculator();
      _calculator.StartPurchase(115, DayOfWeek.Monday, true, false);};

    When i_purchase_it_for_an_adult_non_student = () =>
      _calculator.AddTicket(33, false);

    Then i_pay_11_bucks = () =>
      _calculator.FinishPurchase().ShouldEqual(11m);
  }
</pre>

Try to read this code! There is some noise around it, but you should be able to read it out:

  <span style="font-weight: bold;">Given</span> - I buy a standard movie ticket
  <span style="font-weight: bold;">When</span> - I purchase it for an adult non-student
  <span style="font-weight: bold;">Then</span> - I pay $11

I don't have much space on this page, but if you indent the lambda a little more the Given - When - Then words will become more apparent.

Let's look at the second scenario: a standard movie ticket for a student is $8.
Here is my spec for that:

<pre class="brush: csharp">[Subject("Basic admission rates")]
public class Purchasing_general_ticket_as_an_adult_student{
  private static TicketCalculator _calculator;

  Given i_buy_a_standard_movie_ticket = () => {
    _calculator = new TicketCalculator();
    _calculator.StartPurchase(115, DayOfWeek.Monday, true, false);};

  When i_purchase_it_for_an_adult_student = () =>
    _calculator.AddTicket(33, true);

  Then i_pay_8_bucks = () =>
    _calculator.FinishPurchase().ShouldEqual(8m);
}
</pre>

When I executed the spec with MSpec I received the following error:

<div style="color: #CC0000"><trymspec>(2 tests), 1 test failed<br/>
TryMSpec (2 tests), 1 test failed<br/>
Basic admission rates, Purchasing general ticket as an adult non student (1 test), Success<br/>
i pay 11 bucks, Success<br/>
Basic admission rates, Purchasing general ticket as an adult student (1 test), 1 test failed<br/>
i pay 8 bucks, Failed: Machine.Specifications.SpecificationException: Should equal [8] but is [11]<br/>
</trymspec></div>


Again, I did the simplest thing that could possibly work:

<pre class="brush: csharp">public class TicketCalculator{
  private decimal _ticket_price = 11m;
  public void StartPurchase(int runtime, DayOfWeek day, bool isParquet, bool is3D) {}

  public void AddTicket(int age, bool isStudent){
    if (isStudent)
      _ticket_price = 8m;
  }

  public decimal FinishPurchase(){ return _ticket_price; }
}
</pre>

Everything passed:

<div style="color: #336600"><trymspec>(2 tests), Success
TryMSpec (2 tests), Success
Basic admission rates, Purchasing general ticket as an adult non student (1 test), Success
i pay 11 bucks, Success
Basic admission rates, Purchasing general ticket as an adult student (1 test), Success
i pay 8 bucks, Success
</trymspec></div>

Notice we have duplication in our specs: the "Given" delegate is duplicated the exact same way in both of them. Time to [DRY](http://en.wikipedia.org/wiki/Don't_repeat_yourself) up the code a little bit. I created a base class called SpecBase and moved the field "_calculator" and the "Given" delegate into it.

<pre class="brush: csharp">public class SpecBase{
  protected static TicketCalculator _calculator;

  Given i_buy_a_standard_movie_ticket = () => {
    _calculator = new TicketCalculator();
    _calculator.StartPurchase(115, DayOfWeek.Monday, true, false);
  };
}
</pre>

Both of the specs are now inheriting from this base class. The second one looks like this:

<pre class="brush: csharp">[Subject("Basic admission rates")]
public class Purchasing_general_ticket_as_an_adult_student : SpecBase{
  When i_purchase_it_for_an_adult_student = () =>
    _calculator.AddTicket(33, true);

  Then i_pay_8_bucks = () =>
    _calculator.FinishPurchase().ShouldEqual(8m);
}
</pre>

This way the spec is short and there is no duplication, however, I don't see the "Given" part in it. I took care of it by renaming the "SpecBase" class to "Given_i_buy_a_standard_ticket" and with the right indentation I should have a readable spec that tells a story.

<pre class="brush: csharp">[Subject("Basic admission rates")]
public class Purchasing_general_ticket_as_an_adult_student : 
  Given_i_buy_a_standard_ticket{
  When i_purchase_it_for_an_adult_student = () => _calculator.AddTicket(33, true);
  Then i_pay_8_bucks = () => _calculator.FinishPurchase().ShouldEqual(8m);
}
</pre>

You can find the final C# file in this gist: [http://gist.github.com/576433](http://gist.github.com/576433).

I'd like to list a couple of things that you should see - and maybe follow - based on this example:

*   look at the length of the specs, none of them are more than 3 lines of code,
*   there is no code duplication,
*   there is only one assertion in each one of them,
*   the spec tells you a story.


POSTED BY ATTILA DOMOKOS AT 9:33 PM
