## Wednesday, December 14, 2011

### [(More) Specific Stubbing with RSpec](http://www.adomokos.com/2011/12/more-specific-stubbing-with-rspec.html)

A couple of months ago we had to write code for the following feature: a company would like to reward its most valuable customers by giving them credit which they can use in their future orders.

We came up with the following solution:

```ruby
class GivesCreditToPreferredCustomers
  def self.for_large_orders(sales_amount, added_credit)
    preferred_customers = Customer.has_large_purchases(sales_amount)
    preferred_customers.each do |customer|
      customer.add_credit added_credit
    end
  end
end

class Customer
  attr_reader :total_credit

  def self.has_large_purchases(sales_amount)
    puts "AR query to find buyers with large purchases"
  end

  def add_credit(amount)
    @total_credit = 0 if @total_credit.nil?
    @total_credit += amount
  end
end

describe GivesCreditToPreferredCustomers do
  specify "for large orders" do
    sales_amount = 10000
    credit_given = 100
    found_customer = Customer.new
    Customer.stub(:has_large_purchases) \
            .and_return [found_customer]

    GivesCreditToPreferredCustomers \
            .for_large_orders(sales_amount, credit_given)

    found_customer.total_credit.should == credit_given
  end
end
```

Take a look at the lines where the Customer's :has_large_purchases method is being stubbed: "Customer.stub(:has_large_purchases).and_return([found_customer])".  
Everything is passing there, even though I have not specified any arguments. Of course: when you don't specify arguments, RSpec will take any arguments (or no arguments) and return the canned response.

A couple of months passes by and a new requirement comes in: we need to look at only the last 3 months of purchases, otherwise the company is giving away too much credit to its customers. The look back period is the same to all customers, it's safe to put it in the GivesCreditToPreferredCustomers class.

You would obviously start with modifying the spec, but your co-worker wants to get this done really quick and updates the application code like this:

```ruby
class GivesCreditToPreferredCustomers
  LOOK_BACK_PERIOD = 3
  def self.for_large_orders(sales_amount, added_credit)

    # the has_large_purchases scope now takes two arguments
    preferred_customers = Customer.has_large_purchases(sales_amount, LOOK_BACK_PERIOD)

    preferred_customers.each do |customer|
      customer.add_credit added_credit
    end
  end
end
```

I execute the spec and everything passes:
<span style="color: #090">.</span>

Finished in 0.00063 seconds
<span style="color: #090">1 example, 0 failures</span>

Wow! That's quite a bit of change and nothing failed. Yet.

Let's make sure that only those messages are stubbed that have the correct arguments. I add the with() method to the stub's method chain:

```ruby
describe GivesCreditToPreferredCustomers do
  specify "for large orders" do
    sales_amount = 10000
    credit_given = 100
    look_back_period = 3
    found_customer = Customer.new

    Customer.stub(:has_large_purchases) \
            # stub with arguments
            .with(sales_amount, look_back_period) \
            .and_return [found_customer]

    GivesCreditToPreferredCustomers \
            .for_large_orders(sales_amount, credit_given)

    found_customer.total_credit.should == credit_given
  end
end
```

Everything passes in the spec but we are now stubbing messages only where the :has_large_purchases method is called with the passed in sales amount (10,000) and the correct look back period (3).
<span style="color: #090">.</span>

Finished in 0.00062 seconds
<span style="color: #090">1 example, 0 failures</span>

Let's see what happens when the LOOK_BACK_PERIOD is changed to 2 due to a new requirement from the customer:

<span style="color: #900;">F</span>

Failures:

  1) GivesCreditToPreferredCustomers for large orders
<span style="color: #900;">     Failure/Error: preferred_customers = Customer.has_large_purchases(sales_amount, LOOK_BACK_PERIOD)  
       <customer (class)="">received :has_large_purchases with unexpected arguments
         expected: (10000, 3)
         got: (10000, 2)</customer></span>     # ./describe_stub_spec.rb:5:in `for_large_orders'
     # ./describe_stub_spec.rb:38:in `block (2 levels) in <top (required)="">'

Finished in 0.00104 seconds
<span style="color: #900;">1 example, 1 failure</span></top>

This would happily pass with a stub where I don't specify the arguments but it fails here where the stub argument is strictly defined.

Adding the argument is a little bit more work but the benefits are huge: you are exercising not only the message sent to the object but the arguments that the message is sent with.

Happy stubbing!

You can review the example I created for this blog post in [this Gist](https://gist.github.com/1473661).


POSTED BY ATTILA DOMOKOS AT 9:06 AM

#### 2 comments:

[Chris Flipse](https://www.blogger.com/profile/04631135031429100516) said...

This comment has been removed by the author.

December 14, 2011 at 1:43 PM

[Chris Flipse](https://www.blogger.com/profile/04631135031429100516) said...

_at this point, you're not stubbing, you're mocking. If you care about what the message is called with then that should be a mock, not built into the basic stub._

_The thing you've done above is to introduce a second check into the spec. Now, when there's a failure, I need to spend time working out WHY my spec is failing. Much better to dedicate a specific, documented test to that case -- this follows the principal of one expectation per spec._

[https://gist.github.com/1478077](https://gist.github.com/1478077)

_This way, there's an actual check that verifies specifically that you care about the arguments to the method, in rspec's intention revealing language. If it ever breaks, you know immediately, and you know why ... and the specs checking other behavioral aspects aren't cluttered by mocking out the full API of every intermediate helper method._

December 14, 2011 at 1:44 PM
