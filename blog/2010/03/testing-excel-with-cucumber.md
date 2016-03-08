## Sunday, March 14, 2010

### [Testing Excel with Cucumber](http://www.adomokos.com/2010/03/testing-excel-with-cucumber.html)

Testing an Excel application with [Ruby's unit testing tool](http://ruby-doc.org/stdlib/libdoc/test/unit/rdoc/classes/Test/Unit.html) is doable, however, the tests are not easily readable.
Why not using Given/When/Then structured [Cucumber](http://cukes.info/) tests instead? Cucumber is an active [open source project](http://wiki.github.com/aslakhellesoy/cucumber/) enjoying great popularity
in the Rails universe. I won’t be describing the tool in this blog post, if you’re new to Cucumber, please visit the official website or read the excellent [Cucumber book](http://www.pragprog.com/titles/achbd/the-rspec-book)
to learn more about it.

Here is the task I need to write tests for: we have a basic Excel spreadsheet with a handful of cells in it.

![excel_spreadsheet](/resources/2010/03/excel_spreadsheet.jpg)

The row cells with yellow background are the column headers. Column cells starting from A2 are captions. "Category1" has three child elements and "Category2" has only two. The category rows are calculated by the sum of their children. Value Total is the sum of Value1 and Value2 for each row. The totals in the bottom are calculated by adding up Category1 and Category2 values.

First I'd like to verify that the column headers and captions are in place. I started writing my feature this way:

<span style="color: blue;">Feature: Modify values in the Excel sheet</span>
<span style="color: blue;">In order to show my power</span>
<span style="color: blue;">  As a user</span>
<span style="color: blue;">  I want to interact with Excel</span>

<span style="color: blue;">Scenario: Display column headers and captions</span>
<span style="color: blue;">  Given I have 2 categories</span>
<span style="color: blue;">    And I have 3 child elements under the first category</span>
<span style="color: blue;">  When I open the Excel workbook</span>
<span style="color: blue;">  Then I should see "Category" in the "A1" cell</span>
<span style="color: blue;">    And I should see "Value Total" in the "B1" cell</span>
<span style="color: blue;">    And I should see "Value1" in the "C1" cell</span>
<span style="color: blue;">    And I should see "Value2" in the "D1" cell</span>
<span style="color: blue;">    And I should see "Category1" in the "A2" cell</span>
<span style="color: blue;">    And I should see "Child1" in the "A3" cell</span>
<span style="color: blue;">    And I should see "Child2" in the "A4" cell</span>

I created the following folder structure:
/features
  |
  |-- step_definitions
  |      |-- excel_handler.rb
  |      |-- excel_steps.rb
  |-- support
  |      |-- env.rb
  | - excel.feature

The feature from above was saved in the excel.feature file.

I am not particularly concerned with the "Given" part of the scenario. The data can be loaded into Excel either from a CSV, XML or a remote data store. I'll ignore this part to keep my examples clear and concise.

My [previous blog post](http://adomokos.blogspot.com/2010/03/automated-testing-of-oba-app-with-ruby.html) described how I can interact with Excel through the great [WIN32OLE](http://ruby-doc.org/stdlib/libdoc/win32ole/rdoc/classes/WIN32OLE.html) object. I created the ExcelHandler class which does that:

<pre class="brush: ruby">class ExcelHandler
  include Singleton

  # set this to your Excel file path
  @@excel_file_path = 'C:\Temp\TestWorkbook.xlsx'
  def open_excel
    begin
      @excel = WIN32OLE.connect('excel.application')
      @wb = @excel.ActiveWorkbook
    rescue
      @excel = WIN32OLE::new('excel.application')
      @excel.visible =true
      @wb = @excel.Workbooks.Open(@@excel_file_path )
    end
  end
end
</pre>

I might not have the most elegant code in the open_excel method, but this allows me to attach to a running instance of the Excel workbook which is a big thing for me. In case the workbook has not been launched yet, I take care of it here. Launching and closing Excel takes time and resources, I'd like to reuse running instances of Excel whenever I can.

I take advantage of the singleton pattern in this class. Starting up Excel is an expensive operation, I want to make sure that one and only instance is handling the workbook. My tests are single threaded, I think I should be safe there.

The env.rb file has the require statements:

<pre class="brush: ruby">require 'spec/expectations'
require 'win32ole'
require 'singleton'
</pre>

All the magic happens in the excel_steps.rb file:

<pre class="brush: ruby">When /^I open the Excel workbook$/ do
  ExcelHandler.instance.open_excel
  @worksheet = ExcelHandler.instance.worksheet
  @worksheet.extend CellValueGetter
end

Then /^I should see "([^\"]*)" in the "([^\"]*)" cell$/ do |value, cell|
  @worksheet.get_cell_value(cell).strip.should == value
end

module CellValueGetter
  def get_cell_value(cell)
    get_cell(cell).value
  end

  def set_cell_value(cell, value)
    get_cell(cell).value = value
  end

  def get_cell(cell)
    cell_values = cell.split('')
    cell_values.length.should == 2
    cells(cell_values[1].to_i, cell_values[0])
  end
end
</pre>

Look at how I add methods to the @worksheet object. You gotta love Ruby for that!
The methods in this module are responsible for getting and setting a value based on the provided cell.
I left out the Given parts that I ignore anyway. You can look at the entire source code after you pulled it from my [github account](http://github.com/adomokos/cucumber_excel).
When I execute "cucumber features" in the command prompt I get this:

<span class="Apple-style-span" style="color: #6aa84f;">1</span> <span class="Apple-style-span" style="color: #38761d;">scenario (1 passed)</span>
<span class="Apple-style-span" style="color: #38761d;">10 steps (10 passed)</span>
<span class="Apple-style-span" style="color: #38761d;">0m0.027s</span>

Hey, the first scenario is passing!!

All right, let's verify in the second scenario that the data was loaded correctly:
<span class="Apple-style-span" style="color: blue;">
Scenario: Display loaded values
  Given I have 2 categories
    And I have 3 child elements under the first category
  When I open the Excel workbook
  Then I should see 111 in the "C3" cell
    And I should see 353 in the "C2" cell
    And I should see 458 in the "B3" cell
    And I should see 1523 in the "B2" cell</span>

I had to add one step definition to the excel_step.rb file:

<pre class="brush: ruby">Then /^I should see (\d+) in the "([^\"]*)" cell$/ do |value, cell|
  @worksheet.get_cell_value(cell).should == value.to_i
end
</pre>

When I execute "cucumber features" in the command prompt I see this:

<span class="Apple-style-span" style="color: #6aa84f;">1 scenario (1 passed)
7 steps (7 passed)
0m0.024s
</span>

I know I am not using the "Given" part of the scenarios, however, I do repeat code there. I used the [background feature](http://wiki.github.com/aslakhellesoy/cucumber/background) of Cucumber and DRY-ed up my scenarios a little bit.

<span class="Apple-style-span" style="color: blue;">Background:
Given I have 2 categories
And I have 3 child elements under the first category</span>

I use [scenario outline](http://wiki.github.com/aslakhellesoy/cucumber/scenario-outlines) in my third scenario. I set the "Value1" cell for the "Child1" row to 211\. Take a look at the result in Excel:

![](resources/2010/03/excel_spreadsheet_add_100.jpg)

I also try to set the same cell to 51, I got these numbers then:

![](resources/2010/03/excel_spreadsheet_subtract_160.jpg)

I am verifying numbers in red in the last scenario:

<span class="Apple-style-span" style="color: blue;">Scenario Outline: Change values
  Given the default values were loaded
  When I open the Excel workbook
    And I put <child1_value> in the "C3" cell
  Then I should see <category_value1> in the "C2" cell
    And I should see <child1_sum> in the "B3" cell
    And I should see <category_total> in the "B2" cell
    And I should see <value1_total> in the "C9" cell

Examples:
| child1_value | category_value1 | child1_sum | category_total | value1_total |
| 211 | 453 | 558 | 1623 | 1281 |
| 51 | 293 | 398 | 1463 | 1121 |</span>

I added the following step to the excel_steps.rb file:

<pre class="brush: ruby">When /^I put (\d+) in the "([^\"]*)" cell$/ do |value, cell|
  @worksheet.set_cell_value(cell, value)
end
</pre>

Let's see what we get now:

<span class="Apple-style-span" style="color: #6aa84f;">2 scenarios (2 passed)
18 steps (18 passed)
0m0.038s</span>

All the scenarios passed individually; let's see how we do when we execute them all at once:

<span class="Apple-style-span" style="color: #6aa84f;">4 scenarios (4 passed)
37 steps (37 passed)
0m3.718s</span>

Everything is rock solid so far. Am I testing the right file? I change the first scenario's "Then I should see "Category" in the "A1" cell" line to this: "Then I should see "CategoryABC" in the "A1" cell".
When I execute "cucumber features" I get the following output:

<span class="Apple-style-span" style="color: #cc0000;">4 scenarios (1 failed, 3 passed)</span>
<span class="Apple-style-span" style="color: #cc0000;">37 steps (1 failed, 6 skipped, 30 passed)</span>
<span class="Apple-style-span" style="color: #cc0000;">0m3.710s</span>

I see that the scenario I just changed is now failing:

<span class="Apple-style-span" style="color: #cc0000;">Then I should see "CategoryABC" in the "A1" cell # features/step_definitions/excel_steps.rb:23
  expected: "CategoryABC",
    got: "Category" (using ==)
  Diff:
  @@ -1,2 +1,2 @@
  -CategoryABC
  +Category
  (Spec::Expectations::ExpectationNotMetError)
  ./features/step_definitions/excel_steps.rb:24:in `/^I should see "([^\"]*)" in the "([^\"]*)" cell$/'
features\excel.feature:13:in `Then I should see "CategoryABC" in the "A1" cell'</span>

I change it back to the original one and everything is OK again.

One more thing I need to do: when the feature is complete, I'd like to close Excel.

I use the cucumber's at_exit hook to accomplish that:

<pre class="brush: ruby">at_exit do
  ExcelHandler.instance.close_excel
end</pre>

When I execute the cucumber feature I see Excel popping up and closing down.
What if I wanted to run the feature in headless mode? It's simple, I just have to change the @excel.visible = true value to false in the excel_handler.rb file.

I think cucumber is a great way to automate testing on an Excel spreadsheet. The cucumber features can serve both as requirement documents and test scripts, hence providing executable documentation. They are easy to read and understand for the entire team.

You can get the demo's source code from my [github account](http://github.com/adomokos/cucumber_excel).

I developed the code on Windows 7 using ruby version 1.9.1\. You also need cucumber and the rspec gems installed to run the code.

Happy coding!

#### 10 comments:

[Matt.Snyder](https://www.blogger.com/profile/15724459143365994221) said...

Nice post Attila!

March 15, 2010 at 9:00 AM

[Aslak Hellesøy](https://www.blogger.com/profile/05425581666293755697) said...

Very nice.

Now, what would have been nice is to do the reverse - have Cucumber pull in values to use in a Scenario Outline from Excel (or Google Spreadsheet). That might make it a little easier to involve non-technical users in the writing of automated tests.

We might add support for this in a future Cucumber release.

March 15, 2010 at 11:39 AM

[adomokos](https://www.blogger.com/profile/09067995287578229487) said...

Great idea! I don't think it would be too hard to implement that. I'll fork Cucumber on github and I'll try to look under the hood. Maybe that will be my next blog post.

March 15, 2010 at 2:02 PM

[dragonsrefuge](http://unimatrixzxero.macbay.de/dragonsrefuge/) said...

Props. I would never have thought that a problem in Excel would be so elegantly attackable with ruby. Nice.

May 3, 2010 at 6:29 AM

[Future Silas](https://www.blogger.com/profile/13965407922178490515) said...

I really like what you have done and I'm extending the concept and using it. May I recommend that you change get_cell(cell) as follows:

def get_cell(cell)
cell_values = Array.new
cell_values[0] = cell[0..(cell.index(/\d+/)1)]
cell_values[1] = cell[cell_values[0].length..cell.length]
cell_values.length.should == 2
cell_values[0].should =~ /[A-Za-z]+/
cell_values[1].should =~ /\d+/
cells(cell_values[1].to_i, cell_values[0])
end

October 22, 2010 at 12:03 PM

[Loga](https://www.blogger.com/profile/09934272028071512845) said...

Is there a similar library I could use for Mac/Linux boxes, as Winole32 is Windows based only.

July 4, 2011 at 12:36 PM

[devaru](https://www.blogger.com/profile/01928167428097210190) said...

HI Admokos, Itried to run ur code but I am getting error as follows.
g:\cucumber\adomokos-cucumber_excel-8ea39bb\features>cucumber excel.feature
no such file to load -- spec/expectations (LoadError)
C:/Ruby187/lib/ruby/site_ruby/1.8/rubygems/custom_require.rb:55:in `gem_original_require'
C:/Ruby187/lib/ruby/site_ruby/1.8/rubygems/custom_require.rb:55:in `require'
g:/cucumber/adomokos-cucumber_excel-8ea39bb/features/support/env.rb:1
C:/Ruby187/lib/ruby/gems/1.8/gems/cucumber-1.0.0/bin/../lib/cucumber/rb_support/rb_language.rb:143:in `load'
C:/Ruby187/lib/ruby/gems/1.8/gems/cucumber-1.0.0/bin/../lib/cucumber/rb_support/rb_language.rb:143:in `load_code_file'
C:/Ruby187/lib/ruby/gems/1.8/gems/cucumber-1.0.0/bin/../lib/cucumber/runtime/support_code.rb:176:in `load_file'
C:/Ruby187/lib/ruby/gems/1.8/gems/cucumber-1.0.0/bin/../lib/cucumber/runtime/support_code.rb:78:in `load_files!'
C:/Ruby187/lib/ruby/gems/1.8/gems/cucumber-1.0.0/bin/../lib/cucumber/runtime/support_code.rb:77:in `each'
C:/Ruby187/lib/ruby/gems/1.8/gems/cucumber-1.0.0/bin/../lib/cucumber/runtime/support_code.rb:77:in `load_files!'
C:/Ruby187/lib/ruby/gems/1.8/gems/cucumber-1.0.0/bin/../lib/cucumber/runtime.rb:137:in `load_step_definitions'
C:/Ruby187/lib/ruby/gems/1.8/gems/cucumber-1.0.0/bin/../lib/cucumber/runtime.rb:39:in `run!'
C:/Ruby187/lib/ruby/gems/1.8/gems/cucumber-1.0.0/bin/../lib/cucumber/cli/main.rb:43:in `execute!'
C:/Ruby187/lib/ruby/gems/1.8/gems/cucumber-1.0.0/bin/../lib/cucumber/cli/main.rb:20:in `execute'
C:/Ruby187/lib/ruby/gems/1.8/gems/cucumber-1.0.0/bin/cucumber:14
C:/Ruby187/bin/cucumber:19:in `load`
C:/Ruby187/bin/cucumber:19

August 3, 2011 at 5:15 AM

[adomokos](https://www.blogger.com/profile/09067995287578229487) said...

deravu: do you have the rspec gem installed?

August 3, 2011 at 5:17 AM

[devaru](https://www.blogger.com/profile/01928167428097210190) said...

Yes I Have installed. Infact I have updated my Ruby version to 1.9.2 & cucumber to 1.0.2 after that I am getting following error.
G:\cucumber\adomokos-cucumber_excel-8ea39bb\features>cucumber excel.feature
no such file to load -- spec/expectations (LoadError)
C:/Ruby192/lib/ruby/site_ruby/1.9.1/rubygems/custom_require.rb:36:in `require'
C:/Ruby192/lib/ruby/site_ruby/1.9.1/rubygems/custom_require.rb:36:in `require'
G:/cucumber/adomokos-cucumber_excel-8ea39bb/features/support/env.rb:1:in `'
C:/Ruby192/lib/ruby/gems/1.9.1/gems/cucumber-1.0.2/lib/cucumber/rb_support/rb_language.rb:143:in `load'
C:/Ruby192/lib/ruby/gems/1.9.1/gems/cucumber-1.0.2/lib/cucumber/rb_support/rb_language.rb:143:in `load_code_file'
C:/Ruby192/lib/ruby/gems/1.9.1/gems/cucumber-1.0.2/lib/cucumber/runtime/support_code.rb:176:in `load_file'
C:/Ruby192/lib/ruby/gems/1.9.1/gems/cucumber-1.0.2/lib/cucumber/runtime/support_code.rb:78:in `block in load_files
C:/Ruby192/lib/ruby/gems/1.9.1/gems/cucumber-1.0.2/lib/cucumber/runtime/support_code.rb:77:in `each'
C:/Ruby192/lib/ruby/gems/1.9.1/gems/cucumber-1.0.2/lib/cucumber/runtime/support_code.rb:77:in `load_files!'
C:/Ruby192/lib/ruby/gems/1.9.1/gems/cucumber-1.0.2/lib/cucumber/runtime.rb:137:in `load_step_definitions'
C:/Ruby192/lib/ruby/gems/1.9.1/gems/cucumber-1.0.2/lib/cucumber/runtime.rb:39:in `run!'
C:/Ruby192/lib/ruby/gems/1.9.1/gems/cucumber-1.0.2/lib/cucumber/cli/main.rb:43:in `execute!'
C:/Ruby192/lib/ruby/gems/1.9.1/gems/cucumber-1.0.2/lib/cucumber/cli/main.rb:20:in `execute'
C:/Ruby192/lib/ruby/gems/1.9.1/gems/cucumber-1.0.2/bin/cucumber:14:in `'
C:/Ruby192/bin/cucumber:19:in `load'
C:/Ruby192/bin/cucumber:19:in `'

August 4, 2011 at 5:52 AM

[Rob](https://www.blogger.com/profile/07966252008096453177) said...

devaru: Try require 'rspec/expectations'

February 17, 2012 at 3:28 PM
