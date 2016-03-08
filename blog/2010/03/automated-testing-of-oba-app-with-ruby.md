## Saturday, March 6, 2010

### [Automated Testing of an OBA App with Ruby](http://www.adomokos.com/2010/03/automated-testing-of-oba-app-with-ruby.html)

Hello!

You could call a me a late adopter, or a busy dad, but what matters is that it's finally here. I am writing my first blog post!!
I was thinking what the best topic could be for The First one? How I got into programming? Or what I worked on yesterday? Maybe a little bit of both.

I've been doing .NET development as a day job but I am hacking out Ruby/Rails code in the evenings.
I am test infected, I can't live without tests and it really bothers me when I see code written without supporting tests around it. I've seen far too many projects starting out with everything running smooth: features are being developed with the speed of light, everybody is happy. And then, maybe 4-6 months into the project development speed slows down, defect rates go up and the code is just plain fragile. Nobody dares to touch it, not knowing what could go wrong. Changing anything is close to impossible.

I admire the testing enthusiasm I see in the Ruby community. The way they practice TDD, BDD, the way they won't write a single line of code without a failing test first. And while Ruby is really flexible, it can be very dangerous. I'd say not having sufficient test coverage in a dynamic language is like you sitting in car running towards a cliff.

I work on an OBA ([Office Business Application](http://msdn.microsoft.com/en-us/office/aa905528.aspx)) project at work. We have decent unit test coverage, however, not having automated functional testing is a risk. Sure we have QA staff, they can just go through the test scripts manually over and over. I think what could automated should be automated.
I bumped into the win 32 OLE automation object in ruby ([http://ruby-doc.org/core/classes/WIN32OLE.html](http://ruby-doc.org/core/classes/WIN32OLE.html)) a couple of months ago but I never had the time to fully investigate it. I was able to open Excel, read and write a value in the cell, save the worksheet and close it. This was all cool, but not enough.

<pre class="brush: ruby" name="code">require 'win32ole'
require 'test/unit'
ExcelTest < Test::Unit::TestCase

 def setup
  @excel = WIN32OLE::new("Excel.Application")
  @excel.visible = true
  @workbook = @excel.Workbooks.Open("C:\\path_to_your_excel_file\some_file.xls")
  @sheet1 = @workbook.worksheets(1)
  @sheet2 = @workbook.worksheets(2)
 end

 def test_should_verify_captions
  assert_equal('Category 1', @sheet1.cells(2, "A").value)
  assert_equal('Child 1', @sheet1.cells(3, "A").value)
  assert_equal('Child 2', @sheet1.cells(4, "A").value)
  assert_equal('Child 3', @sheet1.cells(5, "A").value)
  assert_equal('Child 4', @sheet1.cells(6, "A").value)
 end
end
</pre>

I am trying to use Ruby unit testing. It's easy and simple for now. In the setup method I create an Excel object with the tested workbook loaded into it. I create two instance variables for the worksheets to keep my code simple.

Then in the first test, I verify caption cells. Nothing magical here, I just make sure that in the 2nd row's "A" column the cell should have a value of "Category1".

To be continued...

#### 1 comment:

[Ryan K](https://www.blogger.com/profile/13399971190233079313) said...

Attila,

Truly great post. Congratulations!!

I have one question, though.

I kept changing Gherkin script and Excel content and all works as expected.

However, when I change the path to Excel file, `@@excel_file_path = ...` , in excel_handler.rb and run "cucumber features" it has no effect what so ever: The file in C:\Temp continues to be used.

What should I do to effectively change directory and file name of Excel file?


(May 25, 2010 at 9:22 PM)
