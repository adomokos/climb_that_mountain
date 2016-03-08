## Monday, April 12, 2010

### [Put on Your Hat and Dance to Sinatra](http://www.adomokos.com/2010/04/put-on-your-hat-and-dance-to-sinatra.html)

It's been almost two weeks since we had our kick-off/planning meeting. We ended up with screen shots - very vague, drawn on a white board - and some stories in [Pivotal Tracker](http://www.pivotaltracker.com/). I immediately started to look for css templates. I am not a designer, but I do like simple and clean design. Ended up picking one from [http://www.freecsstemplates.org/](http://www.freecsstemplates.org/).

My goal was not to jump into code and create models, database tables and such. I'd like to have a rough HTML prototype that can help us figuring out what we really need. I emphasize HTML, because I want to be able to change it easily. I don't want to have a migration script created when we need to add one more field to a form.

I started out with an index and the user signup page. When I added the confirmation page I quickly realized that I'll have serious duplication unless I do something about it. My header and footer html content was repeated in three HTML files. I know this is only a prototype helping us discovering how our web app should work. But still. Even my prototype code should be [DRY](http://en.wikipedia.org/wiki/Don't_repeat_yourself)-ed up. I badly needed a template solution.

I could have created a skeleton Rails app, but that just seemed too heavy for this purpose.

I remember Matt Otto's ([@matthewotto](http://twitter.com/matthewotto)) tweet from late March:

![sinatra_matthewotto1](/resources/2010/04/sinatra_matthewotto1.png)

I saw the tweet, I did not really pay attention to it then. But after I created my thrid page and noticed the duplication, I knew I had to find something. I can't recall why I looked at [Sinatra](http://www.sinatrarb.com/) a couple of days later, but I was blown away. It is exactly what I needed! I put a simple app together with it and I was convinced: I found my template engine for the prototype!

Here is the only Ruby file I had in my sample app:

<pre class="brush: ruby"># myapp.rb
require 'rubygems'
require 'sinatra'

get '/' do
  'Hello, World!'
end

get '/hello/:name' do
  # matches "GET /hello/foo" and "GET /hello/bar"
  # params[:name] is 'foo' or 'bar'
  "Hello #{params[:name]}!"
end

get '/test' do
  @data = 'Hello from test!'
  erb :index
end
</pre>

The first block is executed when you make a request - assuming you run the app locally - to http://localhost:4567\. There is no template rendering, you'll only see the string "Hello, World!" in the browser.

Ok, this works, but how could I use this for my templates?

Digging into the [Sinatra documentation](http://www.sinatrarb.com/intro) I found the solution. I had to create the following directory structure:

<div style="margin-bottom: 0px; margin-top: 0px;">-|</div>

<div style="margin-bottom: 0px; margin-top: 0px;">-|-views</div>

<div style="margin-bottom: 0px; margin-top: 0px;">        |- index.erb</div>

<div style="margin-bottom: 0px; margin-top: 0px;">        |- layout.erb</div>

<div style="margin-bottom: 0px; margin-top: 0px;">-|-myapp.rb</div>

layout.erb is my template:

<pre class="brush: xml" name="code"><html>
<head>
<title>Something Test</title>
</head>
<body>
<div id="content">
<%= yield %>
</div>
<div id="footer">
<hr/>
</div>
</body>
</html>
</pre>


And index.erb provides the page content:

<pre class="brush: xml" name="code"><div>
This is the content: <%= @data %>
</div>
</pre>

Please note that I set the @data variable in the "get '/test'" block.

And that's it. I tell Sinatra to grab the index.erb file and since I have layout.erb it knows it has to render that as a template. There is a whole lot more to Sinatra than this of course. I found the "[Sinatra Book](http://sinatra-book.gittr.com/)" really helpful. I'd recommend taking a look at it if you want to give Sinatra a try.

I used HTML in ERB first, but then Matt Otto tweeted that he went with ([HAML](http://haml-lang.com/)) so I started playing with that.

HAML looked cryptic, but once you get the hang of it, it's really simple.

In the end, I started using HAML and SASS for my prototypes, I'll blog about them in my follow-up blog post.


POSTED BY ATTILA DOMOKOS AT 8:16 AM

#### 2 comments:

[shoorik](http://www.google.com/profiles/shoorik) said...

Seems great for rapid development and prototyping, but how does Sinatra scale with more complicated business logic? Seems you'd need fuller MVC framework.

April 15, 2010 at 11:42 AM

[adomokos](https://www.blogger.com/profile/09067995287578229487) said...

Absolutely! I would never attempt to do anything more than a prototype or a simple web site with it. The good thing is: I can reuse my ERB or HAML files in the final web app. This way the prototype UI code won't be thrown away.

April 19, 2010 at 6:42 AM
