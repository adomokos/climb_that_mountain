## Saturday, November 13, 2010

### [Running Jasmine BDD Specs in the Terminal](http://www.adomokos.com/2010/11/running-jasmine-bdd-specs-in-terminal.html)

I had a pretty bad week with Ruby on Windows 7 64 bit: I tried to set up DBI and ODBC on my work machine but I did not have any luck with that.

I needed something to feel good about, so I decided to set up [Jasmine BDD](http://pivotal.github.com/jasmine/) spec execution in the terminal on OS X.

I downloaded the [standalone zip file](http://pivotal.github.com/jasmine/download.html) from Jasmine's web site and made sure that the sample specs are executing fine with the SpecRunner.html file in the browser.

![spec_runner](/resources/2010/11/spec_runner.jpg)

I wanted to execute the exact same specs but instead of running it in the browser, I wanted to do it in terminal.

[Michael Hines'](http://blog.michaelphines.net/javascript-testing-with-jasmine-presentation) blog post was a pretty good starting point. He used [JazzMoney](https://github.com/pivotalexperimental/jazz_money), so I tried it myself.

I easily found JazzMoney's installation instructions on their github page.

![jazzmoney_install](/resources/2010/11/jazzmoney_install.jpg)

It has prerequisites: I had to install [harmony](https://github.com/mynyml/harmony) first.

I picked ruby-1.9.2-head from [RVM](http://rvm.beginrescueend.com/), created a new gemset called "jasmine" and I got started.
Harmony has dependencies as well, I had to get stackdeck and johnson before I installed harmony.

```shell
$ gem install stackdeck
$ gem install johnson -v "2.0.0.pre3"
```

This is where it turned ugly. Stackdeck got installed fine, but johnson had some issues.

<div style="color: #cc0000;">Building native extensions. This could take a while...
ERROR: Error installing johnson:
ERROR: Failed to build gem native extension.</div>

After Googling the error I found out that johnson is not playing nice with Ruby beyond 1.8.7\. I went back to RVM and started out by installing a new version of Ruby.
Here is what I did:

```shell
$ rvm install 1.8.7-p249
$ rvm 1.8.7-p249
$ rvm gemset create jasmine
$ rvm 1.8.7-p249@jasmine // switched to jasmine gemset
$ gem install stackdeck
$ gem install johnson -v "2.0.0.pre3"
$ gem install harmony // I tested harmony with a quick test in IRB, worked fine
$ gem install jazz_money
```

I did not have any problems with installing the gems under 1.8.7.

I had to create a Ruby script that sets up the test suite and this is the file I ran in the terminal. My run_specs.rb file was placed into the root folder right next to SpecRunner.html:

```ruby
require 'rubygems'
require 'jazz_money'

javascript_files = [
  'spec/SpecHelper.js',
  'spec/PlayerSpec.js'
]

jasmine_spec_files = [
  'src/Player.js',
  'src/Song.js'
]

JazzMoney::Runner.new(javascript_files, jasmine_spec_files).call
```

I ran the file with the following parameters:

```shell
$  ruby run_specs.rb -f n -c
```

Success! This is the output I received in the terminal:

![terminal_jasmine](/resources/2010/11/terminal_jasmine.jpg)

I did one more thing: I created a shell script, this way I did not have to remember all the command line arguments.
I saved this in the specrunner.sh file:

```shell
ruby run_specs.rb -f n -c
```

I can now invoke my specs by running "sh specrunner.sh" in the terminal.


POSTED BY ATTILA DOMOKOS AT 9:06 PM

#### 5 comments:

[cjkihlbom](http://id.elabs.se/cj) said...

_Or, you could just use Evergreen: http://github.com/jnicklas/evergreen_

November 14, 2010 at 8:33 PM

[adomokos](https://www.blogger.com/profile/09067995287578229487) said...

_Awesome! Thanks for the link!_

November 14, 2010 at 8:53 PM

[jcontonio](https://www.blogger.com/profile/06808214946908626736) said...

_Worked great after I realized you were using the downloaded zip and not the Jasmine gem. Thanks a ton!_

November 16, 2010 at 9:28 AM

[adomokos](https://www.blogger.com/profile/09067995287578229487) said...

_Sweet!_

November 16, 2010 at 9:30 AM

[jcontonio](https://www.blogger.com/profile/06808214946908626736) said...

_Now combine this with the watchr gem and get autotest functionality_

https://gist.github.com/701965

November 16, 2010 at 9:56 AM
