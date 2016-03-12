## Monday, January 23, 2012

### [JavaScript Testing with Mocha](http://www.adomokos.com/2012/01/javascript-testing-with-mocha.html)

JavaScript is a neat and powerful language. Sure it has its flaws, but serious software can be developed with it. The way I prefer developing JavaScript applications is by test driving the code with some kind of testing tool. And I am not thinking about hitting the browser's refresh button. No, I mean executing the specs [right in the terminal](http://www.adomokos.com/2010/11/running-jasmine-bdd-specs-in-terminal.html).

I recently started playing with [Visionmedia's mocha](https://github.com/visionmedia/mocha) testing framework. The code is well maintained and the authors are responding fairly fast to pull requests or issues.
I would recommend it as an alternative to [Jasmine](http://tryjasmine.com/).

This blog post will show you the first couple of steps you need to take to test drive your JavaScript code with mocha in the CLI. All my instructions are for OS X, but setting it up should be very similar on Linux and (maybe) on Windows as well.

First of all, you need [node.js](http://nodejs.org/) to run JavaScript code in the terminal. You can download the source code and compile it yourself, but I'd recommend using [Homebrew](http://mxcl.github.com/homebrew/) and let it do the job for you.

```shell
$: brew install node
```

At the time of this writing my current node version is 0.6.6\. You can check your node.js version by running this command:

```shell
$: node -v
v0.6.6
```

Next you need [node's package management tool (npm)](http://npmjs.org/). Your version of node may include npm, I list this step here in case it does not. Installing it is super easy, just follow the instructions on their web site and in your terminal.

With these two steps you're ready to roll. Create the project directory, cd into it and start moving in. Create a "src" and a "test" directory. You need to install mocha and [should.js](https://github.com/visionmedia/should.js) as npm packages. Having [sinon.js](https://github.com/cjohansen/Sinon.JS) - an excellent spying framework - wouldn't hurt either. Create your spec and source file and you are ready to test drive your app with mocha.

I really wanted to help you - Dear Reader - so I created [this shell script](https://raw.github.com/gist/1658930/17e51bef9a33e8f39560d118c9e4f1f49e6cabc3/mocha_sample_project_install.sh) to make your life easier. Create a directory, cd into it and run the command below in your terminal:

   `curl -L http://git.io/setup_mocha_project | sh`

If everything goes OK, you will see this:

<div style="padding: 10px;">create the src directory...
create the test directory...
write the package.json file...
install npm packages...

create a sample spec file...
create a sample src file...
run the spec with mocha...
  <span style="color: #090">.</span>

  <span style="color: #090">✔ 1 tests complete (1ms)</span>

run the spec with list reporter...

  <span style="color: #090">✓</span> Person should be able to say hello: 1ms

  <span style="color: #090">✔ 1 tests complete (2ms)</span>
</div>

Let's add one more feature to our Person object. Open up the test/person_spec.js file - it was created by the shell script above - and add the "can say good night" spec:

```javascript
var should = require('should');
var Person = require(__dirname + '/../src/person');

describe('Person', function() {
  it('should be able to say hello', function() {
    var Person = global.theApp.Person();
    var personInstance = new Person();
    var message = personInstance.sayHelloTo('adomokos');

    message.should.equal('Hello, adomokos!');
  });

  // Add this spec
  it('can say good night', function() {
    var Person = global.theApp.Person();
    var personInstance = new Person();
    var message = personInstance.sayGoodNight();

    message.should.equal('Good night!');
  });
});
```

Run the mocha specs with this command:

```shell
$: ./node_modules/mocha/bin/mocha
```

The error is obvious: the Person object does not yet have the method "sayGoodNight".

  .<span style="color: #900">.</span>

  <span style="color: #900">✖ 1 of 2 tests failed</span>:

  1) Person can say good night:
     <span style="color: #900">TypeError: Object [object Object] has no method 'sayGoodNight'</span>

Let's fix it by adding the missing method to the Person object:

```javascript
global.theApp = {};

global.theApp.Person = function() {

  var Person = function() {
   this.sayHelloTo = function(anotherPerson) {
      return 'Hello, ' + anotherPerson + '!';
    };

   // Add this method
   this.sayGoodNight = function() {
     return 'Good night!';
   };
  };

  return Person;

};
```

When I run the specs again, they all pass.

  <span style="color: #090">..</span>

  <span style="color: #090">✔ 2 tests complete (2ms)</span>

You can try other reporters as well. The "list" reporter will give you the documentation text:

```shell
$: ./node_modules/mocha/bin/mocha -R list
```

Try the [landing reporter](http://twitpic.com/88zi7h/full), I found its output unexpected but really cool!

```shell
$: ./node_modules/mocha/bin/mocha -R landing
```

**The steps once more:**

1.  Make sure you have node.js installed
2.  Check for npm as well
3.  Create your project directory and cd into it
4.  Run this script $: `curl -L http://git.io/setup_mocha_project | sh`
5.  Execute the specs with $: `node_modules/mocha/bin/mocha`

And I almost forgot: mocha will pick up CoffeeScript files as well.

Enjoy!

::: Update (01/24/2012):  
I asked [TJ Holowaychuck](http://tjholowaychuk.com/), the author of Mocha of what thoughts he had on my blog post. He recommended adding a "test" script to the package.json file making it easier to run the specs. [I made that change](https://gist.github.com/1658930): `npm test` executed in the terminal should run all your specs under the test directory.


POSTED BY ATTILA DOMOKOS AT 7:25 PM

#### 6 comments:

[Shahrier](https://www.blogger.com/profile/16746869348438490733) said...

_Looking into JS BDD frameworks. Any idea how Mocha stacks up against Jasmine?_

January 27, 2012 at 12:24 PM

[Attila Domokos](https://www.blogger.com/profile/09067995287578229487) said...

_Mocha BDD UI and Jasmine are very close syntactically. But Mocha offers a wide variety of interfaces (bdd, tdd, xunit) and all sorts of reporters. Just run this command in your terminal to see the different reporters you can use:_

```shell
$: ./node_modules/mocha/bin/mocha --reporters
```

_It's built for JS testing in the terminal (what I needed the most) although it has browser based testing tools as well._

_There is a project called jasmine-node that runs your Jasmine specs on node.js, but that project is not well maintained._

_I also fell in love with the code of Mocha. It's the kind of JavaScript code I'd like to write in the future._

January 27, 2012 at 1:04 PM

[Shahrier](https://www.blogger.com/profile/16746869348438490733) said...

_Thanks! I've been reading about people using phantom.js with jasmine on top of it for headless JS testing. Since mocha's already running off node, this is great. Thanks for this again._

January 27, 2012 at 3:08 PM

[@quartzmo](https://www.blogger.com/profile/15222755899473111565) said...

_I really like Mocha as well. I now use it to run tests in the browser for the Rails project I work on in my day job. I extracted the integration code into a gem:_

https://github.com/quartzmo/mocha_rails

_I wrote about Mocha CLI testing with a Makefile for CoffeeScript on my blog: http://www.coffeescriptlove.com/2012/02/testing-coffeescript-with-mocha.html_

February 22, 2012 at 8:07 PM

[Attila Domokos](https://www.blogger.com/profile/09067995287578229487) said...

_@quartzmo: Thank you for the links!_

February 23, 2012 at 4:34 AM

[Ken Collins](https://www.blogger.com/profile/04397597320910156592) said...

_I just finished my mocha-phantomjs project here on github. http://metaskills.net/mocha-phantomjs/_

_It was a bit of a chore getting all the reporters, especially the dot reporter, to work correctly. But I am really happy with the current status. Feedback and help appreciated._

September 9, 2012 at 7:53 PM
