## Tuesday, May 8, 2012

### [Running Mocha Specs in the Browser](http://www.adomokos.com/2012/05/running-mocha-specs-in-browser.html)

The way I've been [writing JavaScript code](http://www.adomokos.com/2012/01/javascript-testing-with-mocha.html) might not appeal to other developers: I live my life in the terminal, I write and execute specs in it hitting the browser occasionally just to make sure everything I do works there. Others use the terminal as little as possible. One thing we have in common: we test our JavaScript code with [Mocha](http://visionmedia.github.com/mocha/).

I still remember the cold, autumn day when I first [downloaded Jasmine's](http://pivotal.github.com/jasmine/download.html) standalone test-runner a couple of years ago. All I had to do was unzip the file, hook in my own JS source and spec files and I was in business. It even had simple JavaScript objects like Player and Song guiding me through the first steps.

Mocha - unfortunately - does not have a downloadable zip file to help you get started and its browser based tool is deeply buried under its own tests. Extracting it from there takes time and effort.
This writing describes how you can get started with executing your Mocha specs in the browser.

I put all the code of this starter project in a Github repo: [mocha-in-browser](https://github.com/adomokos/mocha-in-browser). You can either clone it or just [download the project zip file](https://github.com/adomokos/mocha-in-browser/blob/master/mocha-in-browser.zip?raw=true).

I used the [String Calculator Kata](http://osherove.com/tdd-kata-1/) as an example. Open up the public/index.html file in your favorite browser and you should see this:

![string_calculator](/resources/2012/05/string_calculator.jpg)

Just type 1,3 in the text box, hit the "Calculate" button and the correct answer appears in green color under the text box.

When you open up the [spec/runner.html](https://github.com/adomokos/mocha-in-browser/blob/master/spec/runner.html) in the browser, Mocha is happily reporting the test output:

![test_output](/resources/2012/05/test_output.jpg)

I did not finish the String Calculator Kata. Please work through it not only to sharpen your skills but to get familiar with JavaScript unit testing with Mocha as well.

You can drop the spec directory from this project into your working directory, reset your JavaScript source and spec files in the spec/runner.html file and start using it.

Both Mocha and [Should.js](https://github.com/visionmedia/should.js) are rapidly changing projects. I'd encourage you to download and use the latest source code of those projects as ofter as you can.
I created a [Makefile](https://github.com/adomokos/mocha-in-browser/blob/master/Makefile) to make this super simple for you. Just run "make" in the project root and you should have the latest version of those files brought to you by node.js and npm.


POSTED BY ATTILA DOMOKOS AT 7:03 AM

#### 5 comments:

[Tom](https://www.blogger.com/profile/17591494041073898938) said...

_very helpful. thank you!_

July 20, 2012 at 1:58 PM

[Attila Domokos](https://www.blogger.com/profile/09067995287578229487) said...

_Awesome! Thanks for the comment!_

July 20, 2012 at 2:22 PM

[DoubleU](https://www.blogger.com/profile/12826179399655363650) said...

_Much more helpful than the mocha docs! Rather than the crude assert function you provided, you could use something like this: https://github.com/Jxck/assert_

July 3, 2013 at 5:29 PM

[Carl-Erik](https://www.blogger.com/profile/07120809714327784013) said...

_Could you happen to update your github project with Expect.JS assertions? https://github.com/LearnBoost/expect.js/_

_Or perhaps you could accept a pull request?_

July 31, 2013 at 3:47 AM

[Carl-Erik](https://www.blogger.com/profile/07120809714327784013) said...

_You say you do most of your development in the terminal (which I like to do too), but I would like to see how to accomplish doing tests using Mocha that work both in the browser and in Node._

_This was a super-smooth no-brainer operation when using BusterJS, but that project seems to have lost traction in a swamp of refactoring, and so I am trying to find another framework to use for testing._

_I do not find the idea of testing code in an environment in which it will never run very appealing, so have the code exercised by multiple environments (node+various browsers) seems essential to me. How do achieve this?_

_Right now all my code has a section to make it exportable to Node for testing as well as some code to hook it into the browser environments._

```javascript
/** Export module if we are running in Node, add it to the global namespace otherwise */
if (typeof window !== "undefined") {
  window.asm_md_parser = { parse : parse };
} else {
  module.exports = { parse : parse };
}

// The only reason I do this is so that my tests can import the required code
var expect = require("expect.js");
var AbstractStateMachine = require("../src/abstract-state-machine");
...
```

_How is your workflow?_

July 31, 2013 at 5:48 AM
