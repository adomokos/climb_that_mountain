### Using Ruby in AWS Lambda

It was May 2015 at the AWS Summit in Chicago, where I first heard about AWS Lambdas. The company I worked with was running its servers on Linode, I had no chance of using it, but I still found the concept fascinating: "just deploy code and we will execute it for you" was the promsie.

Bulk of my work at my current gig is about transforming data: we get some kind of XML, we need to transform and load it into our own data storage. Sure the worker boxes can do the job, but maintaining a series of these instances takes effort. AWS Lambdas would be the perfect solution for us, but Amazon does not support Ruby natively, which is most of our business logic is written in.

AWS Lambda, as of this writing, supports 3 main platforms: Java, Node.JS and Python. I played around running Clojure on AWS Lambda, which worked as the code is compiled into a jar file, but our current code can't support any other language for a service just yet.

Amazon claimed you can run any language on AWS Lambda, Ruby included, but I have not found a comprehensive guide that would show me how. Once you can package up your app to run as an executable, you can run it. I bumped into this [blog post](https://medium.com/@gigq/using-swift-in-aws-lambda-6e2a67a27e03#.gtg1u3lve) that describes how a Swift code base can be bundled, deployed and invoked on AWS Lambda. It was clear to me that this solution would work, I only had to package Ruby with its own interpreter to accomplish the same. I looked for tools that can do this, and found the great [Travelling Ruby](http://phusion.github.io/traveling-ruby/) app. You can package your code and run it as an executable on the user's computer, no local Ruby installation is necessary. I first wanted to try it locally, thinking if it works there (on OSX), it should work on AWS Lambda as well.

I created a project skeleton like this:


