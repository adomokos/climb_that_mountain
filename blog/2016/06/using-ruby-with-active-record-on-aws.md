### Using Ruby with ActiveRecord in AWS Lambda

I showed in the previous blog post how to run MRI Ruby on AWS Lambda. In this writing I'll guide you through adding gems to the project: first faker, and then the mysql2 gem with active_record, and finally we will have the Ruby code talk to an RDS instance.

(1) Add faker to the project

Let's jump in by editing our Ruby app, let's add a Gemfile to it in the `hello_ruby` directory:

```ruby
source 'https://rubygems.org'

gem 'faker'
```

Run `bundle install` in that directory, but provide the following arguments to the command: `--path vendor`. This is very important, as we have to package all the files in the `vendor` directory.

Edit the `lib/hello.rb` file:

```ruby
#!/usr/bin/env ruby

require 'faker'

puts "Hello - '#{Faker::Name.name}' from Ruby!"
```

We required the faker gem and used it to generate a fake name. When I run the app in the terminal with `bundle exec ruby lib/hello.rb`, I see the following output:

```shell
Hello - 'Jamar Gutmann II' from Ruby!
```

You will get a different name between the single quotes, but that's the point, faker generates random name for us.

[Commit point](https://github.com/adomokos/aws-lambda-ruby/commit/df038e90b3dc56fe380b7e4b6afa97721000f3ff)

(2) Use faker with Traveling Ruby

The `run` target in the Makefile will have to copy all vendorized gems, plus it needs to configure the app to run with the correct bundler settings. This step is heavily influenced by how Traveling Ruby packages gems for deployment, please review [their tutorial](https://github.com/phusion/traveling-ruby/blob/master/TUTORIAL-2.md) as reference.

Add a `bundler_config` template file to the `resources` directory with this content:

```shell
BUNDLE_PATH: .
BUNDLE_WITHOUT: development:test
BUNDLE_DISABLE_SHARED_GEMS: '1'
```

Changed the `resources/wrapper.sh` file to set the Gemfile's location:

```shell
#!/bin/bash
set -e

# Figure out where this script is located.
SELFDIR="`dirname \"$0\"`"
SELFDIR="`cd \"$SELFDIR\" && pwd`"

# Tell Bundler where the Gemfile and gems are.
export BUNDLE_GEMFILE="$SELFDIR/lib/vendor/Gemfile"
unset BUNDLE_IGNORE_CONFIG

# Run the actual app using the bundled Ruby interpreter, with Bundler activated.
exec "$SELFDIR/lib/ruby/bin/ruby" -rbundler/setup "$SELFDIR/lib/app/hello.rb"
```

Modify the Makefile's `run` target with the following changes:

```shell
...

run: ## Runs the code locally
	@echo 'Run the app locally'
	@echo '-------------------'
	@rm -fr $(OSXDIR)
	@mkdir -p $(OSXDIR)/lib/ruby
	@tar -xzf resources/traveling-ruby-20150715-2.2.2-osx.tar.gz -C $(OSXDIR)/lib/ruby
	@mkdir $(OSXDIR)/lib/app
	@cp hello_ruby/lib/hello.rb $(OSXDIR)/lib/app/hello.rb
	@cp -pR hello_ruby/vendor $(OSXDIR)/lib/
	@rm -f $(OSXDIR)/lib/vendor/*/*/cache/*
	@mkdir -p $(OSXDIR)/lib/vendor/.bundle
	@cp resources/bundler-config $(OSXDIR)/lib/vendor/.bundle/config
	@cp hello_ruby/Gemfile $(OSXDIR)/lib/vendor/
	@cp hello_ruby/Gemfile.lock $(OSXDIR)/lib/vendor/
	@cp resources/wrapper.sh $(OSXDIR)/hello
	@chmod +x $(OSXDIR)/hello
	@cd $(OSXDIR) && ./hello

...
```

Run the target with `make run` and you should see something similar to this in the terminal:

```shell
$: make run
Run the app locally
-------------------
Hello - 'Kelly Huel' from Ruby!
```

[Commit point](https://github.com/adomokos/aws-lambda-ruby/commit/2e1d816c206f7cf22ab080939d0b8ef2949db97b)

