### Scenario Outline in RSpec

Some business logic is best described in a table format. The rows and columns can better detail all the different permutations, than words and sentences. Gherkin handles this well with its [scenario outlines](https://github.com/cucumber/cucumber/wiki/Scenario-Outlines), however, RSpec does not have suchfeature.

As I wanted to test all the different permutation of a business logic, I ended up with a ~200 lines spec file. It was hard to review what use cases were covered as I had to scroll up and down to review the file. Had RSpec have a table format, this logic could have been described in 40 lines, which can easily fit into a screen.

It was obvious a simple DSL was needed, but would that look like. I tried this first, which looked OK, but couldn't easily report which line or use case triggered the failure as the entire content was one, long string with wrapped lines:

```ruby
RSpec.describe 'eating apples' do
  def method_under_test(start, eat, left)
    start - eat == left
  end

  def example_runner(examples)
    lines = examples.split(/\n/)
    lines.delete_at(0) # Don't need the title
    lines.each do |line|
      example = line.split('|')
      start, eat, left = example.reject(&:blank?).map(&:to_i)
      expect(method_under_test(start, eat, left)).to eq(true)
    end
  end

  it 'can tell how many are remaining' do
    examples =
      <<~TABLE
        | start | eat | left |
        | 12    | 5   | 7    |
        | 20    | 5   | 15   |
      TABLE

    example_runner(examples)
  end
end
```
I am using the `example_runner` method to parse the examples table and run the specs one by one. The spec itself is clear, the examples TABLE is easy to grok.

The spec documentation, however, does not reveal the used table. This is what it looks like:

```shell
eating apples
  can tell how many are remaining
```
Let's say, one of the examples will trigger an error. When I change the second example and change the "left" columns 15 value to 16, this is what I get when I run the spec.

```shell
F

Failures:

  1) eating apples can tell how many are remaining
     Failure/Error: expect(method_under_test(start, eat, left)).to eq(true)

       expected: true
            got: false

       (compared using ==)
```
This is not very useful. I don't know which example failed and why it failed. Fortunately RSpec lets us provide more context for an expectation. When I change the line of the assertion with an added context like this:
```ruby

```


