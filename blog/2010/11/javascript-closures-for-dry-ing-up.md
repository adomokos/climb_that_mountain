## Tuesday, November 9, 2010

### [JavaScript Closures for DRY-ing Up The Logic](http://www.adomokos.com/2010/11/javascript-closures-for-dry-ing-up.html)

Our #Hackibou today was a real blast. We focused on JavaScript development using the great [Jasmine BDD](http://pivotal.github.com/jasmine/) framework. Since most of us were a little rusty on JS, we decided to start with something very simple: a Calculator. Our task was to develop a calculator that accepts an array of integer numbers in its add(), subtract(), multiply() and divide() functions.

We started out with a couple of very simple specs:

```javascript
describe("Calculator", function() {
  var calculator;
  beforeEach(function() {
   calculator = new Calculator();
  });

  describe("Arithmetic operations on an array input", function() {
   it("creates a new Calculator object", function() {
    expect(calculator).not.toBeNull();
   });

   it("adds two numbers together", function() {
    expect(calculator.add([1,0])).toEqual(1);
   });

   it("adds three numbers together", function() {
    expect(calculator.add([1,2,3])).toEqual(6);
   });

   it("multiplies two numbers", function(){
    expect(calculator.multiply([1,2])).toEqual(2);
   });
  });
});
```

And our - not too elegant - solution was this:

```javascript
function Calculator() {
  this.add = function(input) {
    var result = 0;
    for(i = 0; i<input.length; ++i) {
      result += input[i];
    }
    return result;
  }

  this.multiply = function(input) {
    var result = 1;
    for(i=0; i<input.length; ++i) {
      result *= input[i];
    }
    return result;
  }
}
```

Look at the code above. 90% of the code is duplicated there. One of us suggested assigning the first element of the array to the result right on the declaration. With this change the only difference between the two functions is the operation. One uses addition and the other multiplication. I played with JS closures a little bit before, so I proposed this:

```javascript
function Calculator() {
 var operator = function(result, input) { return result + input; };
 this.add = function(input){
  return operation(input, operator);
 };

 this.multiply = function(input){
  return operation(input, function(result, input){return result*input;});
 }

 function operation(input, operator) {
  var result = input[0];
  for(i = 1; i < input.length; i++){
   result = operator(result, input[i]);
  }
  return result;
 }
}
```

Check out the operation() function. It uses two parameters, the first one is the array of integers and the other is a function object that holds the calculation logic. It's invoked on line 14\. The variable result is both passed in as the first input and is assigned as the result of the function call. One of us suggested using the [shift() function](http://www.w3schools.com/jsref/jsref_shift.asp) on the input array, this way we did not have to start our for loop with the second element of the array. Our operation() function now looked like this:

```javascript
function operation(input, operator) {
 var result = input.shift();
 for(i = 0; i < input.length; i++){
  result = operator(result, input[i]);
 }
 return result;
}
```

Adding subtraction and division was very simple:

```javascript
this.subtract = function(input){
  return operation(input, function(result, input){return result-input;});
 }

 this.divide = function(input){
  return operation(input, function(result, input){return result/input;});
 }
```

Please note that there is no [if statement](http://www.antiifcampaign.com/) in the Calculator object.

Our final solution can be found in [this gist](https://gist.github.com/665954).


POSTED BY ATTILA DOMOKOS AT 8:10 AM
