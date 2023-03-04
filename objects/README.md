# Introduction #

In this project we will explore the design of a simple Object-Oriented
Language. The language is based on the Sigma calculus, which is a
core OOP language described by Abadi and Cardelli in the book:

[A Theory of Objects](https://link.springer.com/book/10.1007/978-1-4419-8598-9)

Unfortunatelly the book is not freely available online, but there are a few
sets of slides available with information about the Sigma calculus. For example:

[Slides for A Theory of Objects](
http://lucacardelli.name/Talks/1996-10%20A%20Theory%20of%20Objects%20%28OOPSLA%20Tutorial%29.pdf)

In part two of the slides above a technical description of the Sigma calculus is given.
Interested students can have a look for further reference, but the project bundle
should be self-contained and explain the major concepts involved.

The Sigma calculus and the Theory of Objects play a similar role to
the lambda calculus for Object-Oriented Languages. It aims to capture
the key concepts in Object-Oriented Programming in a minimal language.
In the Sigma calculus, there are 4 basic constructs:

1. Variables
2. Objects
3. Method Calls
4. Method Updates

The first 3 constructs should be familiar to anyone that has used an
OOP language before. The 4th construct is related to method overriding, but it
is more significantly more powerful.  

**Variables** are just like the variables we covered
in the course and that appear in essentially all programming languages.

**Objects** are collections of methods. An important point of the Sigma
calculus is that classes are not needed to build objects: instead objects
can be built directly. In the base Sigma calculus there is no `new`
construct, which is what languages like Java use to create an object from a class.
In technical terms the Sigma calculus is an object-based language, rather than
a class based language. As a side note the Javascript OOP-model is also object-based,
though in recent versions of JavaScript classes have been added. We will also
add classes to our language later. But for now we focus on the basic Sigma calculus.

Objects are built directly. The following expression gives a first example of
objects in the Sigma calculus:

```javascript
[l1 = {this} this, l2 = {this} 0]
```
In this example, we create an object. The syntax for objects is minimalistic, and
it is basically a collection of components enclosed by square brackets. 
Each component starts with a label (the name of the method or field) and has a
method definition.

An important point about the Sigma calculus is that method definitions
take a special argument, which is basically the self-reference to the object.
For instance the method definition for `l1` is `{this} this`. The first part (`{this}`)
introduces a variable called `this`, which is the self-reference. The `this`
in the Sigma calculus plays a similar role to `this` in languages like Java.
But the Sigma calculus is different from those languages in that:

* All methods have to explicitly introduce a variable for the self-reference;
In contrast in languages like Java, `this` is a keyword and it is implicitly
available in a class.

* You can name the self-reference with any name. For instance, we could have
written the same program above as:

```javascript
[l1 = {x} x, l2 = {x} 0]
```

In both examples, the first method (`l1`), returns the object itself.
The second method `l2` returns `0`. The two versions of the program
behave in exactly the same way. The only difference is syntactic:
in one program the self-reference is called `this`, while in the other
it is called `x` (note that you could even choose different names for
each method). 

**Method calls**, allow calling a method in an object, and they are
similar to method calls in standard OOP languages. A simple
example that illustrates methods calls is:

```javascript
[l1 = {x} x, l2 = {x} 0].l1.l2
```

In this example, right after the creation of the object, we call methods `l1`
and `l2`. Since `l1` just returns the object itself, the subsequent method
method call to `l2` will just return `0`.

Another example is a small variant of the earlier program:

```javascript
[l1 = {this} this, l2 = {this} 0, l3 = {this} this.l2]
```

In this program the method `l3` calls `l2` via the self reference `this`
with the call expression `this.l2`. 

The most unusual construct is the **method update** construct.
A method update allows *replacing* an existing method implementation
in an object by another one. Some dynamically typed languages, offer similar
functionality, but most statically typed OOP languages do not
allow this. An example of a method update is:

```javascript
[ contents = {x} 0, set = {this} this.contents <~ {y} 10]
```

In this code, the `set` method, when executed, will replace the implementation
of contents by `{y} 10`. Thus, if you execute:

```javascript
[ contents = {x} 0, set = {this} this.contents <~ {y} 10].set.contents
```

You should get `10`. Note that in the example, the self reference variables
`x` and `y` are unused. In our concrete syntax, we can omit unused self-reference
variables. Thus, you can write the code as:

```javascript
[ contents = 0, set = {this} this.contents <~ 10]
```

Our parser will translate it to the correct form. Notice also, how the code above
looks like mutable update of `contents` in the method `set`. This is not a
mere coincidence: the method update construct generalizes assignment and mutable
updates. 


## Project Bundle ##

The structure of fold is:

```
-- app
 |-- Main.hs
-- src
 |-- Declare.hs
 |-- Parser.hs
 |-- Tokens.hs
 |-- Target.hs
 |-- Source.hs
 |-- Testcase.hs
-- test
  -- testcases
   |-- (some examples)
```

In `app` folder, there is only one file `Main.hs`, for calling the functions in `src` folder.

The `src` folder is the core. 

`Tokens.hs` contains all tokens we might use, generated from `Tokens.x`.

`Parser.hs` is the parser of concrete syntax, generated from `Parser.y`.

`Declare.hs` contains all the definition of abstract syntax with a pretty printer.

`Target.hs` contains a partial implementation of the Sigma Calculus.

`Source.hs` is the file that contains the source language.

`Testcase.hs`contains some unit tests that help you check your program.

The `test/testcaes` folder provides some examples help you check the program.
Feel free to add more tests. 

There are several useful commands you might need to know:

`stack clean` delete the executable file and all the object files from the directory.

`stack build` compile the whole project.

`stack run -- (filename)` load the file from test folder and run it. 
For example, if you want to run the file *cell.obj* in test/testcases, 
you should type `stack run -- testcases/cell.obj` when your directory is ".../bundle".

`stack test` run the unit tests from the comments.

# First Part: The Sigma Calculus #

In the bundle you will find an implementation of an interpreter for the Sigma
calculus in the file `Target.hs`. The definitions of the syntax and other
definitions can be found in the file `Declare.hs`.

## Abstract Syntax ##

We start with the abstract syntax:

```haskell
data Method = Method Var SigmaTerm
  deriving Eq
  
data SigmaTerm = SigmaVar Var              -- variables:    x, y ... 
  | Object [(Label, Method)]               -- objects:      [l = {this} 0]
  | Call SigmaTerm Label                   -- calls:        x.l
  | Update SigmaTerm Label Method          -- update:       x.l <~ {this} 0 
  | Let Var SigmaTerm SigmaTerm  -- variable declarations:  var x = 1; 2
  | Lit Int                                -- numbers       1,2, 100
  | Boolean Bool                           -- booleans      true, false 
  | Binary BinaryOp SigmaTerm SigmaTerm    -- binary op     1 + 2, 10 * 6
  | Unary UnaryOp SigmaTerm                -- unary op      -(5), not true
  | If SigmaTerm SigmaTerm SigmaTerm -- conditionals  if (x > 0) then 1 else 2
  deriving Eq
```

The first 4 constructors correspond to the 4 language constructs of the Sigma calculus
that were already presented in the introduction.
In addition to those, we also have some other constructs that were
covered in the classes: local variable declarations; integers, booleans,
unary and binary operations and conditionals. 
For example, we support code such as

```javascript
var x = 10; [ l = x + 10 ].l
```
If we evaluate this code, the result should be `20`.

## Interpreter ##

The interpreter uses a standard environment that keeps track of local variables
and the values associated with them. Moreover, it also uses a memory model, like the
interpreter in Lecture 12.

The type `Mem` is a model of memory:

```
type Object = [(Label, MethodClosure)]

type Mem = [Object]
```

It is used to store objects in memory. For this language we have to have memory,
just as in the interpreter with mutable state, because objects are mutable. That is,
the method update operation can actually modify methods stored in objects.
Therefore the memory stores all the objects that are allocated in the program.

Notice that the objects stored in memory are essentially collections
of method closures; each method has a name (the label) and a method
closure. The method closure, like with function closures, stores the
environment at the point of definition of the method.

We use a `replace` operation:

```replace :: Int -> Label -> MethodClosure -> Mem -> Mem```

to update a method in an object stored in memory.

The values in the language are:

```
data Value = VInt Int | VBool Bool | ObjRef Int deriving Eq
```

We have 3 values: integers, booleans and object references. Whenever
evaluation evaluates an expression that computes an "object" what is
returned is not the object itself, but rather a reference to the location
of the object in memory. 

The type signature for the evaluator for the Sigma calculus is:

```evaluateS :: SigmaTerm -> Env -> Mem -> Maybe (Value, Mem)```

Basically we have 3 inputs and two outputs. The types of the inputs are:

1. `SigmaTerm`: The expression to be evaluated
2. `Env`: The current environment
3. `Mem`: The current memory

The outputs are:

1. `Value`: The value that is computed
2. `Mem`: The updated memory (in case method update operations have been performed)

The main point is that when objects are created, memory is allocated
to store the object information (the methods) in memory. To access the objects in memory
we use object references. 


The semantics of `clone(a)` is that it returns a *new object* with the same methods as the object denoted by
the expression `a`. Any change of the cloned value should not affect the value of `a`. 
During the evaluation, you should retrieve the value of `a`, and allocated a fresh object with the same
methods as `a` in memory.

An example using clone is:

```javascript
var o1 = [l = {this} 0]; var o2 = (clone(o1).l <~ 10); o1.l + o2.l
```

In this example, we create an object `o1`, and then create a clone of the object, but
with an updated implementation of `l`. The update of `l` should not affect `o1`
(only the cloned object). Thus the final result should be 10. 


In the monadic interpreter the code completely encapsulates the memory management. For
instance, the refactored code for binary operations in the interpreter for Lecture 12 is:

```
evaluate (Binary op a b) env = do
   v1 <- evaluate a env
   v2 <- evaluate b env
   return (binary op v1 v2)
```

In such code all the memory management is implicitly dealt by the bind and return
operations of the Monad. Moreover, there are a few little auxiliary functions, such
as:

```
newMemory :: Value -> Stateful Value
newMemory val = ST (\mem-> (AddressV (length mem), mem ++ [val]))
```

That are helpful to encapsulate memory management even in cases where the memory
needs to be changed. For instance, for mutable cells, we used the following code:

```
evaluate (Mutable e) env = do     -- @Mutable(2+3)
  v <- evaluate e env
  newMemory v
```

We refactor the code in `Target.hs` and write in
monadic style, so that all memory management is encapsulated. You can create
auxiliary functions, such as `newMemory` to help you with this.



# Second Part: An Extended Source Language 

## Abstract Syntax

Realistic programming languages are often built around a small core like the Sigma
Calculus or the Lambda Calculus, but providing many convenience source level features
that can be encoded in terms of the small calculus. We will take a similar approach
in the project. In addition to the Sigma Calculus, which will be our core language,
we will have another, richer, language that translates to the Sigma calculus.
In essence we will have the following architecture:

```Source --> Sigma```

The first thing that you might notice is that we do not have first-class functions
in the Sigma Calculus. The reason is that we can encode lambda expressions with
the existing constructs in the Sigma calculus.
We use the concrete syntax `\var -> exp` to represent function expressions, just like
Haskell and `a(b)` to represent function application.

The Abstract Syntax for the Source language is:

```haskell
data SMethod = SMethod Var Exp
  deriving Eq

data Exp = SLit Int
         | SBool Bool
         | SUnary UnaryOp Exp
         | SBin BinaryOp Exp Exp
         | SIf Exp Exp Exp
         | SVar Var
         | Lam Var Exp                 -- new!
         | Apply Exp Exp               -- new!
         | SObject [(Label, SMethod)]
         | SCall Exp Label
         | SUpdate Exp Label SMethod
         | SLet Var Exp Exp
         deriving Eq
```

There are only two new constructs: `Lam` is used for defining lambda expressions, and
`Apply` is used to define function applications. We have seen such constructs in the class. 
For example, the code 

```javascript
(\x -> x)(5+6)
```
will evaluate to 11.

The code 

```javascript
[contents=0, set={this}\n->this.contents<~n].set(3).contents
```
will evaluate to 3.

Concretely, we implement a function

```haskell
translate :: Exp -> SigmaTerm
```

It converts terms in the source language to terms in the target language.
The translation of most constructs are straightforward, since they already exist in the target.
Only the two new constructs for first-class functions are more challenging.
We give some *pseudo-code* for what the translation next. You can also
look at [slide 61](http://lucacardelli.name/Talks/1996-10%20A%20Theory%20of%20Objects%20(OOPSLA%20Tutorial).pdf) for a more formal definition of the translation (the first two cases
in the slides can be ignored).

For lambda expressions `\ x -> b`, the translation proceeds as follows:

```javascript
translate (\x -> b) ~->
[ arg = {x} x.arg, val = {x} (substitute (translate b) x (x.arg)) ]
```

The idea is to encode a lambda expression using an object with two fields/methods.
The first field (`arg`) stores the argument. For the second field, the translation
first translates the lambda body `b`, and then substitures all the occurrences of `x`
by `x.arg` in the translated body. A simple example is:

```javascript
\x -> (x + 1)   ~->
[arg = {x} x.arg, val = {x} x.arg+1]
```

For function applications `b(a)`, the translation proceeds as follows: 

```javascript
translate ( b(a) ) ~->
var f = clone(translate b); y = translate a; (f.arg <~ {z} y).val
```

The translation is also tricky.
We firstly translate `b`, then clone the value and assign it to `f`.
We then translate `a`, and have a method update that updates the function argument
with `y` `(f.arg <~ {z} y)`. 
Then we perform the method call by invoking `val`.
A simple example of a translation of a function application is:

```javascript
(\x -> (x + 1) ) (5) ~->
let f = clone([arg={x} x.arg, val={x} x.arg+1]) in 
  let y = 5 in (f.arg<~{_} y).val```


## Classes

In modern OOP languages, classes are a basic feature. We will also add
support for classes in our source language. For that, we need three new
constructs that we add to the source language.
 
```haskell
data Exp = ...
	| Top                          --- new
	| Class [(Label, SMethod)] Exp --- new
	| SNew Exp                     --- new
```

`Top` represent an empty class. `Top` is like `Object` in Java.
We use it to tell the interpreter that our class does not inherit from any class (or you can say it inherits from the `Top` class).

For example, 

```javascript
class { l = {x} exp }    // exp refers to an expression
```
will be interpreted to `Class [Label "l", SMethod (Var "x") exp] Top`

Another example of a class definition in concrete syntax is:

```javascript
class { 
  contents = {x} 0, 
  set = {x} \ n -> x.contents <~ n
}
```

Classes look like objects, but instead of defining methods, they define pre-methods.
Unlike objects in the Sigma calculus, we cannot simply define a class and immediately
call a method. We must first create an instance of the class using `new`. This is
the same as class-based languages like Java. 

In `Source.hs` we define a function called `classGen`.
`classGen` transforms source expressions: it takes a source expression with classes,
and produces another source expression where classes are encoded in terms of objects.

The procedure of `classGen` is basically to generate an object using 2 steps:
1) it changes all the pre-methods into methods that have a self-reference argument;
2) it adds adds a `new` method to create an object instance.
In other words: assume that you have a class declaration with multiple methods:

```javascript
class {l_i = {x_i}  b_i}        // for i from 1 to n
```

`classGen` should return

```javascript
[ new = {z} [li = {x} z.li(x) ],
  li = \xi -> bi ]                  // for i from 1 to n
```

For example, you have class definition:

```javascript
class { 
  contents = {x} 0 , 
  set = {x} \ n -> x.contents <~ n
}
```

Then the `classGen` function should return

```javascript
[ new = {z}
    [contents = {x} z.contents(x), set = {x} z.set(x)],
  contents = \ x -> 0 , 
  set = \ x -> \ n -> x.contents <~ n
]
```

Object instances are created with `new`.
An example of `new` is:

```javascript
var cell = class { 
  contents = {x} 0 , 
  set = {x} \ n -> x.contents <~ n
};
(new cell).set(3).contents
```

It will evaluate to 3.

We denote a class as `cell`, and we create a new an instance and do some operations on that.

The expression `new cell` actually does two things:
1) applies `classGen` to `cell` and get an object;
2) invokes `new` component from the object.

For example, the result of `classGen` of `cell` will be:

```javascript
[new={z} [set={x} z.set(x), contents={x} z.contents(x)], 
    set = \x -> \n -> x.contents<~n, 
    contents = \x -> 0]
```

For solve this question, we might need to change the function signature of translation function:

```haskell
-- before: 
translate :: Exp -> SigmaTerm
-- new:
type TClass = [(Var, Exp)]
translate :: Exp -> TClass -> SigmaTerm
classGen :: Exp -> TClass -> Exp
```

We add a new context `TClass` for storing class definitions.
When you have class definition, you should store it to the context.
When you use new on a class, you look up the class definition from the context.

The translation for `new` operation is (in pseudo-code):

```haskell
translate (SNew a) tc = Call (translate (classGen a tc) tc) (Label "new")
```

## Single Inheritance 


Our classes can easily support single inheritance. We describe
the functionality of classes and the encoding of classes as objects next.

An example of single inheritance using concrete syntax as:

```javascript
var cell = class { 
  contents = {x} 0 , 
  set = {x} \ n -> x.contents <~ n
};

var gcell = class { 
  get = {x} x.contents 
} extends cell;

(new gcell).set(5).get
```

After running the `classGen` function  we should get:

```javascript
[new = {z} 
  [get = {x} z.get(x), set = {x} z.set(x), 
   contents = {x} z.contents(x)], 
 get = \x -> x.contents, 
 set = super.set, 
 contents = super.contents]
```

We can even support method overriding.

In the example below,

```javascript
var cell = class { 
  contents = {x} 0 , 
  set = {x} \ n -> x.contents <~ n
};

var uncell = class { 
  undo = {x} x, 
  set = {x} super.set(var y=clone(x); x.undo <~ y) 
} extends cell;

(new uncell).set(3).set(5).undo.contents
```

After running the `classGen` function  we should get:

```javascript
[new = {z} 
  [get = {x} z.get(x), set = {x} z.set(x), 
   contents = {x} z.contents(x), undo = {x} z.undo(x)], 
 set = \ x -> super.set(var y=clone(x); x.undo <~ y) , 
 contents = super.contents,
 undo = \ x -> x]
```

Remember that our class definition is 

```javascript
Class [(Label, SMethod)] Exp
```

With single inheritance enabled, `Exp` refers to the super class, otherwise it refers to `Top`.

For supporting single inheritance, we could just modify `classGen` a little bit:

1. when meeting the `super`, refer it to the super class. You already store all the class definition in context.

2. for every pre-method `m_i` in super classes, check if it is defined in the sub class. If no, change the pre-method to `m_i = super.m_i` in sub class.

3. you don't need to change other things.
