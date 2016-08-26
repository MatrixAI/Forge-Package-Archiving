To add parallelism, we need to understand the basics of lazy evaluation...
Haskell is lazy; expressions are not evaluated until they are bound to IO monad.
This is usually fine, as there are no restrictions on how the program should be run, just that things are bound when they are bound.

When there are still pending functions, a variable is called unevaluated.

You can check if a variable is unevaluated in ghci using the :sprint function. Unevaluated variables return `_`, otherwise called a thunk.

Unevaluated variables at the low level in haskell are actually just pointers in memory to thunks.
For `x = 1 + 2 :: Int`, here's how it might look like in memory, where x is a pointer.
![http://orm-chimera-prod.s3.amazonaws.com/1230000000929/images/pcph_0201.png]

after we evaluate by typing `x` in ghci to show x, the thunk will be evaluated to 3. Once the thunk is evaluated, it stays evaluated. This is the basis of lazy evaluation.

Now consider:
`let x = 1 + 2 :: Int
let y = x + 1
:sprint x
x = _
:sprint y
y = _
`
with the following diagram for good measure:
![http://orm-chimera-prod.s3.amazonaws.com/1230000000929/images/pcph_0202.png]

To do evaluation manually, we can use the seq function. Trying:
`seq y ()`

By the way, running seq on a polymorphically constrained type... Normal Form just means you can write it in some sort of constructor form....
seq forces evaluation to the WHNF (Weak Head Normal Form), which can be thought of as anything that can be pattern matched, i.e. a constructor form, lambda function, or some other type....

seq always evaluates its first argument, and returns its second argument...

Have a look at the following:
`let x = 1 + 2 :: Int
let z = (x,x)
:sprint z
z = (_,_)
`
As shown below in  memory:
![http://orm-chimera-prod.s3.amazonaws.com/1230000000929/images/pcph_0203.png]

`let z = swap (x, x + 1)`

so z is a thunk...
seq z ()
will evaluate the outermost function in this case swap...
this produces the pair (_,_)
which is still unevaluated, but only inside the tuple.


