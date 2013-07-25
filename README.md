# Pico - a small language expressing a small idea

The main idea behind Pico is that the only difference between a function and a constant expression is that a function leaves some arguments unspecified. What do I mean by that? Well consider the expression:

```
1 < 2
```

This value is the boolean constant true, no matter what, because every argument has been specified. Now consider:

```
a < b
```

This value can be true or false, since we can choose any `a` and `b`. Then a function is simply an expression, the only difference being that it might not be constant.

Expressions can be conditional, like a ternary operator in C:

```c
unsigned fact(unsigned i) { return i <= 1 ? i : i * fact(i-1); }
```

We are essentially saying that the statement `i <= 1 ? i : i * fact(i-1)` itself is a function, because we don't know what `i` is. But it can be made into a constant expression by specifying any `i`, in C by calling `fact(/*some number*/)`.

We therefore don't need to separate functions and expressions, because from a programmatic point of view, all that matters is whether there are any unresolved symbols in an expression, and then it becomes a function.

```
fact = if i <= 1 then 1 else i * fact(i-1)
```

Even though we didn't say it, because there is an unresolved symbol in fact (namely `i`), we know that fact is a function which takes one variable; i.e. it's a function which requires one variable to become a constant expression. We can also see that the way to make this function into an expression is to satisfy its unresolved symbol:

```
a = fact(10)
a = 3628800 <-- equivalent
```

However, what if we only provide some of an expression's arguments? Consider:

```
foo = a - b * c
foo' = foo(2)
```

So then foo is a function with three unresolved symbols. What then, is `foo'`? Functional programmers will know this as currying: `a` is no longer unresolved, so `foo` has two unresolved symbols, so `foo'` is a function on two variables. Then the statement `bar = foo'(3,4)` resolves to `2 - 3 * 4 = -10`. But what about `b` and `c`? If we set `b` equal to 2 instead of `a`, then we still have two unresolved symbols. But now calling this function on `(3,4)` would resolve to `3 - 2 * 4 = -5`. We can see that on a function with initially `n` symbols unresolved, we can potentially construct `2^n - 1` child functions from it (not `2^n` since providing all `n` arguments results in a constant expression). This is an extended idea of currying. Pico provides a syntax for this:

```
foo = a * b - c
foo1 = foo(2) <-- foo with a=2
foo1(3,4) <-- 2

foo2 = foo(,2) <-- foo with b=2
foo2(3,4) <-- 2

foo3 = foo(,,2) <-- foo with c=2
foo3(3,4) <-- 10

foo4 = foo(1,,2) <-- foo with a=1, c=2
foo4(5) <-- 3
```

Or we could do a dictionary-style syntax:

```
bar = (a * b + c^2)^d
bar1 = bar(a=5, c=4)
bar1(6, 2) <-- 2116
bar(a=5, c=4)(b=6, d=2) <-- 2116
```

Of course inside of an expression there might be other expressions, or assignments. Then we could have something like this tail-recursive factorial function:

```
fact = (fact' = if n' < 2 then acc else fact'(n'-1, acc*n'), fact'(n, 1))
fact(10) <-- 3628800
``` 

Here `fact'` is internal to `fact` (and would not be visible to other functions). `fact` has one unresolved symbol, `n`, while `fact'` seems to have two, `n'` and `acc`. But actually `fact'` is constant for any input of `n`, so we can consider its symbols to be resolved. But if `n'` is not provided, then the story changes. So we could write the same thing as

```
fact = (fact' = if n' < 2 then acc else fact'(n'-1, acc*n'), fact'(,1))
fact(10) <-- 3628800
``` 

This works the same way, because `fact(,1)` provides one symbol to `fact'`, leaving still one unresolved. Then `fact` is still a function of one variable. It might be easier to think of the `10` passed into `fact` being effectively handed to `fact'`. This can be illustrated in another example:

```
foo = (bar = i, bar)
foo' = foo(1) <-- what happens here?
```

This one is obvious: `foo'` equals 1. How about this?

```
foo = (bar = baz(i), bar)
foo' = foo(1) <-- what happens here?
```

What is `foo'`? Well, `foo` returns `bar`, which is itself a function with `i` unresolved, so `foo' = bar(1) = baz(1)`. We can think alternatively `foo` having no unresolved symbols, so it passes `i` down the chain. Let's make it a little more complex:

```
foo = (
   bar = (
      buzz = baz(i), herp = derp(j), 
      buzz + herp), 
   bar)
foo' = foo(1) <-- how about this?
```

The easy way to look at it is to find the first unresolved variable and replace it with `1`, and see what we have. `foo` has no unresolved variables, and neither does `bar`, but `buzz` does: `i`. Resolving `i` resolves `buzz`, leaving `bar` as `baz(1) + derp(j)`. Since `foo` resolves simply to `bar`, `foo' = bar = baz(1) + derp(j)`.

Notice another implication of this: just as we can provide arguments to create new functions, we can add arguments or other functions and create new functions.

```
foo = a + b <-- foo is a function of two variables
bar = a - b <-- bar is a function of two variables
baz = foo * bar <-- baz is a function of four variables
baz(3,4) <-- equivalent to foo(3,4) * bar
baz(3,4,5,6) <-- equivalent to foo(3,4) * bar(5,6) = (3 + 4) * (5 - 6) = -1
``` 

Notice a few things here: one, because functions have separate namespaces, combining two functions can result in namespace collisions. So if we want to use dictionary syntax to call the function, we have to indicate which function's variables we're supplying. This is similar to SQL.

```
foo = a * b
bar = a / b
qux = foo^bar
lux = qux(foo.a = 4, bar.b = 5)
bux = lux(,10) <-- second unbound variable is now bar.a, foo.b still unbound
dux = bux(3)   <-- no unbound variables, now we can resolve:
               <-- bux(3) = lux(1,10) = qux(4,3,10,5) = foo(4,3) / bar(10,5)
               <-- = (4 * 3) ^ (10 / 5) = 12^2 = 144
```

I'm currently debating whether to make Pico a dynamic language. They have their benefits. There are many advantages to typing, however, and one in this case is simply that it better indicates which variables are unbound:

```
fact = if Int n < 2 then Int acc else fact(n-1, acc*n) <-- function of two variables
fact1 = fact(,1) <-- partial application leaves fact1 a function of 1 variable
fact2 = fact(acc=1) <-- different syntax, same result
fact3 = fact(Int n, 1) <-- note that this n is distinct from fact's n, so fact3 still has one unbound variable
fact4 = fact(n = Int n, acc = 1) <-- once again Int specifier means n is a new, unbound variable
n = 10
fact5 = fact(n,1) <-- now n is bound, so fact4 is the constant expression 3628800
fact6 = fact3(n) <-- constant expression 3628800
```