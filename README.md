## Pico - a tiny functional language

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

Going further off of SQL inspiration, we could create aliases for some of these functions:

```
foo = a * b
bar = a / b
qux = (foo f)^(bar b)
lux = qux(f.a = 12, f.b = 3, b.a = 4, b.b = 8) <-- = (12 * 3)^(1/2) = 6
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

This would also allow the writer to indicate off the bat which unbound variables there were:

```
bsearchr = (Int start, finish, target, Vector(Int) v, <-- 4 unbound variables
            mid = (start + finish)/2, <-- not unbound
            if target == v[mid] then True, else
            if start == finish then False, else
            if target > v[mid] then bsearchr(mid, finish, target, v), else
            if target < v[mid] then bsearchr(start, mid, target, v)
          )
bsearch = bsearchr(0, len(Vector(Int) v), Int target, v) <-- here we've inlined the variable declarations

<-- Now we can use it for some stuff.
contains5 = bsearch(,5)
v = {1,2,3,4,5,6,7,8,9,10}
vcontains = bsearch(v)
vcontains(6) <-- true
vcontains(11) <-- false

<{ note that the below is a meaningless function; it's just there 
   to illustrate combining two functions. }>
usableVector = (contains5 || vcontains)
usableVector({1,2,3,4}, 5) <-- true
usableVector({1,2,3}, 12) <-- false

<{ one interesting thing is that we could make the OR operator short-circuit, so 
   that the following resolves to True even though it has unbound variables: }>
usableVector({1,2,3,4,5}) <-- contains 5, so resolves to true.
usableVector({1,2,3,4}) <-- is a function equivalent to vcontains
f = (if Int i != i then Int j, else 1) <-- given any argument, f will always resolve to 1
f(123) <-- 1
```

As a functional language, Pico will have algebraic types. Consider a List which is either `Empty` or `Cons(elem, List)`, and a `when` statement which acts as a pattern matcher. Note that this vector usage might not be how we actually do it.

```
len = (when List l is Empty: Int acc, when l is Cons(_, next): len(next, acc+1))
fillVector = (List(Int) l, Vector v, Int posn
               when l is Empty: v,
               when l is Cons(elem, next): listToVector(next, add(v, posn, elem), posn - 1)
             )
listToVector = (ln = len(List(Int) l, 0),
                v = Vector(Int)(ln),
                fillVector(l, v, ln - 1))

<{
   Note: one curious question is what would happen if we defined it as such:
   listToVector = (ln = len(, 0),
                   v = Vector(Int)(ln),
                   fillVector(l, v, ln - 1))
   The call to len supplies one argument, so ln is a function which takes a List as an argument. Then passing in a list to listToVector, the list would be passed to the first unbound variable, which is l in the len function. Then the l on the third line of listToVector would be undefined, which would either mean listToVector requires two (identical or same-length) lists as arguments, or if we require a type declaration in front of every unbound variable, then we have an error.
}>                
```

Pico is designed to be a very small language with minimal syntax. Currently the entire grammar is here (this will be expanded slightly to include algebraic types, pattern matching and possibly interfaces)

```
pico -> expression '.'
      | pico expression '.'

expression -> term
            | var '=' term ',' expression
            | IF term THEN term ',' ELSE expression

term -> literal
      | '(' expression ')'
      | invocation
      | term op term
      | unary_op term
      | term ( '(' term (',' term)* ')' )*

literal -> (typename)? var 
         | int_literal 
         | float_literal
         | string_literal
         | char_literal

op -> '+' | '-' | '*' | '/' | '%' | '>' | '<' | '&' | '|'
      | ">=" | "<=" | "==" | "!=" | "&&" | "||"

unary_op -> '-' | '!' | '~'

var ->            [a-z_][a-zA-Z_]*
typename ->       [A-Z][a-zA-Z_]*
int_literal ->    [0-9]+
float_literal ->  ([0-9]*\.[0-9]+)
string_literal->  \"(\\.|[^\\"])*\"
char_literal ->   \'(\\.|[^\\'])*\'
```