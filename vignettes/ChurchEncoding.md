<!--
  %\VignetteIndexEntry{Church Encoding (Lambda Calculus) with lambdass}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
-->
> Church encoding is a means of representing data and operators in the lambda calculus.

from <http://en.wikipedia.org/wiki/Church_encoding>. All the codes written in below refer to the wikipedia's examples.

preliminary
-----------

``` r
# devtools::install_github("tobcap/lambdass")
library(lambdass)

# auxiliary functions
"%|>%" <- function(x, f) f(x) # forward pipe operator
"%<|%" <- function(f, x) f(x) # backward pipe operator

# church numeral to R numeric
to_int <- f.(n, n(f.(n, n + 1L))(0L))
# church boolean to R logical
to_logical <-f.(f, identical(true, f, ignore.environment = TRUE))
```

Church numerals
---------------

``` r
zero <- f.(f, f.(x, x))
one  <- f.(f, f.(x, f(x)))
two  <- f.(f, f.(x, f(f(x))))
three<- f.(f, f.(x, f(f(f(x)))))
# macro for numbers
num  <- f.(n, {
  nested <- (function(n, acc) {
    if (n == 0) acc
    else Recall(n - 1, call("f", acc))
  })(n, quote(x))
  eval(substitute(f.(f, f.(x, nest_fx)), list(nest_fx = nested)), parent.frame())
})

plus <- f.(m, f.(n, f.(f, f.(x, m(f)(n(f)(x))))))
succ <- f.(n, f.(f, f.(x, f(n(f)(x)))))
mult <- f.(m, f.(n, f.(f, m(n(f)))))
exp  <- f.(m, f.(n, n(m)))
pred <- f.(n, f.(f, f.(x, n(f.(g, f.(h, h(g(f))))) (f.(u, x)) (f.(u, u)))))
minus <- f.(m, f.(n, (n(pred))(m)))

identical(three, num(3))
## [1] TRUE
plus(zero)(one) %|>% to_int
## [1] 1
plus(one)(two)  %|>% to_int
## [1] 3
succ(one) %|>% to_int
## [1] 2
num(99) %|>% to_int
## [1] 99
99 %|>% num %|>% to_int
## [1] 99
plus(num(5))(num(3)) %|>% to_int
## [1] 8
minus(num(5))(num(3)) %|>% to_int
## [1] 2
mult(num(5))(num(3)) %|>% to_int
## [1] 15
num(3) %|>% mult %<|% num(5) %|>% to_int
## [1] 15
```

Church booleans
---------------

``` r
true <- f.(a, f.(b, a))
false <- f.(a, f.(b, b))
and <- f.(m, f.(n, m(n)(m)))
or <- f.(m, f.(n, m(m)(n)))
not1 <- f.(m, f.(a, f.(b, m(b)(a)))) # this does not work in R
not2 <- f.(m, m(f.(a, f.(b, b)))(f.(a, f.(b, a)))) # ok due to normal-order
if_ <- f.(m, f.(a, f.(b, m(a)(b))))

is_zero <- f.(n, n(f.(x, false))(true))
leq <- f.(m, f.(n, is_zero(minus(m)(n))))
eq <- f.(m, f.(n, and(leq(m)(n))(leq(n)(m))))

and(true)(true) %|>% to_logical
## [1] TRUE
and(true)(false) %|>% to_logical
## [1] FALSE
and(false)(true) %|>% to_logical
## [1] FALSE
and(false)(fakse) %|>% to_logical
## [1] FALSE

or(true)(true) %|>% to_logical
## [1] TRUE
or(true)(false) %|>% to_logical
## [1] TRUE
or(false)(true) %|>% to_logical
## [1] TRUE
or(false)(false) %|>% to_logical
## [1] FALSE

not2(false) %|>% to_logical
## [1] TRUE
not2(true) %|>% to_logical
## [1] FALSE

if_(true)(one)(two) %|>% to_int
## [1] 1
if_(false)(one)(two) %|>% to_int
## [1] 2

eq(three)(num(3)) %|>% to_logical
## [1] TRUE
eq(plus(one)(two))(three) %|>% to_logical
## [1] TRUE
eq(num(6))(mult(two)(three)) %|>% to_logical
## [1] TRUE
(6 %|>% num) %|>% eq %<|% (two %|>% mult %<|% three) %|>% to_logical
## [1] TRUE
```

Church pairs and list
---------------------

``` r
# pair
pair <- f.(x, f.(y, f.(z, z(x)(y))))
first <- f.(p, p(f.(x, f.(y, x))))
second <- f.(p, p(f.(x, f.(y, y))))

first(pair(1)(2))
## [1] 1
second(pair(1)(2))
## [1] 2

# list
nil <- pair(true)(true)
isnil <- first
cons <- f.(h, f.(t, pair(false)(pair(h)(t))))
head <- f.(z, first(second(z)))
tail <- f.(z, second(second(z)))

# R's pairlist defined in C source code is as:
# CONS(ScalarInteger(1), CONS(ScalarInteger(2), CONS(ScalarInteger(3), R_Nilvalue)))
# The essential difference is whether a constructor funtion is curried or not.
lst <- cons(one)(cons(two)(cons(three)(nil)))

head(lst) %|>% to_int
## [1] 1
head(tail(lst)) %|>% to_int
## [1] 2
head(tail(tail(lst))) %|>% to_int
## [1] 3
lst %|>% tail %|>% tail %|>% head %|>% to_int
## [1] 3
lst %|>% tail %|>% tail %|>% tail %|>% isnil %|>% to_logical
## [1] TRUE
```

SKI Combinator
--------------

``` r
# http://www.angelfire.com/tx4/cus/combinator/birds.html
S <- f.(x, f.(y, f.(z, (x(z))(y(z)) )))
K <- f.(x, f.(y, x))
I <- S(K)(K) # == f.(x, x) == identity()

# SKI boolean logic
# http://en.wikipedia.org/wiki/SKI_combinator_calculus#Boolean_logic
T_ <- K
F_ <- K(I)
NOT <- S(S(I)(K(K(I))))(K(K))
OR <- S(I)(K(K))
AND <- S(S)(K(K(K(I))))

# http://en.wikipedia.org/wiki/Lambda_calculus#Alpha_equivalence
# If 'alpha equivalence' was able to be applied, you could use to_logical() already defined.
to_logical_ski <-f.(f, identical(T_, f, ignore.environment = TRUE))

NOT(T_) %|>% to_logical_ski
## [1] FALSE
NOT(F_) %|>% to_logical_ski
## [1] TRUE

AND(T_)(T_) %|>% to_logical_ski
## [1] TRUE
AND(T_)(F_) %|>% to_logical_ski
## [1] FALSE
AND(F_)(T_) %|>% to_logical_ski
## [1] FALSE
AND(F_)(F_) %|>% to_logical_ski
## [1] FALSE

OR(T_)(T_) %|>% to_logical_ski
## [1] TRUE
OR(T_)(F_) %|>% to_logical_ski
## [1] TRUE
OR(F_)(T_) %|>% to_logical_ski
## [1] TRUE
OR(F_)(F_) %|>% to_logical_ski
## [1] FALSE
```

Recursion and fixed points
--------------------------

![YCombinator](http://upload.wikimedia.org/math/9/c/c/9ccb07cb4f99bef41be7043990ac5eb3.png)

``` r
# Address of λ (lambda) in Unicode is U+03BB or \u03BB
# cat("\u03BB\n")
λ <- f. 
Y <- λ(f, (λ(x, f(x(x))))(λ(x, f(x(x)))))
# Imagine you cannot detect parenthesis and replace comma with period,
# R's syntax is almost the same as the definition from wikipedia at subtitle.

fib <- function(f) function(x) if (x <= 1) x else f(x - 1) + f(x - 2)
FIB <- f.(f, f.(x, if_(leq(x)(one))(x)(plus(f(minus(x)(one)))(f(minus(x)(two))))))

Y(fib)(10)
## [1] 55
Y(FIB)(num(10)) %|>% to_int # Yes, it works
## [1] 55
```

References
----------

-   <http://en.wikipedia.org/wiki/Church_encoding>
-   <http://en.wikipedia.org/wiki/Lambda_calculus#Arithmetic_in_lambda_calculus>
-   <http://en.wikipedia.org/wiki/SKI_combinator_calculus>
-   <http://www.angelfire.com/tx4/cus/combinator/birds.html>
-   <http://www.allisons.org/ll/FP/Lambda/Introduction/>
-   <http://taiju.hatenablog.com/entry/20120529/1338299884>

Rmarkdown options
-----------------

-   <http://www.rstudio.com/wp-content/uploads/2015/03/rmarkdown-reference.pdf>
-   <http://rmarkdown.rstudio.com/html_document_format.html>
