
lambda syntax-sugar (lambdass)
==============================

What is this package?
---------------------

The purpose of this package is to provide you with an easy syntax for making an anonymous function.

Related packages and functions
------------------------------

-   `rlang::new_function()`, `rlang::as_closure()` <https://github.com/tidyverse/rlang>
-   `pryr::f()`, `pryr::make_function()` <https://github.com/hadley/pryr>
-   `lambda.r::"%as%"` <https://github.com/zatonovo/lambda.r>
-   `lambdaR::lambda()` <https://github.com/hoxo-m/lambdaR>

Similar notation
----------------

Family of `purrr::map` such as `map_if()` or `map_dbl()` can accept formula notation with `..1` or `.x` which is converted to a closure by `rlang::as_closure()`. See the comparison of usage below.

``` r
microbenchmark::microbenchmark(
  1 %>% Map(~~ .. + 1, .), # base::Map with lamdass's double-tilde
  1 %>% map(~~ .. + 1),    # purrr::map with lambdass's double-tilde
  1 %>% map(~~ ..1 + 1),   # purrr::map with lambdass's double-tilde
  1 %>% map(~ ..1 + 1),    # purrr::map with rlang:::as_closure
  1 %>% map(~ .x + 1)      # purrr::map with rlang:::as_closure
)
#> Unit: microseconds
#>                    expr     min       lq     mean   median       uq       max neval
#>  1 %>% Map(~~.. + 1, .) 147.558 157.1430 203.0687 168.5110 257.2240   486.362   100
#>     1 %>% map(~~.. + 1) 206.849 219.1085 298.4507 236.7175 363.0995  1407.817   100
#>    1 %>% map(~~..1 + 1) 209.078 221.5605 279.1835 238.5010 361.3165   542.086   100
#>     1 %>% map(~..1 + 1) 277.730 304.0320 617.2780 330.1110 503.0790 21250.987   100
#>      1 %>% map(~.x + 1) 279.514 298.4595 381.8811 324.9850 483.0185   566.604   100
```

Installation
------------

``` r
# install.packages("devtools")
devtools::install_github("tobcap/lambdass")
library("lambdass")
```

Usage
-----

can be written in three ways.

``` r
~~ .. + 1
f.(x, x + 1)
x %->% {x + 1}

# all means `add one`
function(x) x + 1
```

### Double-tilde

Double-tilde with dotted symbol placeholder makes an anonymous function like the usage of `%` in Closure's lambda, `_` in Scala's lambda, or `#` in Mathematica's Pure Function. See details in the documents below.

-   <http://en.wikibooks.org/wiki/Clojure_Programming/Examples/API_Examples/Function_Tools#.25>
-   <http://www.scala-lang.org/files/archive/spec/2.11/06-expressions.html#placeholder-syntax-for-anonymous-functions>
-   <http://reference.wolfram.com/language/howto/WorkWithPureFunctions.html>

A bounded variable can be specified by `..` which is a synonym for `..1`. Arguments can be designated by `..1`, `..2`, up to `..5`.

``` r
~~ .. + 1
~~ ..2 / ..1

Map(~~ .. ^ 2, 1:10)
Reduce(~~ ..1 + ..2, 1:10)
```

The placeholder must be in order.

``` r
~~ ..2 + 1
#> Error in ~~..2 + 1: 
#> Tail-prefix number of placeholders must be in order and
#> the number of arguments is limitted to five
```

Double-tilde cannot create curried-function such as `function(x) function(y) x + y`. The "De Bruijn index" is suitable for a notation of lambda calculas, but double-tilde is designed to cooperate with normal use of R's functions, not curried-function. See <https://en.wikipedia.org/wiki/De_Bruijn_index> for "De Bruijn index".

### f.

A function name `f()` is used in many situations, so I avoid using it and `f.()` is adopted.

``` r
# f.(...arguments, body)
f.(x, x + 1)
f.(x, y, x + y)
# can define curried-function
f.(x, f.(y, x + y)) 

Map(f.(x, x ^ 2), 1:10)
Reduce(f.(x, y, x + y), 1:10)
```

`f.()` is now implemented by C. The conceptual R code is defined by `f.r()`.

``` r
identical(f.(x, y, x + y), f.r(x, y, x + y))
#> [1] TRUE

## about ten times speed-up
microbenchmark::microbenchmark(
  f.(x, y, x + y), 
  f.r(x, y, x + y)
)
#> Unit: microseconds
#>              expr    min     lq      mean median      uq      max neval
#>   f.(x, y, x + y)  1.784  4.459  11.31057  6.241  6.9105  156.029   100
#>  f.r(x, y, x + y) 39.676 66.201 113.22799 68.876 84.0330 1084.617   100
```

### Arrow-notation

right-hand side always needs `{` because R cannot control the strength of associativity for `%infix-function%`.

``` r
x %->% {x + 1}

# left-hand side is written in two ways:
{x; y} %->% {x + y}
`any-valid-varname`(x, y) %->% {x + y}
# but recommend to use just `f`, for readability and simplicity
f(x, y) %->% {x + y}

Map(x %->% {x ^ 2}, 1:10)
Reduce({x; y} %->% {x + y}, 1:10)
```

Arguments can be checked when using `:` with the result of `tyepof(actual_argument)` as symbol

``` r
{x:integer} %->% {x * 2}
#> function (x) 
#> {
#>     if (!(is.integer(x))) 
#>         stop("some actual arguments are not appropriate type")
#>     x * 2
#> }
f(x:character) %->% {paste(x, "test")}
#> function (x) 
#> {
#>     if (!(is.character(x))) 
#>         stop("some actual arguments are not appropriate type")
#>     paste(x, "test")
#> }
```

When the default value is set to a parameter, its type-checking syntax is automatically added at head of function-body.

``` r
{x = 1L} %->% {x * 2}
#> function (x = 1L) 
#> {
#>     if (!(is.integer(x))) 
#>         stop("some actual arguments are not appropriate type")
#>     x * 2
#> }
f(x:character, y = "") %->% {paste0(x, y)}
#> function (x, y = "") 
#> {
#>     if (!(is.character(x) && is.character(y))) 
#>         stop("some actual arguments are not appropriate type")
#>     paste0(x, y)
#> }
```

Benchmarking
------------

Note that it's nanoseconds

``` r
microbenchmark::microbenchmark(
   "f." = f.(x, x), 
   "~~" = ~~ ..,
   "%->%" = {x} %->% {x},
   "as.function" = as.function(alist(x=, x)),
   "as.function.default" = as.function.default(alist(x=, x)),
   "function(x) x" = function(x) x
)
# Unit: nanoseconds
#                 expr    min       lq      mean   median     uq    max neval
#                   f.   5350   6687.5   8073.96   8025.0   8916  34326   100
#                   ~~  10699  12037.0  13704.19  13374.0  14712  28977   100
#                 %->% 138194 141983.5 160380.99 145772.5 170514 324978   100
#          as.function  32543  33881.0  41360.74  36110.0  40567 188122   100
#  as.function.default  22736  24295.5  27447.78  26302.0  29423  66869   100
#        function(x) x      0    446.0    910.25    447.0    892  38338   100

microbenchmark::microbenchmark(
   "f." = f.(x, y, x + y), 
   "~~" =  ~~ ..1 + ..2,
   "%->%" = f(x, y) %->% {x + y},
   "as.function" = as.function(alist(x=, y=, x + y)),
   "as.function.default" = as.function.default(alist(x=, y=, x + y)),
   "function(x, y) x+y" = function(x, y) x + y
)
# Unit: nanoseconds
#                 expr    min     lq      mean   median       uq    max neval
#                   f.   6241   7134   8947.70   8471.0   9362.5  20507   100
#                   ~~  13374  14712  19263.05  18724.0  20506.0  98965   100
#                 %->% 113676 119916 131948.24 122814.5 128163.0 327652   100
#          as.function  33434  35663  41761.75  39006.5  44133.0  74001   100
#  as.function.default  23627  25411  30964.91  27639.0  32766.0  73109   100
#   function(x, y) x+y      0    446    602.64    447.0    892.0   4904   100
```

Comparison with `rlang::as_closure()`

``` r
microbenchmark::microbenchmark(
  ~~ ..,
  ~~ ..1,
  as_closure(~ .x),
  as_closure(~ ..1),
  identity
)
#> Warning in microbenchmark::microbenchmark(~~.., ~~..1, as_closure(~.x), : Could not measure a positive
#> execution time for 29 evaluations.
#> Unit: nanoseconds
#>              expr   min       lq      mean   median       uq    max neval
#>              ~~..  1784   4459.0   6589.57   5796.0   8471.0  30315   100
#>             ~~..1  2675   6242.0   9455.96   7579.5  11145.0 110112   100
#>   as_closure(~.x) 84256 144660.5 147897.28 151348.0 157588.5 205066   100
#>  as_closure(~..1) 82027 115238.5 141928.11 150679.0 156920.0 279068   100
#>          identity     0      0.0    183.36      1.0      1.0   7134   100
```

Apply closure

``` r
microbenchmark::microbenchmark(
  (~~ ..)(1) ,
  (~~ ..1)(1) ,
  as_closure(~ .x)(1),
  as_closure(~ ..1)(1),
  identity(1)
)
#> Unit: nanoseconds
#>                 expr   min      lq      mean  median       uq    max neval
#>            (~~..)(1)  2229  3344.5   5805.03  5350.0   6687.5  29424   100
#>           (~~..1)(1)  3121  4013.0   6718.93  6242.0   8025.5  14266   100
#>   as_closure(~.x)(1) 84701 87376.5 109108.82 91834.0 153799.5 167619   100
#>  as_closure(~..1)(1) 83810 87599.0 107530.68 89382.5 135745.0 197933   100
#>          identity(1)     0   446.0    593.67   447.0    892.0   8025   100
```

Church Encoding
---------------

By using f.(), R notation is almost as-is with the article written in Wikipedia.

<https://github.com/TobCap/lambdass/blob/master/vignettes/ChurchEncoding.md>
