
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

Family of tidyverse functions such as `dplyr::mutate_if` and `purrr::map` whose formal arguments include `.predicate`, `.f`, and/or `.funs` for a closure object can accept formula notation with `..1` or `.x` which is converted to a closure by `rlang::as_closure()`. See the comparison of usage below.

``` r
if ("package:lambdass" %in% search()) detach("package:lambdass")
library(purrr)
microbenchmark::microbenchmark(
  1 %>% map(~ ..1 + 1),    # purrr::map with rlang:::as_closure
  1 %>% map(~ .x + 1),     # purrr::map with rlang:::as_closure
  times = 1e3
)
#> Unit: microseconds
#>                 expr     min       lq     mean   median       uq       max neval
#>  1 %>% map(~..1 + 1) 721.294 739.7955 877.2752 751.8315 794.6280 51680.843  1000
#>   1 %>% map(~.x + 1) 723.523 740.9090 834.7126 751.3855 795.5195  6694.482  1000

suppressPackageStartupMessages(library("lambdass"))
microbenchmark::microbenchmark(
  1 %>% Map(~~ .. + 1, .), # base::Map with lamdass's double-tilde
  1 %>% map(~~ .. + 1),    # purrr::map with lambdass's double-tilde
  1 %>% map(~~ ..1 + 1),   # purrr::map with lambdass's double-tilde
  times = 1e3
)
#> Unit: microseconds
#>                    expr     min       lq     mean  median       uq      max neval
#>  1 %>% Map(~~.. + 1, .) 305.815 320.5260 365.0700 326.768 378.4795 3759.377  1000
#>     1 %>% map(~~.. + 1) 434.204 450.6980 522.5550 458.722 548.3270 6059.672  1000
#>    1 %>% map(~~..1 + 1) 446.240 457.6075 526.1281 464.518 555.6820 3918.080  1000
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
#>              expr    min      lq      mean   median      uq     max neval
#>   f.(x, y, x + y)  4.458  7.5790  11.15902  11.1455  15.604  24.965   100
#>  f.r(x, y, x + y) 75.340 89.3825 138.00521 152.9080 161.601 391.854   100
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
#> execution time for 6 evaluations.
#> Unit: nanoseconds
#>              expr    min       lq      mean   median       uq    max neval
#>              ~~..   6242  12483.0  17444.76  15158.0  25634.0  37002   100
#>             ~~..1   8025  17386.0  22709.53  19615.5  28755.0  49038   100
#>   as_closure(~.x) 228693 240506.5 361820.16 410131.0 436432.0 590231   100
#>  as_closure(~..1) 226909 236495.0 353113.87 404558.5 421944.5 738235   100
#>          identity      0      1.0    433.19    446.0    447.0  10254   100
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
#>                 expr    min     lq      mean   median       uq    max neval
#>            (~~..)(1)   7134   8471  12893.10  14712.0  15603.5  33882   100
#>           (~~..1)(1)   9362  10700  16793.83  16272.0  18724.0 101196   100
#>   as_closure(~.x)(1) 230922 236717 268127.74 239392.0 243404.0 472542   100
#>  as_closure(~..1)(1) 230922 235380 263032.29 239614.5 243850.0 573292   100
#>          identity(1)    892   1338   1948.94   1784.0   2230.0   7580   100
```

Church Encoding
---------------

By using f.(), R notation is almost as-is with the article written in Wikipedia.

<https://github.com/TobCap/lambdass/blob/master/vignettes/ChurchEncoding.md>
