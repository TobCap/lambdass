
lambda syntax-sugar (lambdass)
==============================

What is this package?
---------------------

The purpose of this package is to provide you with easy syntax for making an anonymous function.

Related packages and functions
------------------------------

-   `rlang::new_function()`, `rlang::as_closure()` <https://github.com/tidyverse/rlang>
-   `pryr::f()`, `pryr::make_function()` <https://github.com/hadley/pryr>
-   `lambda.r::"%as%"` <https://github.com/zatonovo/lambda.r>
-   `lambdaR::lambda()` <https://github.com/hoxo-m/lambdaR>

Similar notation
----------------

Family of `purrr::map` such as `map_if()` or `map_dbl()` can accept formula notation with `..1` or `.x` which is converted to a closure by `rlang::as_closure()`. See comparison of usage below.

``` r
microbenchmark::microbenchmark(
  1 %>% Map(~~ .. + 1, .), # base::Map with lamdass's double-tilde
  1 %>% map(~~ .. + 1),    # purrr::map with lambdass's double-tilde
  1 %>% map(~~ ..1 + 1),   # purrr::map with lambdass's double-tilde
  1 %>% map(~ ..1 + 1),    # purrr::map with rlang:::as_closure
  1 %>% map(~ .x + 1)      # purrr::map with rlang:::as_closure
)
#> Unit: microseconds
#>                    expr     min       lq      mean   median       uq       max neval
#>  1 %>% Map(~~.. + 1, .) 389.624 413.9200  515.4274 432.6435 547.4355  1203.644   100
#>     1 %>% map(~~.. + 1) 555.014 579.7550  686.9734 601.3765 668.2450  1282.103   100
#>    1 %>% map(~~..1 + 1) 560.364 578.6410  741.5386 595.5810 661.7810  5325.899   100
#>     1 %>% map(~..1 + 1) 748.934 769.2175 1511.5446 814.9115 990.7775 58708.359   100
#>      1 %>% map(~.x + 1) 749.825 773.0075  974.7198 803.0985 917.8900  4316.621   100
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

Double-tilde with dotted symbol placeholder makes an anonymous function like the usage of `%` in Closure's lambda, `_` in scala's lambda, or `#` in Mathematica's Pure Function. See details in the documents below.

-   <http://en.wikibooks.org/wiki/Clojure_Programming/Examples/API_Examples/Function_Tools#.25>
-   <http://www.scala-lang.org/files/archive/spec/2.11/06-expressions.html#placeholder-syntax-for-anonymous-functions>
-   <http://reference.wolfram.com/language/howto/WorkWithPureFunctions.html>

A bounded vairable can be specified by `..` which is synonym for `..1`. Arguments can be designated by `..1`, `..2`, up to `..5`.

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

Double-tilde cannot create curried-function such as `function(x) function(y) x + y`. The "De Bruijn index" is suitable for a notation of lambda culculas, but double-tilda is degined to cooporate with normal use of R's functions, not curried-function. See <https://en.wikipedia.org/wiki/De_Bruijn_index> for "De Bruijn index".

### f.

A function name `f()` is used in many situation, so I avoid using it and `f.()` is adopted.

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
#>              expr    min       lq      mean  median       uq     max neval
#>   f.(x, y, x + y)  5.796  10.9225  16.38816  12.929  18.0555 241.621   100
#>  f.r(x, y, x + y) 92.280 150.4565 177.52479 193.698 198.6020 651.751   100
```

### Arrow-notation

right-hand side always needs `{` because R cannot control strength of associativity for `%infix-function%`.

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
#>              expr    min     lq      mean   median     uq    max neval
#>              ~~..   5796   7579  11346.22   9139.5  13375  62412   100
#>             ~~..1   8025  12037  18701.76  17387.0  19170  45917   100
#>   as_closure(~.x) 222898 228247 275822.29 232482.5 247862 826057   100
#>  as_closure(~..1) 225126 229585 274658.73 234934.0 248754 784152   100
#>          identity      0      1    299.40      1.0    447   5350   100
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
#>                 expr    min       lq      mean   median       uq    max neval
#>            (~~..)(1)   7580  10254.0  18706.28  16050.0  24965.5  44580   100
#>           (~~..1)(1)   9808  17164.0  22223.62  19170.0  26526.0  54387   100
#>   as_closure(~.x)(1) 231367 237609.0 332268.69 249868.5 433089.5 902733   100
#>  as_closure(~..1)(1) 232259 237385.5 332219.62 245633.0 439999.0 631691   100
#>          identity(1)    892   1784.0   2408.02   1784.0   3122.0   5350   100
```

Church Encoding
---------------

By using f.(), R notation is almost as-is with the article written in wikipedia.

<https://github.com/TobCap/lambdass/blob/master/vignettes/ChurchEncoding.md>
