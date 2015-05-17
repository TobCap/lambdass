# lambda Syntax-sugar (lambdass)

## What is this package?
The purpose of this package is to provide you with easy syntax for making anonymous
function. 

## Related packages and functions
##### please tell me if you know other packages.
* `pryr::f()`, `pyry::make_function()` from https://github.com/hadley/pryr
* `lambda.r::"%as%"` from https://github.com/zatonovo/lambda.r
* `lambdaR::lambda()` from https://github.com/hoxo-m/lambdaR    


## Installation
``` r
devtools::install_github("tobcap/lambdass")
## if you want this package to be compiled,
# devtools::install_github("tobcap/lambdass", args = "--byte-compile")
library("lambdass")
```

## Usage
``` r
# can be written in three ways.
f.(x, x + 1)
~~ .. + 1
x %->% {x + 1}
# all means add one
function(x) x + 1
```
### f.
``` r
# the function name f() is used in many situation, so I avoid using it and `f.` is adopted.
# f.(...arguments, body)
f.(x, x + 1)
f.(x, y, x + y)
# can define curried-function
f.(x, f.(y, x + y)) 

Map(f.(x, x ^ 2), 1:10)
Reduce(f.(x, y, x + y), 1:10)
```

``` r
## f.() is reorganized by C-lang.
## Original code is written in R. See f.r().
identical(f.(x, y, x + y), f.r(x, y, x + y))

## about ten times speed-up
microbenchmark::microbenchmark(
  f.(x, y, x + y), 
  f.r(x, y, x + y)
)
# Unit: microseconds
#              expr    min      lq     mean median     uq     max neval
#   f.(x, y, x + y)  4.904  6.0185  7.95367  7.579  8.025  50.375   100
#  f.r(x, y, x + y) 73.556 75.3395 86.02026 76.231 83.809 214.871   100
```

### double-tilda
``` r
# double-tilda with dotted placeholder like the usage of an underscore in scala's lambda
# or a hash-tag in Mathematica's Pure Function.
# 
# http://www.scala-lang.org/files/archive/spec/2.11/06-expressions.html#placeholder-syntax-for-anonymous-functions
# http://reference.wolfram.com/language/howto/WorkWithPureFunctions.html
# 
# A bounded vairable can be specified by tow-dots placeholder.
# Two or more variables can be designated by ..1, ..2, and so on.
~~ .. + 1
~~ ..2 / ..1
# cannot define curried-function such as `function(x) function(y) x + y`

Map(~~ .. ^ 2, 1:10)
Reduce(~~ ..1 + ..2, 1:10)
```

### arrow-notation
``` r
# arrow-notation
# right-hand side always needs `{` because R cannot control
# strength of associativity for `%infix-function%`.
x %->% {x + 1}

# left-hand side is written in two ways:
{x; y} %->% {x + y}
`any-valid-varname`(x, y) %->% {x + y}
# but recommend to use just `f`, for readability and simplicity
f(x, y) %->% {x + y}

Map(x %->% {x ^ 2}, 1:10)
Reduce({x; y} %->% {x + y}, 1:10)

# arguments can be checked when using `:` with tyepof() notation
{x:integer} %->% {x * 2}
f(x:character) %->% {paste(x, "test")}

# when the default value is set to a parameter, its type-checking 
# syntax is automatically added at head of function-body.
{x = 1L} %->% {x * 2}
f(x:character, y = "") %->% {paste0(x, y)}
```

## Benchmarking
``` r
## note that it's nanoseconds
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

## Church Encoding
by using f.(), R notation is almost as-is with the article written in  wikipedia
https://github.com/TobCap/lambdass/blob/master/vignettes/ChurchEncoding.md
