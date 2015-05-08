# Lambda Syntax-sugar (lambdass)

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
# double-tilda with dotted placeholder like the usage of underscore in scala's lambda
# A bounded vairable can be specified by tow-dots placeholder.
# Two or more variables can be designated by ..1, ..2, and so on.
~~ .. + 1
~~ ..1 * (..1 + ..2)
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
> microbenchmark::microbenchmark(
  "f." = f.(x, x), 
  "%->%" = x %->% {x}, 
  "~~" = ~~ ..,
  "as.function" = as.function(alist(x=, x)),
  "function(x) x" = function(x) x
  )
Unit: nanoseconds
          expr    min       lq      mean   median       uq    max neval
            f.   7579   9139.5  10940.42  11146.0  12037.0  22736   100
          %->% 128388 133515.0 145715.94 136189.5 139978.0 388729   100
            ~~ 283522 290655.0 302094.39 293776.0 299125.0 456043   100
   as.function  41905  44356.5  55965.23  54610.5  59067.5 267474   100
 function(x) x      0    447.0    754.24    892.0    893.0   2230   100
```

## Church Encoding
by using f.(), R notation is almost as-is with the article written in  wikipedia
https://github.com/TobCap/lambdass/blob/master/vignettes/ChurchEncoding.md
