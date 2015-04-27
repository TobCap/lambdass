# Lambda Syntax-sugar (lambdass)

## What is this package?
The purpose of this package is to provide you with easy syntax for making anonymous
function. 

## Related packages and functions
##### please tell me if you know other packages.
* pryr::f(), pyry::make_function() from https://github.com/hadley/pryr
* lambda.r::`%as%` from https://github.com/zatonovo/lambda.r
* lambdaR::lambda() from https://github.com/hoxo-m/lambdaR    


## Installation
``` r
devtools::install_github("tobcap/lambdass")
library("lambdass")
```

## Usage

``` r
# can be written in three ways.
~~ .. + 1
f.(x, x + 1)
x %->% {x + 1}
# all means add one
function(x) x + 1
```

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
# double-tilda with dotted placeholder
# A bounded vairable can be specified by tow-dots placeholder.
# Two or more variables can be designated by ..1, ..2, and so on.
~~ .. + 1
~~ ..1 * (..1 + ..2)
# cannot define curried-function such as `function(x) function(y) x + y`


Map(~~ .. ^ 2, 1:10)
Reduce(~~ ..1 + ..2, 1:10)
```

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
# note that it's nanoseconds
> microbenchmark::microbenchmark(f.(x, x), x %->% {x}, ~~ .., function(x) x)
Unit: nanoseconds
             expr    min       lq      mean median       uq    max neval
         f.(x, x)  97181 100302.0 109979.55 103868 106097.0 543409   100
 x %->% {     x } 123928 126602.5 136681.82 130169 132174.5 406108   100
             ~~.. 278168 281735.0 291996.59 284409 291764.5 431963   100
    function(x) x      0    446.0    620.39    447    892.0   1338   100
```
