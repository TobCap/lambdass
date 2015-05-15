#' Syntax sugar of making an anonymous function
#'
#' Tilda is a R's "Primitive Function" that does not evaluate its argument, and
#' it is normally used to create a formula object as an inner-DSL role.
#' I hijack this functionality to make an anounymous function.
#' Double tilda with a two-dots symbol, \code{..}, make an anonymous function
#' in which two-dots plays a placeholder. If you need two or more arguments,
#' the placeholde should be \code{..1}, \code{..2}, and so on. See examples.
#' Single tilda works as if it is normaly used but it takes a bit calulation 
#' process, and not fully tested.
#'
#' @details Unsupported nested lambda.
#' \code{function(x) function(y) x + y} cannot define by double-tilda.
#' Use \code{f.} and type this; \code{f.(x, f.(y, x + y))}
#' @param e1,e2 The original-tilda is both unary and binary function. 
#' if \code{e2} is missing and \code{e1}'s first call object is \code{~},
#' then anonymous function is made.
#' @name double-tilda
#' @useDynLib lambdass C_double_tilda
#' @examples
#' \donttest{
#' ~~ .. + 1 # => function(..) .. + 1
#' ~~ ..1 + ..2 # => function(._1, ._2) ._1 + ._2
#' 
#' Reduce(~~ ..1 + ..2, 1:10)
#' Filter(~~ .. %% 3 == 0, 1:10)
#' }
#' # The tilda's role remains.
#' ~ speed + dist
#' lm(speed ~ dist, data = cars)
#' lm(mpg ~ ., data = mtcars)
NULL

#' @rdname double-tilda
#' @export
`~` <- function(e1, e2) {
  env_ = parent.frame()
  return(.Call(C_double_tilda, environment(), env_))

  e1_expr <- substitute(e1)
  if (!missing(e2) || length(e1_expr) != 2 || e1_expr[[1]] != "~")
    return(as.formula(as.call(c(quote(`~`), e1_expr)), env_))

  expr <- e1_expr[[2]]

  all_vars <- all.names(expr, functions = FALSE, unique = TRUE)
  args_char <- sort.default(all_vars[grep("^..$|^..[0-9]+$", all_vars)])
  args_len <- length(args_char)

  if (args_len == 1 && args_char != "..")
    stop("only one placeholder must be input `..`")
  if (args_len > 1 && !all(args_char == paste0("..", seq_len(args_len))))
    stop("the placeholder must start with `..` and end with numeric from 1 in order.")

  args_new <- gensyms(args_len)
  substi_list <- setNames(args_new, args_char)

  eval(call("function", as.formals(args_new), substituteDirect(expr, substi_list)), env_)
}


gensyms <- (function() {
  base_name <- "._"
  num <- 0
  function(n, allow_overlapping = TRUE){
    if (missing(n) || n < 0) stop("n must be greater than one")
    if (n == 0) {
      num <<- 0
      return(invisible())
    }

    current_num <- num
    if (!allow_overlapping) num <<- num + n

    lapply(paste0(base_name, seq_len(n) + current_num), as.symbol)
  }
})()
