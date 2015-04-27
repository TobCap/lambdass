#' Syntax sugar of making an anonymous function
#'
#' Tilda is a very useful symbol because rest parts are not evaluated.
#' In normal R usage, it is used to create a formula object as inner-DSL.
#' I hijack this functionality to make an anounymous function.
#' Double tilda and its posterior parts with a symbol of `..` make
#' an anonymous function whose formal parameter is converted from `..` to
#' an arbitrary non-overlapping symbol (mimic LISP's gensym).
#' If you want to make an anoymous function which has two or more arguments,
#' the placeholde should be \code{..1}, \code{..2}, and so on. See examples.
#' Single tilda works as if it is normaly used but it takes a bit calulation process.
#'
#' @details Unsupported nested lambda.
#' \code{function(x) function(y) x + y} cannot define by double-tilda.
#' Use \code{f.} and type this; \code{f.(x, f.(y, x + y))}
#' @param ... expression starts with \code{~~}
#' @param env_ environment where \code{...} is evaluated
#' @name double-tilda
#' @examples
#' # `x`, `x1`, and `x2` are bound variables, does not matter if
#' # they are shown in other symbols.
#' ~~ .. + 1 # => function(x) x + 1
#' ~~ ..1 + ..2 # => function(x1, x2) x1 + x2
#'
#' Reduce(~~ ..1 + ..2, 1:10)
#' Filter(~~ .. %% 3 == 0, 1:10)
#'
#' # The tilda's role remains.
#' ~ speed + dist
#' lm(speed ~ dist, data = cars)
#' lm(mpg ~ ., data = mtcars)
NULL

#' @rdname double-tilda
#' @export
`~` <- function(..., env_ = parent.frame()) {

  dots <- as.list.default(substitute((...)))[-1]
  if (length(dots) != 1 || length(dots[[1]]) == 1 || dots[[1]][[1]] != "~")
    return(as.formula(deparse(as.call(c(quote(`~`), dots))), env_))

  expr <- dots[[1]][[2]]

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
