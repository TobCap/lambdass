#' Syntax sugar of making an anonymous function
#'
#' @name lambdass
#' @param x x is coerced to character (internally STRSXP).
#' @param y y is expected to be pairlist object with completely named
#' @param value a default value of a formal parameter.
#' @param ...  If one argument is passed, that is parsed as lambda's body.
#' If two or more argument is passed, the last part is parsed as lambda's body
#' and the rest are parsed as formal parameters for lambda.
#' @param env_ an environment in which call object is evaluated.
#' @examples
#'  f.(x, x + 1) # => function(x) x + 1
#'  f.(x, f.(y, x + y)) #=>
#'  Reduce(f.(x, y, x + y), 1:10) # => 55
#'  Filter(f.(x, x %% 2 == 0), 1:10) # => c(2L, 4L, 6L, 8L, 10L)
#'
#'
NULL

#' @rdname lambdass
#' @export
as.formals <- function(x, value = list(quote(expr=))) {
  ## a faster version of tools:::as.alist.call
  if (!all(nzchar(x)))
    stop('Including "" or substitute() is invalid input.')

  if (length(x) == 0)
    return(NULL)

  # backquote is added when as.character(list(symbol))
  # https://github.com/wch/r-source/blob/c49da80f91d6dd6c20b5fd714e7cceaaecbd8d39/src/main/coerce.c#L1049-L1062
  # check the below codes
  # as.character(quote(`_`))
  # as.character(list(quote(`_`)))

  if (is.null(names(x))) {
    new_names <-
      if (length(x) == 1 && !is.recursive(x)) as.character(x)
      else vapply(x, as.character, "")
    return(`names<-`(as.pairlist(rep_len(value, length(x))), new_names))
  }

  ans <- as.list(x)
  idx <- which(!nzchar(names(ans)))
  names(ans)[idx] <- vapply(ans[idx], as.character, "")
  ans[idx] <- rep_len(value, length(idx))
  as.pairlist(ans)
}

#' @rdname lambdass
#' @export
is.formals <- function(y) {
  is.pairlist(y) && length(y) == sum(nzchar(names(y)))
}

### sugar-syntax for lambda
### adopt `f.` instead of `f` because `f` often causes conflicts in many sample codes.
#' @rdname lambdass
#' @export
f. <- function(..., env_ = parent.frame()) {
  # see https://gist.github.com/TobCap/6366396 for how to handle unevaluated `...`
  d <- as.pairlist(as.vector(substitute((...)), "list")[-1])
  # need to be pairlist to return NULL when nothing is passed to `...`.

  n <- length(d)
  eval(call("function", as.formals(d[-n]), d[[n]]), env_)
}
