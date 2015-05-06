#' Syntax sugar of making an anonymous function
#'
#' arrow function can check arguments class.
#'
#' @name arrow
#' @param lhs formal parameters: any R parsable expression is acceptable,
#' but \code{f(x, y)} or \code{{x; y}} is recommended.
#' @param rhs body for lambda
#' @examples
#' {} %->% {x + 2}
#' x %->% {x + 1}
#' {x; y} %->% {x + y}
#' {x = 1L; y = 2L} %->% {x + y}
#'
#' {x:character; e:environment} %->% {get(x, envir = e, inherits = FALSE)}
#' # see more examples in https://gist.github.com/TobCap/6826123
NULL


#' @rdname arrow
#' @export
`%->%` <- function(lhs, rhs) {
  env_ = parent.frame()

  # as.formals <- function(xs) as.pairlist(tools:::as.alist.call(xs))
  lhs_expr <- substitute(lhs)
  if (length(lhs_expr) > 1) {
    args_ <- as.vector(lhs_expr, "list")[-1]
  } else if (length(lhs_expr) == 1 && class(lhs_expr) != "{") {
    args_ <- list(lhs_expr)
  } else {
    # length(lhs_expr) == 1 && class(lhs_expr) == "{"
    args_ <- NULL
  }

  # short-cut for non-class-defined situation
  # if (!any(c(":", "=") %in% all.names(lhs_expr)))
  if (is.null(args_) ||
      (all(!nzchar(names(args_))) &&
       all(vapply(args_, function(x) length(x) == 1, FALSE))))
    return(eval(call("function", as.formals(args_), substitute(rhs)), env_))

  mod_args <- mapply(
    function(arg_sym, arg_name) {
      arg_sym_char <- as.character(arg_sym)
      has_name <- !is.null(arg_name) && nzchar(arg_name)
      if (!has_name) {
        if (is.call(arg_sym) && arg_sym[[1]] == ":") {
          ## in case class is defined
          if (arg_sym_char[[3]] %in% sub("is.", "", ls(pattern = "^is\\.", baseenv()))) {
            arg_sym_new <- as.formals(arg_sym_char[[2]])
            arg_class <- arg_sym_char[[3]]
          } else if (tolower(arg_sym_char[[3]]) == "any") {
            arg_sym_new <- as.formals(arg_sym_char[[2]])
            arg_class <- NA
          } else {
            stop("'", paste0(arg_sym_char[[3]], "' is not appropriate class designation."))
          }
        } else if (is.call(arg_sym) && arg_sym[[1]] == "=") {
          ## default value is set
          arg_sym_new <- as.formals(`names<-`(list(arg_sym[[3]]), arg_sym_char[[2]]))
          arg_class <- class(eval(arg_sym[[3]], env_))
        } else if (is.symbol(arg_sym)) {
          ## only a symbol. This allows any class.
          arg_sym_new <- as.formals(arg_sym_char)
          arg_class <- NA
        } else {
          stop("An argument must be a symbol.")
        }
      } else { ## When has_name, assigning value must be able to be evaluate.
        arg_sym_new <- as.formals(arg_name, list(arg_sym))
        arg_class <- class(eval(arg_sym, env_))
      }

      check_expr <-
        if (is.na(arg_class)) NULL
        else call(paste0("is.", arg_class), as.symbol(names(arg_sym_new)))

      list(arg_sym_new = arg_sym_new, check_expr = check_expr)
    },
    args_,
    rep_len(as.list(names(args_)), length(args_)),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )

  arglist <- unlist(lapply(mod_args, function(x) x$arg_sym_new), recursive = FALSE)
  checking_expr <- lapply(mod_args, function(x) x$check_expr)

  and_bool_expr <- (function(x) {
    n <- length(x)
    if (n == 0) quote(TRUE)
    else if (n == 1) x[[1]]
    else call("&&", Recall(x[-n]), x[[n]])
  })(Filter(Negate(is.null), checking_expr))

  expr_add <-
    call("if",
      call("!",
        call("(", and_bool_expr)),
          quote(stop("some actual arguments are not appropriate type")))

  body_ <-
    if (all(as.character(expr_add[1:2]) == c("if", "!(TRUE)"))) substitute(rhs)
    else as.call(append(as.list(substitute(rhs)), expr_add, 1))

  eval(call("function", as.pairlist(arglist), body_), env_)
}
