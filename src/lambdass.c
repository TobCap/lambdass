#include <R.h>
#include <Rinternals.h>

SEXP makeClosure(SEXP formals, SEXP body, SEXP env);
void anyDuplicateNames(SEXP plist);

SEXP C_f(SEXP args, SEXP env) {
  SEXP argsnew, ansp, body;
  args = CDR(args); // skip name

  int len = length(args);
  if (len <= 1) {
    return makeClosure(R_NilValue, CAR(args), env);
  }

  PROTECT(argsnew = ansp = allocList(len - 1));
  while (--len) {
    if (TAG(args) == R_NilValue) {
      if (TYPEOF(CAR(args)) != SYMSXP) {
        error("argument must be a symbol or `Name=Value` style");
      }
      SETCAR(argsnew, R_MissingArg);
      SET_TAG(argsnew, CAR(args));
    } else {
      SETCAR(argsnew, CAR(args));
      SET_TAG(argsnew, TAG(args));
    }
    argsnew = CDR(argsnew);
    args = CDR(args);
  }
  body = CAR(args); // last element

  anyDuplicateNames(ansp);
  UNPROTECT(1);
  return makeClosure(ansp, body, env);
}

SEXP makeClosure(SEXP formals, SEXP body, SEXP env) {
  SEXP cl;
  // need PROTECT ?
  PROTECT(formals);
  PROTECT(body);
  PROTECT(env);

  cl = PROTECT(allocSExp(CLOSXP));
  SET_FORMALS(cl, formals);
  SET_BODY(cl, body);
  SET_CLOENV(cl, env);
  UNPROTECT(4);
  return cl;
}

void anyDuplicateNames(SEXP plist) {
  SEXP names = getAttrib(plist, R_NamesSymbol);

  if (any_duplicated(names, 0))
    error("arguments must have unique name");
}
