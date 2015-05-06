#include <R.h>
#include <Rinternals.h>

SEXP makeClosure(SEXP formals, SEXP body, SEXP env);
void anyDuplicateNames(SEXP plist);
void ensureNotNamed(SEXP bd);

SEXP C_fs(SEXP args, SEXP env) {
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

void ensureNotNamed(SEXP bd) {
  if (TAG(bd) != R_NilValue)
    error("the last element should not be named");
}

SEXP C_f(SEXP env, SEXP rho) {
  SEXP dots = findVarInFrame(env, R_DotsSymbol);

/*
  Rprintf("type %s \n", type2char(TYPEOF(dots)));
  Rprintf("TAG %s \n", TAG(dots) == R_NilValue ? "Nil" : CHAR(PRINTNAME((TAG(dots)))));
  Rprintf("length %d \n", length(dots));
  Rprintf("missing? %d \n", dots == R_MissingArg);
  Rprintf("\n");
  */

  int len = length(dots);

  if (dots == R_MissingArg) { // Nothing is passed to
    return makeClosure(R_NilValue, R_NilValue, rho);
  }
  if (len == 1) {
    ensureNotNamed(dots);
    return makeClosure(R_NilValue, PREXPR(CAR(dots)), rho);
  }


  SEXP argsnew, ansp, body;
  PROTECT(argsnew = ansp = allocList(len - 1));
  while (--len) {
    SEXP expr = PREXPR(CAR(dots));
    if (TAG(dots) == R_NilValue) {
      if (TYPEOF(expr) != SYMSXP) {
        error("argument must be a symbol or `Name=Value` style");
      }
      SETCAR(argsnew, R_MissingArg);
      SET_TAG(argsnew, expr);
    } else {
      SETCAR(argsnew, expr);
      SET_TAG(argsnew, TAG(dots));
    }
    argsnew = CDR(argsnew);
    dots = CDR(dots);
  }

  body = PREXPR(CAR(dots)); // last element

  ensureNotNamed(dots);
  anyDuplicateNames(ansp);

  UNPROTECT(1);
  return makeClosure(ansp, body, rho);

return R_NilValue;
}
