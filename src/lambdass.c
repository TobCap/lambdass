#include <R.h>
#include <Rinternals.h>

SEXP makeClosure(SEXP formals, SEXP body, SEXP env);
void ensureNonDuplicateNames(SEXP plist);
void ensureNotNamed(SEXP bd);

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

  if (dots == R_MissingArg) { // Nothing is passed
    return makeClosure(R_NilValue, R_NilValue, rho);
  }
  if (len == 1) {
    ensureNotNamed(dots);
    return makeClosure(R_NilValue, PREXPR(CAR(dots)), rho);
  }

  // the idea comes from asFunction() defined in R source code.
  // https://github.com/wch/r-source/blob/565868293e1s25eb1a4f68fa149e2d24963edf781/src/main/coerce.c#L1275
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
      if (expr == R_MissingArg) {
        error("argument must be a symbol or `Name=Value` style");
      }
      SETCAR(argsnew, expr);
      SET_TAG(argsnew, TAG(dots));
    }
    argsnew = CDR(argsnew);
    dots = CDR(dots);
  }

  body = PREXPR(CAR(dots)); // last element

  ensureNotNamed(dots);
  ensureNonDuplicateNames(ansp);

  UNPROTECT(1);
  return makeClosure(ansp, body, rho);

  return R_NilValue;
}

SEXP makeClosure(SEXP formals, SEXP body, SEXP env) {
  SEXP cl = PROTECT(allocSExp(CLOSXP));
  SET_FORMALS(cl, formals);
  SET_BODY(cl, body);
  SET_CLOENV(cl, env);
  UNPROTECT(1);
  return cl;
}

void ensureNonDuplicateNames(SEXP plist) {
  SEXP names = getAttrib(plist, R_NamesSymbol);

  if (any_duplicated(names, 0))
    error("arguments must have unique name");
}

void ensureNotNamed(SEXP bd) {
  if (TAG(bd) != R_NilValue)
    error("the last element should not be named");
}
