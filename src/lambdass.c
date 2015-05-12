#include <R.h>
#include <Rinternals.h>

SEXP makeClosure(SEXP formals, SEXP body, SEXP env);
void ensureNonDuplicateNames(SEXP plist);
void ensureNotNamed(SEXP bd);
SEXP get_direct_dd_sym(SEXP e);
void set_dd_sym(SEXP s, SEXP *ans);

SEXP C_f(SEXP env, SEXP rho) {
  SEXP dots = findVarInFrame(env, R_DotsSymbol);
  /*
  Rprintf("type %s \n", type2char(TYPEOF(dots)));
  Rprintf("TAG %s \n", TAG(dots) == R_NilValue ? "Nil" : CHAR(PRINTNAME((TAG(dots)))));
  Rprintf("length %d \n", length(dots));
  Rprintf("missing? %d \n", dots == R_MissingArg);
  Rprintf("\n");
  */
  
  R_len_t len = length(dots);

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
}

SEXP C_double_tilda(SEXP env, SEXP rho) {
  SEXP e1, e2;
  e1 = findVarInFrame(env, install("e1"));
  e2 = findVarInFrame(env, install("e2"));
  
  SEXP e1_expr, e2_expr;
  PROTECT(e1_expr = PREXPR(e1));
  PROTECT(e2_expr = PREXPR(e2));

  // `TYPEOF(e2) == PROMSXP` means e2 is not R_MissingArg
  if (TYPEOF(e2) == PROMSXP || length(e1_expr) != 2 ||  CAR(e1_expr) != install("~")) {
    //if (TYPEOF(e2) == PROMSXP) Rprintf("not missing e2\n");
    //if (length(e1_expr) != 2) Rprintf("not unary func\n");
    //if (CAR(e1_expr) != install("~")) Rprintf("not tilda\n");
    SEXP ans, klass;
    PROTECT(ans = lang2(install("~"), e1_expr));
    PROTECT(klass = mkString("formula"));
    setAttrib(ans, R_ClassSymbol, klass);
    setAttrib(ans, install(".Environment"), rho);
    UNPROTECT(4);
    return ans;
  }
  
  SEXP expr = CDR(e1_expr);
  
  SEXP all_nms;
  //all_nms = get_dd_syms(expr);
  all_nms = get_direct_dd_sym(expr);
   /*
  static const char *dot_names[] = {"..", "..1", "..2", "..3", "..4", "..5", "..6", "..7", "..8", "..9"};
  SEXP lst = PROTECT(mkNamed(VECSXP, dot_names));
  
  for (int i = 0 ; i < 10; i++) {
    SET_VECTOR_ELT(lst, i, install(dot_names[i]));
  }
  
  SEXP call = PROTECT(lang4(install("list2env"), lst,
                       R_NilValue, 
                       R_EmptyEnv));
  SEXP new_env = eval(call, rho);
  SEXP expr_new = substitute(expr, new_env);
  
  
  UNPROTECT(4);
  */
   UNPROTECT(2);
  return all_nms;
}

SEXP makeClosure(SEXP formals, SEXP body, SEXP env) {
  SEXP cl = PROTECT(allocSExp(CLOSXP));

  // non-checking version of mkCLOSXP in "dstruct.c"
  // I'm not sure whether PROTECT() is required or not for formals, body, and env.
  // https://github.com/wch/r-source/blob/ed415a8431b32e079100f50a846e4769aeb54d5a/src/main/dstruct.c#L81-L83

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

int ddValMod(SEXP symbol)
{
  char *endp;
  const char *buf = CHAR(PRINTNAME(symbol));
  
  if( !strncmp(buf, "..", 2) ) {
    if (strlen(buf) == 2) {
      return 0;
    } else {
      buf += 2;
      return (int) strtol(buf, &endp, 10);
    }
  }
  
  return -1;

}


typedef struct {
  SEXP    ans;
  int    ddValues;
  int    Counts;
  int    SingleDd;
} DotsData;


void set_dd_sym(SEXP s, SEXP *ans) {
  int dd_val;
  
  switch(TYPEOF(s)) {
  case SYMSXP:
    dd_val = ddValMod(s);
    if (dd_val > -1) {
      *ans = CONS(s, *ans);
    }
    break;
  case LANGSXP:
  case LISTSXP:
    while(s != R_NilValue) {
      set_dd_sym(CAR(s), ans);
      s = CDR(s);
    }
    break;
  default:
    break;
  }
}


SEXP get_direct_dd_sym(SEXP e) {
  SEXP ans;
  PROTECT(ans = R_NilValue);
  
  set_dd_sym(duplicate(e), &ans);
  UNPROTECT(1);
  return ans;
}
