#include <R.h>
#include <Rinternals.h>
#include <limits.h>

#define TWO_DOTS_ID  INT_MIN

SEXP makeClosure(SEXP formals, SEXP body, SEXP env);
void ensureNonDuplicateNames(SEXP plist);
void ensureNotNamed(SEXP bd);
SEXP get_new_args(SEXP e);

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
  PROTECT(e1 = findVarInFrame(env, install("e1")));
  PROTECT(e2 = findVarInFrame(env, install("e2")));
  
  SEXP e1_expr, e2_expr;
  PROTECT(e1_expr = PREXPR(e1));
  PROTECT(e2_expr = PREXPR(e2));
  
  // `TYPEOF(e2) == PROMSXP` means e2 is not R_MissingArg
  if (TYPEOF(e2) == PROMSXP || length(e1_expr) != 2 ||  CAR(e1_expr) != install("~")) {
    SEXP ans, klass;
    
    if (TYPEOF(e2) == PROMSXP)
      PROTECT(ans = lang3(install("~"), e1_expr, e2_expr));
    else
      PROTECT(ans = lang2(install("~"), e1_expr));
    
    PROTECT(klass = mkString("formula"));
    setAttrib(ans, R_ClassSymbol, klass);
    setAttrib(ans, install(".Environment"), rho);
    UNPROTECT(6);
    
    return ans;
  }
  
  SEXP expr;
  PROTECT(expr = CDR(e1_expr)); // LISTSXP
  SET_TYPEOF(expr, LANGSXP); // substitute() only accept LANGSXP
  
  // Rprintf("type %s \n", type2char(TYPEOF(expr)));
  
  SEXP args_newsym, an;
  PROTECT(args_newsym = an = get_new_args(expr));
  R_xlen_t len = length(args_newsym);
    
  if (len == 1 && CAR(args_newsym) == install("..")) {
    //Rprintf(".. is called\n");
    SEXP arg_dot;
    PROTECT(arg_dot = allocList(1));
    SET_TAG(arg_dot, CAR(args_newsym));
    SETCAR(arg_dot, R_MissingArg);
    UNPROTECT(7);
    
    return makeClosure(arg_dot, CAR(expr), rho);
  }
  
  SEXP args_list, substi_list, a, s;
  PROTECT(args_list = a = allocList(len));
  PROTECT(substi_list = s = allocList(len));
  
  SEXP dots_syms, ds;
  PROTECT(dots_syms = ds = list5(install("..1"), install("..2"), install("..3"), install("..4"), install("..5")));
  
  for (int i = 0; i < len; a = CDR(a), s = CDR(s), an = CDR(an), ds = CDR(ds), i++) {
    SET_TAG(a, CAR(an));
    SETCAR(a, R_MissingArg);
    
    SET_TAG(s, CAR(ds));
    SETCAR(s, CAR(an));
  }
  
  //SEXP env2 = NewEnvironment(R_NilValue, duplicate(substi_list), R_BaseEnv);
  SEXP env2;
  PROTECT(env2 = allocSExp(ENVSXP));
  SET_FRAME(env2, substi_list);
  SET_ENCLOS(env2, R_EmptyEnv);
  
  SEXP ans_body;
  PROTECT(ans_body = CAR(substitute(expr, env2))); // need CAR to strip top level LANGSXP
  
  UNPROTECT(11);
  
  return makeClosure(args_list, ans_body, rho);
}

SEXP makeClosure(SEXP formals, SEXP body, SEXP env) {
  SEXP cl;
  PROTECT(cl = allocSExp(CLOSXP));

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
      return TWO_DOTS_; // used get_new_args 
    } else {
      buf += 2;
      return (int) strtol(buf, &endp, 10);
    }
  }
  
  return 0;
}

void set_dd_bit(SEXP s, int *dd_bit) {
  int dd_val = 0;
  
  switch(TYPEOF(s)) {
  case SYMSXP:
    dd_val = ddValMod(s);
    if (dd_val == TWO_DOTS_) {
      *dd_bit |= dd_val;
    } else if (dd_val > 0) {
      *dd_bit |= ((unsigned int)1 << (dd_val - 1));
    }  
    break;
  case LANGSXP:
  case LISTSXP:
    while(s != R_NilValue) {
      set_dd_bit(CAR(s), dd_bit);
      s = CDR(s);
    }
    break;
  default:
    break;
  }
}

SEXP get_new_args(SEXP e) {
  
  int dd_bit = 0;
  set_dd_bit(e, &dd_bit);
  
  //Rprintf("dd_bit is %d\n", dd_bit);
  switch(dd_bit) {
  case  0: return R_NilValue;
  case  1: return list1(install("._1"));
  case  3: return list2(install("._1"), install("._2"));
  case  7: return list3(install("._1"), install("._2"), install("._3"));
  case 15: return list4(install("._1"), install("._2"), install("._3"), install("._4"));
  case 31: return list5(install("._1"), install("._2"), install("._3"), install("._4"), install("._5"));
  case TWO_DOTS_ID: return list1(install(".."));
  default: 
    error(
    "\nTail-prefix number of placeholders must be in order and"
    "\nthe number of arguments is limitted to five"
    "\n"
    );
  }
}
