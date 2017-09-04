#include <R.h>
#include <Rinternals.h>
#include <limits.h>

#define TWO_DOTS_ID  INT_MIN
#define NEW_SYM_PREFIX "._"

SEXP makeClosureForTilde(SEXP rhs, SEXP env);
SEXP makeClosure(SEXP formals, SEXP body, SEXP env);
void ensureNonDuplicateNames(SEXP plist);
void ensureNotNamed(SEXP bd);
SEXP getNewDdBody(SEXP expr, int *ddBit);

SEXP C_f(SEXP env, SEXP rho)
{
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
  SEXP formal, p_formal, body;
  PROTECT(formal = p_formal = allocList(len - 1));
  
  while (--len) {
    SEXP expr = PREXPR(CAR(dots));
    if (TAG(dots) == R_NilValue) {
      if (TYPEOF(expr) != SYMSXP)
        error("argument must be a symbol or `Name=Value` style");
      
      SETCAR(p_formal, R_MissingArg);
      SET_TAG(p_formal, expr);
      
    } else {
      if (expr == R_MissingArg)
        error("argument must be a symbol or `Name=Value` style");
      
      SETCAR(p_formal, expr);
      SET_TAG(p_formal, TAG(dots));
      
    }
    p_formal = CDR(p_formal);
    dots = CDR(dots);
  }

  body = PREXPR(CAR(dots)); // last element

  ensureNotNamed(dots);
  ensureNonDuplicateNames(formal);

  UNPROTECT(1);
  return makeClosure(formal, body, rho);
}

void ensureNonDuplicateNames(SEXP plist)
{
  SEXP names = getAttrib(plist, R_NamesSymbol);

  if (any_duplicated(names, 0))
    error("arguments must have unique name");
}

void ensureNotNamed(SEXP bd)
{
  if (TAG(bd) != R_NilValue)
    error("the last element should not be named");
}

SEXP C_double_tilde(SEXP env, SEXP rho)
{
  SEXP e1, e2;
  PROTECT(e1 = findVarInFrame(env, install("e1")));
  PROTECT(e2 = findVarInFrame(env, install("e2")));
  
  SEXP e1_expr, e2_expr;
  PROTECT(e1_expr = PREXPR(e1));
  PROTECT(e2_expr = PREXPR(e2));
  
  // For single tilde, create R's a normal formula
  // `TYPEOF(e2) == PROMSXP` means e2 is not R_MissingArg just like 'speed ~ dist'
  if (TYPEOF(e2) == PROMSXP || CAR(e1_expr) != install("~")) {
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
  // e1_expr is `~ somthing`, needs CDR(.) to remove `~`
  SEXP expr;
  PROTECT(expr = CDR(e1_expr)); // LISTSXP
  SET_TYPEOF(expr, LANGSXP);
  UNPROTECT(5);
  //Rprintf("type %s \n", type2char(TYPEOF(expr)));
  return makeClosureForTilde(expr, rho);
  
}


SEXP allocFormalsList1(SEXP sym1)
{
  SEXP res = allocList(1);
  SET_TAG(res, sym1);
  SETCAR(res, R_MissingArg);
  return res;
}

SEXP mkNewSym(char const *prefix, int n)
{
  char newStr[10];
  sprintf(newStr, "%s%d", prefix, n);
  return install(newStr);
}

SEXP mkNewFormals(int n)
{
  SEXP res;
  PROTECT(res = allocList(n));
  int i = 0;
  for (SEXP p_res = res; i < n; p_res = CDR(p_res), i++){
    SET_TAG(p_res, mkNewSym(NEW_SYM_PREFIX, i + 1));
    SETCAR(p_res, R_MissingArg);
  }
  UNPROTECT(1);
  return res;
}

SEXP makeClosureForTilde(SEXP expr, SEXP rho)
{

  int ddBit = 0;
  SEXP newBody = getNewDdBody(expr, &ddBit);
  SEXP newFormals;
  //Rprintf("ddBit is %d\n", ddBit);
  
  switch(ddBit) {
  case TWO_DOTS_ID: newFormals = allocFormalsList1(install("..")); break;
  case  0: newFormals = R_NilValue; break;
  case  1: newFormals = mkNewFormals(1); break;
  case  3: newFormals = mkNewFormals(2); break;
  case  7: newFormals = mkNewFormals(3); break;
  case 15: newFormals = mkNewFormals(4); break;
  case 31: newFormals = mkNewFormals(5); break;
  default: 
    error(
    "\nTail-prefix number of placeholders must be in order and"
    "\nthe number of arguments is limitted to five"
    "\n"
    );
  }
  
  return makeClosure(newFormals, CAR(newBody), rho);
}

SEXP makeClosure(SEXP formals, SEXP body, SEXP env)
{
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

int getddId(SEXP symbol)
{
  // https://github.com/wch/r-source/blob/e959f15cf3c37be9829c9a97d6bd4dd86b2495fc/src/main/envir.c#L1320-L1336
  char *endp;
  const char *buf = CHAR(PRINTNAME(symbol));
  
  if( !strncmp(buf, "..", 2) ) {
    if (strlen(buf) == 2) {
      return TWO_DOTS_ID; // used getNewDdBod
    } else {
      buf += 2;
      return (int)strtol(buf, &endp, 10);
    }
  }
  
  return 0;
}

SEXP getNewDdBody(SEXP expr, int *ddBit)
{
  
  int ddId = 0;
  SEXP h, p = R_NilValue, res = R_NilValue;
  
  switch(TYPEOF(expr)) {
  case SYMSXP:
    ddId = getddId(expr);
    if (ddId == TWO_DOTS_ID) {
      *ddBit |= ddId;
      //return install("..");
      return expr;
    } else if (ddId > 0) {
      *ddBit |= 1 << (ddId - 1);
      return mkNewSym(NEW_SYM_PREFIX, ddId);
    } else {
      return expr;
    }
  case LANGSXP:
  case LISTSXP:
    while(expr != R_NilValue) {
      h = getNewDdBody(CAR(expr), ddBit);
      if (isLanguage(expr)) {
        h = LCONS(h, R_NilValue);
      } else {
        h = CONS(h, R_NilValue);
      }
      SET_TAG(h, TAG(expr));
        
      if (h != R_NilValue) {
        if (res == R_NilValue) {
          PROTECT(res = h);
        } else {
          SETCDR(p, h);
        }
        while (CDR(h) != R_NilValue) h = CDR(h);
        p = h;
      }
      expr = CDR(expr);
    }
    if (res != R_NilValue) {
      UNPROTECT(1);
    }
    return res;
  default:
    return expr;
  }
}
