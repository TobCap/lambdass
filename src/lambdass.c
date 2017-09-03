#include <R.h>
#include <Rinternals.h>
#include <limits.h>

#define TWO_DOTS_ID  INT_MIN

SEXP makeClosure(SEXP formals, SEXP body, SEXP env);
void ensureNonDuplicateNames(SEXP plist);
void ensureNotNamed(SEXP bd);
SEXP getAlteredSyms(SEXP e);


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

SEXP C_double_tilda(SEXP env, SEXP rho)
{
  SEXP e1, e2;
  PROTECT(e1 = findVarInFrame(env, install("e1")));
  PROTECT(e2 = findVarInFrame(env, install("e2")));
  
  SEXP e1_expr, e2_expr;
  PROTECT(e1_expr = PREXPR(e1));
  PROTECT(e2_expr = PREXPR(e2));
  
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
  
  SEXP expr;
  PROTECT(expr = CDR(e1_expr)); // LISTSXP
  SET_TYPEOF(expr, LANGSXP); // substitute() only accepts LANGSXP
  
  // Rprintf("type %s \n", type2char(TYPEOF(expr)));
  
  SEXP alteredSyms;
  PROTECT(alteredSyms = getAlteredSyms(expr));
  R_xlen_t len = length(alteredSyms);
    
  if (len == 1 && CAR(alteredSyms) == install("..")) {
    //Rprintf(".. is called\n");
    SEXP arg_dot;
    PROTECT(arg_dot = allocList(1));
    SET_TAG(arg_dot, CAR(alteredSyms));
    SETCAR(arg_dot, R_MissingArg);
    UNPROTECT(7);
    
    return makeClosure(arg_dot, CAR(expr), rho);
  }
  
  SEXP ansFormals, substiList, dots5List;
  PROTECT(ansFormals = allocList(len));
  PROTECT(substiList = allocList(len));
  PROTECT(dots5List = list5(install("..1"), install("..2"), install("..3"), install("..4"), install("..5")));
  
  int i = 0;
  for (SEXP p_ansFormals = ansFormals, p_substiList = substiList ; i < len;
       p_ansFormals = CDR(p_ansFormals), p_substiList = CDR(p_substiList),
       alteredSyms = CDR(alteredSyms), dots5List = CDR(dots5List), i++) {
    
    SET_TAG(p_ansFormals, CAR(alteredSyms));
    SETCAR(p_ansFormals, R_MissingArg);
    
    SET_TAG(p_substiList, CAR(dots5List));
    SETCAR(p_substiList, CAR(alteredSyms));
  }
  
  //SEXP env2 = NewEnvironment(R_NilValue, duplicate(substiList), R_BaseEnv);
  SEXP env2;
  PROTECT(env2 = allocSExp(ENVSXP));
  SET_FRAME(env2, substiList);
  SET_ENCLOS(env2, R_EmptyEnv);
  
  SEXP ansBody;
  ansBody = CAR(substitute(expr, env2)); // need CAR to strip top level LANGSXP
  
  UNPROTECT(10);
  
  return makeClosure(ansFormals, ansBody, rho);
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

void ensureNonDuplicateNames(SEXP plist)
{
  SEXP names = getAttrib(plist, R_NamesSymbol);

  if (any_duplicated(names, 0))
    error("arguments must have unique name");
}

void ensureNotNamed(SEXP bd) {
  if (TAG(bd) != R_NilValue)
    error("the last element should not be named");
}

int getddId(SEXP symbol)
{
  // https://github.com/wch/r-source/blob/e959f15cf3c37be9829c9a97d6bd4dd86b2495fc/src/main/envir.c#L1320-L1336
  char *endp;
  const char *buf = CHAR(PRINTNAME(symbol));
  
  if( !strncmp(buf, "..", 2) ) {
    if (strlen(buf) == 2) {
      return TWO_DOTS_ID; // used getAlteredSyms 
    } else {
      buf += 2;
      return (int)strtol(buf, &endp, 10);
    }
  }
  
  return 0;
}

void setDdBit(SEXP expr, int *ddBit)
{
  int ddId = 0;
  
  switch(TYPEOF(expr)) {
  case SYMSXP:
    ddId = getddId(expr);
    if (ddId == TWO_DOTS_ID) {
      *ddBit |= ddId;
    } else if (ddId > 0) {
      *ddBit |= 1 << (ddId - 1);
    }  
    break;
  case LANGSXP:
  case LISTSXP:
    while(expr != R_NilValue) {
      setDdBit(CAR(expr), ddBit);
      expr = CDR(expr);
    }
    break;
  default:
    break;
  }
}

SEXP getAlteredSyms(SEXP e)
{
  int ddBit = 0;
  setDdBit(e, &ddBit);
  
  //Rprintf("ddBit is %d\n", ddBit);
  switch(ddBit) {
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
