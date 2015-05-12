#include <R.h>
#include <Rinternals.h>

SEXP makeClosure(SEXP formals, SEXP body, SEXP env);
void ensureNonDuplicateNames(SEXP plist);
void ensureNotNamed(SEXP bd);
SEXP get_all_syms(SEXP expr);
SEXP get_dd_syms(SEXP expr);


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
  SEXP e1 = findVarInFrame(env, install("e1"));
  SEXP e2 = findVarInFrame(env, install("e2"));
  R_len_t len = length(e1);

  Rprintf("type of e1 is %s \n", type2char(TYPEOF(e1)));
  SEXP e1_expr = PREXPR(e1);

  Rprintf("type of e2 is %s \n", type2char(TYPEOF(e2)));
  SEXP e2_expr = PREXPR(e2);
  Rprintf("\n");
  
  Rprintf("e1_expr info\n");
  Rprintf("type %s \n", type2char(TYPEOF(e1_expr)));
  Rprintf("TAG %s \n", TAG(e1_expr) == R_NilValue ? "Nil" : CHAR(PRINTNAME((TAG(e1_expr)))));
  Rprintf("length %d \n", length(e1_expr));
  Rprintf("missing? %d \n", e1_expr == R_MissingArg);
  Rprintf("\n");

  Rprintf("CAR(e1_expr) info\n");
  SEXP car_ = CAR(e1_expr);
  Rprintf("type %s \n", type2char(TYPEOF(car_)));
  Rprintf("length %d \n", length(car_));
  Rprintf("\n");
  
  Rprintf("is tilda? %d \n", car_ == install("~"));
  Rprintf("\n");
  
  if (TYPEOF(e2) == SYMSXP) {
    Rprintf("e2_expr info\n");
    Rprintf("type %s \n", type2char(TYPEOF(e2_expr)));
    Rprintf("TAG %s \n", TAG(e2_expr) == R_NilValue ? "Nil" : CHAR(PRINTNAME((TAG(e2_expr)))));
    Rprintf("length %d \n", length(e2_expr));
    Rprintf("missing? %d \n", e2_expr == R_MissingArg);
    Rprintf("\n");
    }
  
  // `TYPEOF(e2) == PROMSXP` means e2 is not R_MissingArg
  if (TYPEOF(e2) == PROMSXP || length(e1_expr) != 2 ||  CAR(e1_expr) != install("~")) {
    if (TYPEOF(e2) == PROMSXP) Rprintf("not missing e2\n");
    if (length(e1_expr) != 2) Rprintf("not unary func\n");
    if (CAR(e1_expr) != install("~")) Rprintf("not tilda\n");
    return R_NilValue;
  }
  
  SEXP expr = CDR(e1_expr);
  
  SEXP all_nms;
  all_nms = get_dd_syms(expr);
  
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


typedef struct {
  SEXP	ans;
  //int	UniqueNames;
  //int	IncludeFunctions;
  int	StoreValues;
  int	ItemCounts;
  //int	MaxCount;
} NameWalkData;

static void namewalk(SEXP s, NameWalkData *d)
{
  SEXP name;
  
  switch(TYPEOF(s)) {
  case SYMSXP:
    name = PRINTNAME(s);
    if(CHAR(name)[0] == '\0') goto ignore;
    if(d->StoreValues) {
      for(int j = 0 ; j < d->ItemCounts ; j++) {
        if(STRING_ELT(d->ans, j) == name)
          goto ignore;
      }
      SET_STRING_ELT(d->ans, d->ItemCounts, name);
    }
    d->ItemCounts++;
    
    ignore:
    break;
  case LANGSXP:
  case LISTSXP:
    while(s != R_NilValue) {
      namewalk(CAR(s), d);
      s = CDR(s);
    }
    break;
  default:
    break;
  }
}

SEXP get_all_syms(SEXP expr) {
  if (TYPEOF(expr) != LANGSXP && TYPEOF(expr) != LISTSXP)
    error("only accept LANGSXP");
  
  int savecount;
  NameWalkData data = {NULL, 0, 0};
  
  // only count 
  namewalk(expr, &data);
  savecount = data.ItemCounts;
  data.ans = allocVector(STRSXP, data.ItemCounts);
  
  data.StoreValues = 1; // TRUE
  data.ItemCounts = 0;
  // set values
  namewalk(expr, &data);
  
  // if duplicated
  if(data.ItemCounts != savecount) {
    
    SEXP str_tmp;
    PROTECT(str_tmp = data.ans);
    data.ans = allocVector(STRSXP, data.ItemCounts);
    for(int i = 0 ; i < data.ItemCounts ; i++)
      SET_STRING_ELT(data.ans, i, STRING_ELT(str_tmp, i));
    UNPROTECT(1);
  }
  
  return data.ans;
}



int ddValMod(SEXP symbol)
{
  Rprintf("hear ddValMod\n");
  //const char *buf;
  char *endp;
  int rval;
  
  Rprintf("hear ddValMod 1\n");
  const char *buf = CHAR(PRINTNAME(symbol));
  Rprintf("hear ddValMod 2\n");
  
  if( strlen(buf) > 2 && !strncmp(buf,"..",2) ) {
    Rprintf("hear ddValMod 3\n");
    buf += 2;
    rval = (int) strtol(buf, &endp, 10);
    Rprintf("hear ddValMod 4\n");
    if( *endp != '\0')
      return 0;
    else
      return rval;
  }
  return 0;

  /*
  buf = CHAR(PRINTNAME(symbol));
  Rprintf("%s %d", buf, strlen(buf));
  if(strlen(buf) >= 2 && !strncmp(buf, "..", 2)) {
    if (strlen(buf) > 2) {
      buf += 2;
      rval = (int) strtol(buf, &endp, 10);
      if(*endp != '\0')
        return 0;
      else
        return rval;
    } else {
      return -1;
    }
  }
  return 0;
  */
}

static void namewalk2(SEXP s, NameWalkData *d)
{
  SEXP name;
  Rprintf("namewalk2\n");
  switch(TYPEOF(s)) {
    case SYMSXP:
      name = PRINTNAME(s);
      Rprintf("at SYM\n");
      int i = ddValMod(s);
      Rprintf("at nw 3\n");
      const char *tmp1 =  CHAR(name);
      Rprintf("at nw 4\n");
      //Rprintf("%s  %d", tmp1, ddValMod(name));
      if (ddValMod(s)) {
        Rprintf("at nw 5\n");
        if(tmp1[0] == '\0') goto ignore;
        if(d->StoreValues) {
          Rprintf("at nw 6\n");
          for(int j = 0 ; j < d->ItemCounts ; j++) {
            if(STRING_ELT(d->ans, j) == name)
              goto ignore;
          }
          SET_STRING_ELT(d->ans, d->ItemCounts, name);
        }
        d->ItemCounts++;
      }
      Rprintf("before ignore\n");
      ignore:
      break;
    case LANGSXP:
    case LISTSXP:
      while(s != R_NilValue) {
        Rprintf("%s \n", PRINTNAME(CAR(s)));
        namewalk2(CAR(s), d);
        s = CDR(s);
      }
      break;
    default:
      break;
  }
}

SEXP get_dd_syms(SEXP expr) {
  int savecount;
  NameWalkData data = {NULL, 0, 0};
  Rprintf("dd1\n");
  // only count 
  namewalk2(expr, &data);
  savecount = data.ItemCounts;
  data.ans = allocVector(STRSXP, data.ItemCounts);
  
  data.StoreValues = 1; // TRUE
  data.ItemCounts = 0;
  // set values
  Rprintf("dd2\n");
  namewalk2(expr, &data);
  
  // if duplicated
  if(data.ItemCounts != savecount) {
    
    SEXP str_tmp;
    PROTECT(str_tmp = data.ans);
    data.ans = allocVector(STRSXP, data.ItemCounts);
    for(int i = 0 ; i < data.ItemCounts ; i++)
      SET_STRING_ELT(data.ans, i, STRING_ELT(str_tmp, i));
    UNPROTECT(1);
  }
  
  return data.ans;
  
}
