#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

void F77_SUB(geodistv)();
void R_digest_C(char **, int *, int *, char **);
SEXP R_digest(SEXP, SEXP, SEXP);

static R_FortranMethodDef FortEntries[] = {
     {"geodistv", (DL_FUNC) &F77_SUB(geodistv), 6},
    {NULL, NULL, 0}
};

static R_NativePrimitiveArgType digest_type[4] =  {STRSXP, INTSXP, INTSXP, STRSXP};
static R_CMethodDef cEntries[] = {
       {"R_digest_C", (DL_FUNC) &R_digest_C, 4, digest_type}, 
        {NULL, NULL, 0, NULL}
     };

static R_CallMethodDef callEntries[]  = {
       {"R_digest", (DL_FUNC) &R_digest, 3},
       {NULL, NULL, 0}
     }; 
        
void
R_init_SoDA(DllInfo *info)
{
  /* Register routines, allocate resources. */
  R_registerRoutines(info, cEntries /* Centries*/, callEntries /*CallEntries*/,
		       FortEntries, NULL /*ExternEntries*/);
}
          
void
R_unload_SoDA(DllInfo *info)
{
  /* Release resources. */
}
