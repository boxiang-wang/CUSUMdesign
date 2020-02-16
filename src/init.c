#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Fortran calls */
extern void F77_NAME(getden)(double *ref, double *di, double *denrat, int *echo, int *change);
extern void F77_NAME(calcus)(double *di, double *ref, int *ndis, double *denrat, double *arg2, 
    double *wl, double *wu, double *regarl, double *firarl, double *ssarl, double *eps, double *esterr, int *ifault);
extern void F77_NAME(cusrlg)(double *di, double *ref, double *cumdis, double *denrat, double *arg2, double *winsrl, 
    double *winsru, double *regarl, double *firarl, double *ssarl, double *eps, double *esterr, int *ifault);
extern void F77_NAME(cmpar)(double *r);
extern void F77_NAME(cmpar2)(int *i);

static const R_FortranMethodDef FortranEntries[] = {
    {"getden",  (DL_FUNC) &F77_SUB(getden),  5},
    {"calcus",  (DL_FUNC) &F77_SUB(calcus),  13},
    {"cusrlg",  (DL_FUNC) &F77_SUB(cusrlg),  13},
    {"cmpar",   (DL_FUNC) &F77_SUB(cmpar),   1},
    {"cmpar2",  (DL_FUNC) &F77_SUB(cmpar2),  1},
    {NULL, NULL, 0}
};

void R_init_CUSUMdesign(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
