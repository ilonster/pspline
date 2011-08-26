#ifndef ADPAK_H
#define ADPAK_H

#include "f77name.h"
#include "transp_util.h"

//
// ---------- fortran adpak interface -------------
//
// see adpak_mod.f90 and supporting files for documentation
//
extern "C" void F77NAME(adp_reset)(const int& knucz, const int& kladen,  const int& kladtip,
				   const int& kleci, const int& kldrmlt,       int& ier) ;

extern "C" void F77NAME(adp_ioniz_r4)(const fp_32& te, const fp_32& ane, fp_32* telion) ;
extern "C" void F77NAME(adp_ioniz_r8)(const fp_64& te, const fp_64& ane, fp_64* telion) ;

extern "C" void F77NAME(adp_recom_r4)(const fp_32& te, const fp_32& ane, fp_32* telrec) ;
extern "C" void F77NAME(adp_recom_r8)(const fp_64& te, const fp_64& ane, fp_64* telrec) ;

extern "C" void F77NAME(adp_rad_r4)(const fp_32& te, const fp_32& ane, const int& ibrem, fp_32* telrad) ;
extern "C" void F77NAME(adp_rad_r8)(const fp_64& te, const fp_64& ane, const int& ibrem, fp_64* telrad) ;

extern "C" void F77NAME(adp_brem_r4)(const fp_32& te, const fp_32& ane, fp_32* telbrem) ;
extern "C" void F77NAME(adp_brem_r8)(const fp_64& te, const fp_64& ane, fp_64* telbrem) ;


#endif
