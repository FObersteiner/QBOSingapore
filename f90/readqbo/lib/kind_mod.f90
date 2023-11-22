!
! $Id: kind_mod.f90,v 1.6 2010/01/11 14:54:51 cvs Exp $
!
!BOP
! !MODULE: kind_mod -- declares kind parameter for single and double precision
!
! !PUBLIC DATA MEMBERS: single, double
!
! !DESCRIPTION: 
!
! !REVISION HISTORY:
!
! 03Mar2004 Markus Kunze Initial code.
!
!EOP
MODULE KIND_MOD
  
  PUBLIC
  
  !INTEGER, PARAMETER  :: SINGLE = KIND(1.0)
  !INTEGER, PARAMETER  :: DOUBLE = KIND(1D0)
  INTEGER, PARAMETER  :: SINGLE = SELECTED_REAL_KIND(6,37)
  INTEGER, PARAMETER  :: DOUBLE = SELECTED_REAL_KIND(13,307)
  INTEGER, PARAMETER  :: I4B    = SELECTED_INT_KIND(9)
  INTEGER, PARAMETER  :: I1B    = SELECTED_INT_KIND(2)
  INTEGER, PARAMETER  :: SHORT  = SELECTED_INT_KIND(4)
  INTEGER, PARAMETER  :: LONG   = SELECTED_INT_KIND(8)
  INTEGER, PARAMETER  :: HUGE   = SELECTED_INT_KIND(16)
  INTEGER, PARAMETER  :: dp = double
  INTEGER, PARAMETER  :: sp = single
  INTEGER, PARAMETER  :: lgt = KIND(.true.)
  REAL(dp), PARAMETER :: tiny_dp = TINY(0._dp)
  REAL(sp), PARAMETER :: tiny_sp = TINY(0._sp)
  REAL(dp), PARAMETER :: eps_dp  = EPSILON(0._dp)
  REAL(sp), PARAMETER :: eps_sp  = EPSILON(0._sp)
  

END MODULE KIND_MOD
