! ---------------------------------------------------------
! $Source: /home/cvs/cvsroot/f90_modules/lib/mod_datesplit.f90,v $
! $Revision: 1.4 $
! $Date: 2009/03/20 15:41:12 $
! $Author: cvs $
! $Id: mod_datesplit.f90,v 1.4 2009/03/20 15:41:12 cvs Exp $
! ---------------------------------------------------------
MODULE mod_datesplit

  IMPLICIT NONE
  
  PRIVATE
  PUBLIC :: datesplit
  
  INTERFACE datesplit
     MODULE PROCEDURE datesplit_char_char
     MODULE PROCEDURE datesplit_char_int
     MODULE PROCEDURE datesplit_int_int
  END INTERFACE
  ! -
CONTAINS
  ! -
  ! - SUBROUTINE datesplit:
  ! - ---------------------
  ! - splits the input date into three parts:
  ! - yyyy, mm, dd
  ! -
  SUBROUTINE datesplit_char_int(date, yyyy, mm, dd, hh)
    ! -
    USE messages_mod, ONLY : err
    ! -
    ! - INPUT:
    ! - ------
    CHARACTER(LEN=*), INTENT(in) :: date
    ! -
    ! - OUTPUT:
    ! - -------
    INTEGER, INTENT(out) :: yyyy, mm, dd
    INTEGER, INTENT(out), OPTIONAL :: hh

    INTEGER :: idate
    INTEGER :: ilen, ihh
    LOGICAL :: lhr
    
    ilen = LEN_TRIM(date)
    lhr  = .FALSE.
    IF (PRESENT(hh)) lhr = .TRUE.
    IF (lhr) THEN
       IF (ilen == 8) THEN
          READ (date,"(i8)") idate
          ihh = 0
       ELSE IF (ilen == 10) THEN
          READ (date,"(i8,i2)") idate, ihh
       ELSE
          CALL err (':datesplit_char_int:wrong date format!')
       END IF
       yyyy = INT (idate/10000)
       mm   = INT ((idate - yyyy*10000)/100)
       dd   = idate - yyyy*10000 - mm*100
       hh   = ihh
    ELSE
       IF (ilen == 8) THEN
          !PRINT *,':datesplit_char_int:date:',date,':ilen:',ilen
          READ (date,"(i8)") idate
       ELSE IF (ilen == 10) THEN
          READ (date,"(i8,i2)") idate, ihh
       ELSE
          CALL err (':datesplit_char:wrong date format!')
       END IF
       yyyy = INT (idate/10000)
       mm   = INT ((idate - yyyy*10000)/100)
       dd   = idate - yyyy*10000 - mm*100
    END IF
    RETURN
  END SUBROUTINE datesplit_char_int
  SUBROUTINE datesplit_char_char(date, yyyy, mm, dd, hh)
    ! -
    USE messages_mod, ONLY : err
    ! -
    ! - INPUT:
    ! - ------
    CHARACTER(LEN=*), INTENT(in) :: date
    ! -
    ! - OUTPUT:
    ! - ------- 
    CHARACTER(LEN=*), INTENT(out) :: yyyy, mm, dd
    CHARACTER(LEN=*), INTENT(out), OPTIONAL :: hh
    ! -
    INTEGER :: ilen
    ! -
    ilen = LEN_TRIM(date)
    IF (.NOT.PRESENT(hh)) THEN
       READ (date,"(a4,2(a2))") yyyy, mm, dd
    ELSE
       IF (ilen == 10) THEN
          READ (date,"(a4,3(a2))") yyyy, mm, dd, hh
       ELSE IF (ilen == 8) THEN
          READ (date,"(a4,2(a2))") yyyy, mm, dd
          hh = '00'
       ELSE
          CALL err (':datesplit_char:wrong date format!')
       END IF
    END IF
    RETURN
  END SUBROUTINE datesplit_char_char
  SUBROUTINE datesplit_int_int(date, yyyy, mm, dd, hh)
    ! -
    ! - INPUT:
    ! - ------
    INTEGER, INTENT(in) :: date
    ! -
    ! - OUTPUT:
    ! - ------- 
    INTEGER, INTENT(out) :: yyyy, mm, dd
    INTEGER, INTENT(out), OPTIONAL :: hh
    ! -
    LOGICAL :: lhr
    
    lhr  = .FALSE.
    IF (PRESENT(hh)) lhr = .TRUE.
    IF (lhr) THEN
       yyyy = INT (date/1000000)
       mm   = INT ((date - yyyy*1000000)/10000)
       dd   = INT ((date - yyyy*1000000 - mm*10000)/100)
       hh   = date - yyyy*1000000 - mm*10000 - dd*100
    ELSE
       yyyy = INT (date/10000)
       mm   = INT ((date - yyyy*10000)/100)
       dd   = date - yyyy*10000 - mm*100
    END IF
    RETURN
  END SUBROUTINE datesplit_int_int

END MODULE mod_datesplit
