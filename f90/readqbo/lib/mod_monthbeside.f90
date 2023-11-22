! -----------------------------------------------------------------------
! $Source: /home/cvs/cvsroot/f90_modules/lib/mod_monthbeside.f90,v $
! $Revision: 1.4 $
! $Date: 2010/09/24 08:38:43 $
! $Author: kunze $
! $Id: mod_monthbeside.f90,v 1.4 2010/09/24 08:38:43 kunze Exp $
! -----------------------------------------------------------------------
MODULE mod_monthbeside

  IMPLICIT NONE
  
  PRIVATE
  PUBLIC :: monthbeside, &
            cmonthbeside
  
  INTERFACE monthbeside
     MODULE PROCEDURE monthbeside_char_char
     MODULE PROCEDURE monthbeside_char_int
     MODULE PROCEDURE monthbeside_int_int
  END INTERFACE
  INTERFACE cmonthbeside
     MODULE PROCEDURE cmonthbeside_char_char
     MODULE PROCEDURE cmonthbeside_char_int
     MODULE PROCEDURE cmonthbeside_int_int
  END INTERFACE
  ! -
CONTAINS
  ! -
  ! - INTEGER FUNCTION monthbeside
  ! - ----------------------------
  ! -
  FUNCTION monthbeside_char_char(date, char_step) RESULT (idate)
    ! -
    USE mod_datesplit, ONLY : datesplit
    USE mod_datestep,  ONLY : monthstep, &
                              istep
    ! -
    ! - INPUT:
    ! - ------
    CHARACTER(len=*),           INTENT(in) :: date
    CHARACTER(len=*), OPTIONAL, INTENT(in) :: char_step
    ! -
    ! - OUTPUT:
    ! - -------
    INTEGER :: idate

    INTEGER :: yyyy, mm, dd, i_step

    i_step = 1 
    IF (PRESENT(char_step)) i_step = istep(char_step)
    CALL datesplit (date, yyyy, mm, dd)
    CALL monthstep (yyyy, mm, i_step)
    idate = 10000*yyyy + 100*mm + dd
    RETURN
  END FUNCTION monthbeside_char_char
  FUNCTION monthbeside_char_int(date, step) RESULT (idate)
    ! -
    USE mod_datesplit, ONLY : datesplit
    USE mod_datestep,  ONLY : monthstep
    ! -
    ! - INPUT:
    ! - ------
    CHARACTER(len=*), INTENT(in) :: date
    INTEGER,          INTENT(in) :: step
    ! -
    ! - OUTPUT:
    ! - -------
    INTEGER :: idate

    INTEGER :: yyyy, mm, dd
  
    CALL datesplit (date, yyyy, mm, dd)
    CALL monthstep (yyyy, mm, step)
    idate = 10000*yyyy + 100*mm + dd
    RETURN
  END FUNCTION monthbeside_char_int
  FUNCTION monthbeside_int_int(date, i_step) RESULT (idate)
    ! -
    USE mod_datesplit, ONLY : datesplit
    USE mod_datestep,  ONLY : monthstep
    ! -
    ! - INPUT:
    ! - ------
    INTEGER,           INTENT(IN) :: date
    INTEGER, OPTIONAL, INTENT(IN) :: i_step
    ! -
    ! - OUTPUT:
    ! - -------
    INTEGER :: idate

    INTEGER :: yyyy, mm, dd, step

    step = 1
    IF (PRESENT(i_step)) step = i_step
    CALL datesplit (date, yyyy, mm, dd)
    CALL monthstep (yyyy, mm, step)
    idate = 10000*yyyy + 100*mm + dd
    RETURN
  END FUNCTION monthbeside_int_int
  ! -
  ! - INTEGER FUNCTION cmonthbeside
  ! - --------------------------------------------------------
  ! -
  FUNCTION cmonthbeside_char_char(date, char_step) RESULT (cdate)
    ! -
    USE mod_datestep, ONLY : istep
    ! - INPUT:
    ! - ------
    CHARACTER(len=*), INTENT(in)           :: date
    CHARACTER(len=*), INTENT(in), OPTIONAL :: char_step
    ! - OUTPUT:
    ! - -------
    CHARACTER(len=8) :: cdate

    INTEGER :: step
    ! -
    step   = 1 
    IF (PRESENT(char_step)) step = istep(char_step)
    CALL do_cmonthbeside (date(1:8), step, cdate)
    RETURN
  END FUNCTION cmonthbeside_char_char
  ! -
  FUNCTION cmonthbeside_char_int(date, istep) RESULT (cdate)
    ! - INPUT:
    ! - ------
    CHARACTER(len=*), INTENT(in) :: date
    INTEGER,          INTENT(in) :: istep
    ! - OUTPUT:
    ! - -------
    CHARACTER(len=8) :: cdate
    ! -
    CALL do_cmonthbeside (date(1:8), istep, cdate)
    RETURN
  END FUNCTION cmonthbeside_char_int
  ! -
  FUNCTION cmonthbeside_int_int(date, i_step) RESULT (cdate)
    ! - INPUT:
    ! - ------
    INTEGER, INTENT(in)           :: date
    INTEGER, INTENT(in), OPTIONAL :: i_step
    ! - OUTPUT:
    ! - -------
    CHARACTER(len=8) :: cdate

    INTEGER           :: step
    CHARACTER(len=10) :: cdate1
    ! -
    cdate1 = '0000000000'
    step = 1
    IF (PRESENT(i_step)) step = i_step
    ! - / convert INTEGER date to CHARACTER /
    IF (date > 99999999) THEN
       WRITE (cdate1,"(i10.10)") date
    ELSE
       WRITE (cdate1,"(i10.10)") date*100
    END IF
    CALL do_cmonthbeside (cdate1(1:8), step, cdate)
    RETURN
  END FUNCTION cmonthbeside_int_int
  SUBROUTINE do_cmonthbeside (date, step, cdate)
    ! -
    USE mod_datesplit, ONLY : datesplit
    USE mod_datestep,  ONLY : monthstep
    ! - / INPUT /
    CHARACTER(len=*), INTENT(in) :: date
    INTEGER,          INTENT(in) :: step
    ! - / OUTPUT /
    CHARACTER(len=*), INTENT(out) :: cdate
    ! -
    INTEGER :: yyyy, mm, dd, hh
    INTEGER :: ilen
    ! -
    ilen = LEN_TRIM(date)
    IF (ilen == 10) THEN
       CALL datesplit (date, yyyy, mm, dd, hh)
    ELSE
       CALL datesplit (date, yyyy, mm, dd)
    END IF
    CALL monthstep (yyyy, mm, step)
    IF (ilen == 10) THEN
       WRITE (cdate,"(i4.4,3(i2.2))") yyyy, mm, dd, hh
    ELSE
       WRITE (cdate,"(i4.4,2(i2.2))") yyyy, mm, dd
    END IF
    RETURN
  END SUBROUTINE do_cmonthbeside
END MODULE mod_monthbeside
