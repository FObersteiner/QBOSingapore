! -----------------------------------------------------------------------
! $Source: /home/cvs/cvsroot/f90_modules/lib/mod_monthcount.f90,v $
! $Revision: 1.1 $
! $Date: 2009/01/13 09:21:36 $
! $Author: cvs $
! -----------------------------------------------------------------------
MODULE mod_monthcount

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: monthcount

  INTERFACE monthcount
     MODULE PROCEDURE monthcount_char_int
     MODULE PROCEDURE monthcount_char
     MODULE PROCEDURE monthcount_int
  END INTERFACE

CONTAINS
  ! -
  ! - --------------------------------------------------------------
  ! -
 FUNCTION mcount(startdate, enddate, step) RESULT (n)
    ! -
    USE date_module,     ONLY : julday
    USE mod_monthbeside, ONLY : cmonthbeside, &
                                monthbeside
    ! -
    ! - Purpose: Count the number of months from startdate
    ! - -------- to enddate.
    ! - INPUT:
    ! - ------
    CHARACTER(len=*), INTENT(in) :: startdate
    CHARACTER(len=*), INTENT(in) :: enddate
    CHARACTER(len=*), INTENT(in) :: step
    ! - OUTPUT:
    ! - -------
    INTEGER :: n ! # of months from startdate to enddate

    CHARACTER(len=8) :: date, tedate

    date   = startdate(1:6)//'01'
    tedate = enddate  (1:6)//'01'
    n = 0
    IF (TRIM(step) == '12') THEN
       DO WHILE (julday(date) <= julday(tedate))
          n    = n + 1
          date = cmonthbeside(date,step)
       END DO
    ELSE
       DO WHILE (julday(date) < julday(monthbeside(tedate)))
          n    = n + 1
          date = cmonthbeside(date,step)
       END DO
    END IF
    RETURN
  END FUNCTION mcount
  ! -
  FUNCTION monthcount_char(startdate, enddate, char_step) RESULT (n)
    ! -
    ! - Purpose: Count the number of months from startdate
    ! - -------- to enddate.
    ! - INPUT:
    ! - ------
    CHARACTER(len=*), INTENT(in)           :: startdate
    CHARACTER(len=*), INTENT(in)           :: enddate
    CHARACTER(len=*), INTENT(in), OPTIONAL :: char_step
    ! - OUTPUT:
    ! - -------
    INTEGER :: n ! # of months from startdate to enddate

    CHARACTER(len=10)  :: step

    step = '1'
    IF (PRESENT(char_step)) WRITE(step,'(a)') TRIM(ADJUSTL(char_step))
    n = mcount (startdate, enddate, step)
    RETURN
  END FUNCTION monthcount_char
  FUNCTION monthcount_char_int(startdate, enddate, i_step) RESULT (n)
    ! -
    USE mod_datestep, ONLY : cstep
    ! -
    ! - Purpose: Count the number of months from startdate
    ! - -------- to enddate.
    ! - INPUT:
    ! - ------
    CHARACTER(len=*), INTENT(in) :: startdate
    CHARACTER(len=*), INTENT(in) :: enddate
    INTEGER,          INTENT(in) :: i_step
    ! - OUTPUT:
    ! - -------
    INTEGER :: n ! # of months from startdate to enddate

    CHARACTER(len=10)  :: step

    step = TRIM(cstep(i_step))
    n    = mcount (startdate, enddate, step)
    RETURN
  END FUNCTION monthcount_char_int
  FUNCTION monthcount_int(startdate, enddate, i_step) RESULT (n)
    ! -
    USE mod_datestep, ONLY : cstep
    ! -
    ! - Purpose: Count the number of months from startdate
    ! - -------- to enddate.
    ! - INPUT:
    ! - ------
    INTEGER, INTENT(in) :: startdate, enddate
    INTEGER, INTENT(in), OPTIONAL :: i_step
    ! - OUTPUT:
    ! - -------
    INTEGER :: n ! # of months from startdate to enddate

    CHARACTER(len=10) :: step
    CHARACTER(len=8)  :: date, tedate

    step = '1'
    IF (PRESENT(i_step)) step = TRIM(cstep(i_step))
    date   = TRIM(cstep(INT(startdate/100) * 100 + 1))
    tedate = TRIM(cstep(INT(enddate/100)   * 100 + 1))
    n      = mcount (date, tedate, step)
    RETURN
  END FUNCTION monthcount_int

END MODULE mod_monthcount