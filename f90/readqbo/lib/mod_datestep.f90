! -----------------------------------------------------------
! $Source: /home/cvs/cvsroot/f90_modules/lib/mod_datestep.f90,v $
! $Revision: 1.4 $
! $Date: 2009/03/11 09:59:08 $
! $Author: cvs $
! $Id: mod_datestep.f90,v 1.4 2009/03/11 09:59:08 cvs Exp $
! -----------------------------------------------------------
MODULE mod_datestep

  IMPLICIT NONE
  
  PRIVATE
  PUBLIC :: datestep, &
            datestep_real, &
            monthstep, &
            hourstep,  &
            istep,     &
            cstep
  ! -
CONTAINS
  ! -
  SUBROUTINE datestep(step, yyyy, mm, dd)
    ! -
    ! - create the new:  yyyy, mm, dd for
    ! - the actual date: yyyy, mm, dd.
    ! - The new date is step days from the old date
    ! -
    ! - INPUT:
    ! - ------
    INTEGER, INTENT(in)    :: step
    ! - OUTPUT:
    ! - -------
    INTEGER, INTENT(inout) :: yyyy, mm, dd
    
    INTEGER  :: istep
    INTEGER, PARAMETER :: dpy=360, mpy=12, dpm=30

    IF (step > 0) THEN
       dd = dd + step
       DO
          IF (dd > dpm) THEN
             dd = dd - dpm
             mm = mm + 1
             IF (mm > mpy) THEN
                mm = 1
                yyyy = yyyy + 1
             END IF
          ELSE
             EXIT
          END IF
       END DO
    ELSE IF (step < 0) THEN
       DO istep = 1, ABS(step)
          dd = dd - 1
          IF (dd == 0) THEN
             dd = dpm
             mm = mm - 1
             IF (mm == 0) THEN
                mm = mpy
                yyyy = yyyy - 1
             END IF
             !PRINT *,'yyyy:',yyyy,':mm:',mm,':dd:',dd
          END IF
       END DO
    ELSE
       yyyy = yyyy
       mm   = mm
       dd   = dd
    END IF
    !PRINT *,'yyyy:',yyyy,':mm:',mm,':dd:',dd
    RETURN
  END SUBROUTINE datestep
  ! -
  SUBROUTINE datestep_real(step, lnoleap, yyyy, mm, dd)
    ! -
    USE date_module, ONLY : julday, &
                            caldat
    ! -
    ! - Purpose:  create the new:  yyyy, mm, dd for
    ! - --------  the actual date: yyyy, mm, dd.
    ! -           The new date is step days from the old date
    ! -
    ! - INPUT:
    ! - ------
    INTEGER, INTENT(in) :: step
    LOGICAL, INTENT(in) :: lnoleap ! TRUE: don't use Feb 29
    ! -
    ! - OUTPUT:
    ! - -------
    INTEGER, INTENT(inout) :: yyyy, mm, dd
    
    INTEGER          :: i, incr
    LOGICAL          :: lleap  ! TRUE: 
    CHARACTER(len=8) :: date
    
    WRITE (date,"(i4.4,2(i2.2))") yyyy, mm, dd
    lleap = .NOT.lnoleap
    IF (lleap) THEN
       CALL caldat  (julday(date) + step, mm, dd, yyyy)
    ELSE
       incr = 1
       IF (step < 0) incr = -1
       DO i = 1, ABS(step)
          CALL caldat  (julday(date) + incr, mm, dd, yyyy)
          IF (lnoleap.AND.(mm == 2.AND.dd == 29)) THEN
             IF (incr < 0) THEN
                dd = 28
                mm = 2
             ELSE
                dd = 1
                mm = 3
             END IF
          END IF
          WRITE (date,"(i4.4,2(i2.2))") yyyy, mm, dd
          !PRINT *,':i:',i,':date:',date,':incr:',incr
       END DO
    END IF
    RETURN
  END SUBROUTINE datestep_real
  ! -
  ! - --------------------------------------------------------
  ! -
  SUBROUTINE monthstep(yyyy, mm, istepin)
    !
    ! - Purpose: subroutine changes yyyy and mm 
    ! - -------- acording to istep.
    ! -
    ! - INPUT:
    ! - ------
    INTEGER, INTENT(in) :: istepin
    ! - INPUT/OUTPUT:
    ! - ------------- 
    INTEGER, INTENT(inout) :: yyyy, mm
    
    INTEGER            :: istep
    INTEGER, PARAMETER :: nm=12
    
    istep = istepin
    IF (mm + istep > nm) THEN
       IF (istep > nm) THEN
          DO WHILE (mm + istep > nm)
             istep = istep - nm
             yyyy = yyyy + 1
             !PRINT *,':YYYY:',yyyy,':istep:',istep,':mm:',mm
          END DO
          mm = mm + istep
       ELSE IF (istep <= nm) THEN
          yyyy = yyyy + 1 
          mm = mm + istep - nm
       END IF
    ELSE IF (mm + istep <= 0) THEN
       DO WHILE (mm + istep <= 0)
          istep = istep + 12
          yyyy = yyyy - 1
       END DO
       mm = mm + istep
    ELSE
       mm = mm + istep
    END IF
    !PRINT *,':YYYY:',yyyy,':istep:',istep,':mm:',mm
    RETURN
  END SUBROUTINE monthstep
  ! -
  SUBROUTINE hourstep(step, lnoleap, lmod, yyyy, mm, dd, hh)
    ! -
    ! - Purpose:
    ! - --------  Calculate the new year, month day, hour 
    ! -           for a step of step hours.
    ! - INPUT:
    ! - ------
    INTEGER, INTENT(in) :: step
    LOGICAL, INTENT(in) :: lnoleap ! TRUE: don't use Feb 29
    LOGICAL, INTENT(in) :: lmod    ! TRUE: model date with 30 days
    ! -
    ! - OUTPUT:
    ! - -------
    INTEGER, INTENT(in out) :: yyyy, mm, dd, hh
    
    INTEGER :: dstep
    INTEGER :: date

    hh    = hh + step
    dstep = 0
    IF (hh < 0) THEN
       DO
          dstep = dstep - 1
          hh = hh + 24
          IF (hh >= 0) EXIT
       END DO
    ELSE
       !PRINT *,':hh:',hh
       dstep = INT(hh/24)
       !PRINT *,':dstep:',dstep
       hh    = hh - dstep*24
       !PRINT *,':hh:',hh
    END IF
    IF (lmod) THEN
       CALL datestep(dstep, yyyy, mm, dd)
    ELSE
       date = 10000*yyyy + 100*mm + dd
       !PRINT *,':date:',date,':dstep:',dstep
       CALL datestep_real(dstep, lnoleap, yyyy, mm, dd)
       !PRINT *,':date:',date,':yyyy:',yyyy,':mm:',mm,':dd:',dd,':hh:',hh
    END IF
    RETURN
  END SUBROUTINE hourstep
  ! -
  ! - -------------------------------------------------
  ! -
  FUNCTION istep(char_step) RESULT (step)
    ! -
    ! - convert CHARACTER of variable length to INTEGER
    ! -
    ! - INPUT:
    ! - ------
    CHARACTER(len=*), INTENT(in) :: char_step
    ! - OUTPUT:
    ! - -------
    INTEGER :: step

    INTEGER :: ilen
    ilen = LEN_TRIM(char_step)
    step = 0
    SELECT CASE(ilen)
    CASE(1);  READ(char_step,"(i1)")  step
    CASE(2);  READ(char_step,"(i2)")  step
    CASE(3);  READ(char_step,"(i3)")  step
    CASE(4);  READ(char_step,"(i4)")  step
    CASE(5);  READ(char_step,"(i5)")  step
    CASE(6);  READ(char_step,"(i6)")  step
    CASE(7);  READ(char_step,"(i7)")  step
    CASE(8);  READ(char_step,"(i8)")  step
    CASE(9);  READ(char_step,"(i9)")  step
    CASE(10); READ(char_step,"(i10)") step
    END SELECT
  END FUNCTION istep
  ! -
  ! - -------------------------------------------------
  ! -
  FUNCTION cstep(istep) RESULT (step)
    ! -
    ! - convert INTEGER of variable length to CHARACTER
    ! -
    ! - INPUT:
    ! - ------
    INTEGER, INTENT(in) :: istep
    ! - OUTPUT:
    ! - -------
    CHARACTER(len=10) :: step

    step = ''
    IF (istep < 10) THEN
       WRITE (step,"(i1,9x)") istep; RETURN; END IF
    IF (istep >= 10 .AND. istep < 100) THEN
       WRITE (step,"(i2,8x)") istep; RETURN; END IF
   IF (istep >= 100 .AND. istep < 1000) THEN
       WRITE (step,"(i3,7x)") istep; RETURN; END IF
   IF (istep >= 1000 .AND. istep < 10000) THEN
       WRITE (step,"(i4,6x)") istep; RETURN; END IF
   IF (istep >= 1E4 .AND. istep < 1E5) THEN
       WRITE (step,"(i5,5x)") istep; RETURN; END IF
   IF (istep >= 1E5 .AND. istep < 1E6) THEN
       WRITE (step,"(i6,4x)") istep; RETURN; END IF
   IF (istep >= 1E6 .AND. istep < 1E7) THEN
       WRITE (step,"(i7,3x)") istep; RETURN; END IF
   IF (istep >= 1E7 .AND. istep < 1E8) THEN
       WRITE (step,"(i8,2x)") istep; RETURN; END IF
   IF (istep >= 1E8 .AND. istep < 1E9) THEN
       WRITE (step,"(i9,1x)") istep; RETURN; END IF
   IF (istep >= 1E9 .AND. istep < 1E10) THEN
       WRITE (step,"(i10)") istep; RETURN; END IF
   RETURN
  END FUNCTION cstep
END MODULE mod_datestep
