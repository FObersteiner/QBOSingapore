! ----------------------------------------------------------------------------
! $Source: /home/cvs/cvsroot/f90_modules/lib/date_module.f90,v $
! $Revision: 1.16 $
! $Date: 2010/12/01 15:39:43 $
! $Author: kunze $
! ----------------------------------------------------------------------------
MODULE date_module

  USE kind_mod,     ONLY : sp

  IMPLICIT NONE
  
  PRIVATE
  PUBLIC :: julday &
          , jul    &
          , doy    &
          , caldat &
          , leap_year

  INTERFACE julday
     MODULE procedure julday_characters, julday_integers
  END INTERFACE
  
  INTERFACE jul
     MODULE procedure jul_char, jul_int
  END INTERFACE
  
  INTERFACE doy
     MODULE procedure doy_characters, doy_integers
  END INTERFACE

  INTERFACE caldat
     MODULE PROCEDURE caldat_int, caldat_char
  END INTERFACE

CONTAINS
  ! -
  INTEGER FUNCTION julday_characters(date)
    ! -
    USE kind_mod, ONLY : sp => single
    !
    !  (C) Copr. 1986-92 Numerical Recipes Software &5A2,V.
    !
    CHARACTER(LEN=*), INTENT(in) :: date     
    
    INTEGER            :: id, iyyy, mm, ja, jm, jy
    INTEGER, PARAMETER :: IGREG = 15 + 31*(10 + 12*1582)
    
    READ (date,"(i4,i2,i2)")iyyy, mm, id
    jy=iyyy
    
    IF (jy == 0) THEN
       PRINT *, 'julday: there is no year zero'
       CALL abort
    END IF
    IF (jy < 0) jy = jy + 1
    IF (mm > 2) THEN
       jm = mm + 1
    ELSE
       jy = jy - 1
       jm = mm + 13
    END IF
    !
    ! Das julianische Startdatum 0 ist der 1. Januar 4713 v. Chr.
    ! Der julianische Tag des 01.01.0000 ist 1720995
    !
    julday_characters = int(365.25_sp*jy)+int(30.6001_sp*jm)+id+1720995
    IF (id+31*(mm+12*iyyy) >= IGREG) THEN
       ja = int(0.01_sp*jy)
       julday_characters = julday_characters + 2 - ja + int(0.25_sp*ja)
    END IF
    RETURN
  END FUNCTION julday_characters
  INTEGER FUNCTION julday_integers(date)
     ! -
     USE kind_mod, ONLY : sp => single
     !
     !  (C) Copr. 1986-92 Numerical Recipes Software &5A2,V.
     !
     integer                 :: date     
     integer                 :: id,iyyy,mm
     integer,parameter       :: IGREG=15+31*(10+12*1582)
     integer                 :: ja,jm,jy
     
     iyyy = int(date/10000)
     mm = int((date - iyyy*10000)/100)
     id = date - iyyy*10000 - mm*100
     jy = iyyy
     
     if (jy.eq.0) then
        print *, 'julday: there is no year zero'
        call abort
     end if
     if (jy.lt.0) jy=jy+1
     if (mm.gt.2) then
        jm = mm + 1
     else
        jy = jy - 1
        jm = mm + 13
     endif
     !
     ! Das julianische Startdatum 0 ist der 1. Januar 4713 v. Chr.
     ! Der julianische Tag des 01.01.0000 ist 1720995
     !
     julday_integers = int(365.25_sp*jy) + int(30.6001_sp*jm) + id + 1720995
     if (id + 31*(mm + 12*iyyy).ge.IGREG) then
        ja = int(0.01_sp*jy)
        julday_integers = julday_integers + 2-ja + int(0.25_sp*ja)
     endif
     return
   END FUNCTION julday_integers
   ! -
   ! - -----------------------------------------------------------------
   ! -
   INTEGER FUNCTION jul_char(date)
     ! - Purpose:
     ! - --------  Julian day (jul) is computed from Gregorian 
     ! -           day, month and year (id, im, iy)
     ! -           (conversion algorithm is due to 
     ! -            Henry F. Fliegel and Thomas C. Van Flandern)
     ! - INPUT:
     ! - ------
     CHARACTER(LEN=*), INTENT(in) :: date     
     
     INTEGER :: id, iy, im
     ! -
     ! - convert character date to interger parts -
     ! -
     READ (date,"(i4,i2,i2)") iy, im, id
     
     jul_char = ( 1461 * ( iy + 4800 + ( im - 14 ) / 12 ) ) / 4 +        &
                ( 367 * ( im - 2 - 12 * ( ( im - 14 ) / 12 ) ) ) / 12 -  &
                ( 3 * ( ( iy + 4900 + ( im - 14 ) / 12 ) / 100 ) ) / 4 + &
                id - 32075
     RETURN
   END FUNCTION jul_char
   INTEGER FUNCTION jul_int(date)
     ! - Purpose:
     ! - --------  Julian day (jul) is computed from Gregorian 
     ! -           day, month and year (id, im, iy)
     ! -           (conversion algorithm is due to 
     ! -            Henry F. Fliegel and Thomas C. Van Flandern)
     ! - INPUT:
     ! - ------
     INTEGER, INTENT(in) :: date     
     
     INTEGER :: id, iy, im
     ! -
     ! - split date to interger parts -
     ! -
     iy = int(date/10000)
     im = int((date - iy*10000)/100)
     id = date - iy*10000 - im*100
     
     jul_int = ( 1461 * ( iy + 4800 + ( im - 14 ) / 12 ) ) / 4 +        &
               ( 367 * ( im - 2 - 12 * ( ( im - 14 ) / 12 ) ) ) / 12 -  &
               ( 3 * ( ( iy + 4900 + ( im - 14 ) / 12 ) / 100 ) ) / 4 + &
               id - 32075
     RETURN
   END FUNCTION jul_int
   SUBROUTINE caldat_int(julian, mm, id, iyyy)
      !
      INTEGER, INTENT(in)  :: julian
      INTEGER, INTENT(out) :: mm, id, iyyy 
      ! -
      integer,parameter  :: IGREG=2299161
      integer            :: ja,jalpha,jb,jc,jd,je
     
      if(julian.ge.IGREG)then
        jalpha=int(((julian-1867216)-0.25)/36524.25)
        ja=julian+1+jalpha-int(0.25*jalpha)
      else
        ja=julian
      endif
      jb=ja+1524
      jc=int(6680.+((jb-2439870)-122.1)/365.25)
      jd=365*jc+int(0.25*jc)
      je=int((jb-jd)/30.6001)
      id=jb-jd-int(30.6001*je)
      mm=je-1
      if(mm.gt.12)mm=mm-12
      iyyy=jc-4715
      if(mm.gt.2)iyyy=iyyy-1
      if(iyyy.le.0)iyyy=iyyy-1
      RETURN
   END SUBROUTINE caldat_int
   SUBROUTINE caldat_char(julian, cdate)
      ! -
      INTEGER,          INTENT(in) :: julian
      CHARACTER(len=8), INTENT(out) :: cdate
      ! -
      integer            :: id, iyyy, mm
      integer,parameter  :: IGREG=2299161
      integer            :: ja,jalpha,jb,jc,jd,je
     
      if(julian.ge.IGREG)then
        jalpha=int(((julian-1867216)-0.25)/36524.25)
        ja=julian+1+jalpha-int(0.25*jalpha)
      else
        ja=julian
      endif
      jb=ja+1524
      jc=int(6680.+((jb-2439870)-122.1)/365.25)
      jd=365*jc+int(0.25*jc)
      je=int((jb-jd)/30.6001)
      id=jb-jd-int(30.6001*je)
      mm=je-1
      if(mm.gt.12)mm=mm-12
      iyyy=jc-4715
      if(mm.gt.2)iyyy=iyyy-1
      if(iyyy.le.0)iyyy=iyyy-1
      WRITE(cdate,"(i4.4,i2.2,i2.2)") iyyy, mm, id
      RETURN
   END SUBROUTINE caldat_char
!
!  (C) Copr. 1986-92 Numerical Recipes Software &5A2,V.

   INTEGER FUNCTION doy_characters(date, lsy, lmod)
     !  Purpose:
     !    This program calculates the day of year corresponding to a
     !    specified date. 
     !
     !  Record of revisions:
     !      Date       Programmer          Description of change
     !      ====       ==========          =====================
     !    11/13/06    S. J. Chapman        Original code (doy.f90, Chap.4)
     !    09/25/07    M. Kunze             Splited code into functions
     !
     ! - INPUT:  date   - CHARACTER - input date as yyyymmdd
     ! -         lsy    - LOGICAL   - .TRUE. include leap days 
    CHARACTER (len=8), INTENT(in)  :: date
    LOGICAL, OPTIONAL, INTENT(in)  :: lsy  ! .TRUE.: with leap year
    LOGICAL, OPTIONAL, INTENT(in)  :: lmod ! .TRUE.: model date with 30 days

    INTEGER :: iyyyy, imm, idd  ! used to convert character input to integer
    INTEGER :: leap_day=0
    INTEGER :: i
    LOGICAL :: mod, leap
    
    leap = .TRUE.
    mod  = .FALSE.
    IF (PRESENT(lsy))  leap = lsy
    IF (PRESENT(lmod)) mod  = lmod
    READ (date,"(i4,i2,i2)") iyyyy, imm, idd
    ! -
    ! - extra day for leap years -
    ! -
    leap_day = 0
    IF (leap) THEN
       IF (leap_year(iyyyy)) leap_day = 1
    END IF
    doy_characters = idd
    IF (mod) THEN
       doy_characters = doy_characters + (imm - 1) * 30
    ELSE
       DO i = 1, imm-1
          ! -
          ! - Add days in months from January to last month 
          ! -
          SELECT CASE (i)
          CASE (1,3,5,7,8,10,12)
             doy_characters = doy_characters + 31
          CASE (4,6,9,11)
             doy_characters = doy_characters + 30
          CASE (2)
             doy_characters = doy_characters + 28 + leap_day
          END SELECT
       END DO
    END IF
    RETURN
  END FUNCTION doy_characters
  INTEGER FUNCTION doy_integers(date, lsy, lmod)
     !  Purpose:
     !    This program calculates the day of year corresponding to a
     !    specified date. 
     !
     !  Record of revisions:
     !      Date       Programmer          Description of change
     !      ====       ==========          =====================
     !    11/13/06    S. J. Chapman        Original code (doy.f90, Chap.4)
     !    09/25/07    M. Kunze             Splited code into functions
     !
     ! - INPUT:  date   - INTEGER   - input date as yyyymmdd
     ! -         lsy    - LOGICAL   - .TRUE. include leap days 
    INTEGER,           INTENT(in)  :: date
    LOGICAL, OPTIONAL, INTENT(in)  :: lsy  ! .TRUE.: with leap year
    LOGICAL, OPTIONAL, INTENT(in)  :: lmod ! .TRUE.: model date with 30 days

    INTEGER :: iyyyy, imm, idd  ! used to convert character input to integer
    INTEGER :: leap_day=0
    INTEGER :: i
    LOGICAL :: mod, leap
    
    leap = .TRUE.
    mod  = .FALSE.
    IF (PRESENT(lsy))  leap = lsy
    IF (PRESENT(lmod)) mod  = lmod
    iyyyy = INT(date/10000)
    imm   = INT((date - iyyyy*10000)/100)
    idd   = date - iyyyy*10000 - imm*100
    ! -
    ! - extra day for leap years -
    ! -
    leap_day = 0
    IF (leap) THEN
       IF (leap_year(iyyyy)) leap_day = 1
    END IF
    doy_integers = idd
    IF (mod) THEN
       doy_integers = doy_integers + (imm - 1) * 30
    ELSE
       DO i = 1, imm-1
          ! -
          ! - Add days in months from January to last month 
          ! -
          SELECT CASE (i)
          CASE (1,3,5,7,8,10,12)
             doy_integers = doy_integers + 31
          CASE (4,6,9,11)
             doy_integers = doy_integers + 30
          CASE (2)
             doy_integers = doy_integers + 28 + leap_day
          END SELECT
       END DO
    END IF
    RETURN
   END FUNCTION doy_integers
   LOGICAL FUNCTION leap_year(year)
     ! - Purpose:
     ! - --------
     ! -  This function checks the input year for leap year.
     ! - 
     ! - Record of revisions:
     !      Date       Programmer          Description of change
     !      ====       ==========          =====================
     !    11/13/06    S. J. Chapman        Original code (doy.f90, Chap.4)
     !    09/25/07    M. Kunze             Splited code into functions
     ! 
     ! - INPUT:   year      -  INTEGER - year as 4 digit number
     ! - OUTPUT:  leap_year -  LOGICAL - .TRUE. year is a leap year
     ! - ----------------------------------------------------------------
     INTEGER, INTENT(in) :: year
     
     ! - Check for leap year -
     IF ( MOD(year,400) == 0 ) THEN
        leap_year = .TRUE.     ! Years divisible by 400 are leap years
     ELSE IF ( MOD(year,100) == 0 ) THEN
        leap_year = .FALSE.    ! Other centuries are not leap years
     ELSE IF ( MOD(year,4) == 0 ) THEN
        leap_year = .TRUE.     ! Otherwise every 4th year is a leap year
     ELSE
        leap_year = .FALSE.    ! Other years are not leap years
     END IF
     
     RETURN
   END FUNCTION leap_year
   
END MODULE date_module
