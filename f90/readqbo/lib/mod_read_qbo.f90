! -
! - $Id: mod_read_qbo.f90,v 1.3 2012/11/07 08:10:01 kunze Exp $
! - 
MODULE mod_read_qbo

  USE kind_mod, ONLY : sp
  
  IMPLICIT NONE
  
  PRIVATE
  PUBLIC :: rqbo,      &
            rqbo_sing, &
            rshea,     &
            read_100
  
CONTAINS
  
  SUBROUTINE rqbo(ifile, sdate, edate, lun, missing, data)
    ! INPUT:
    ! ======
    CHARACTER(LEN=*), INTENT(IN) :: ifile        ! input file name
    CHARACTER(LEN=*), INTENT(IN) :: sdate, edate ! start end end date
    INTEGER,          INTENT(IN) :: lun          ! logical unit number
    REAL(sp),         INTENT(IN) :: missing      ! value for missing data
    ! OUTPUT:
    ! =======
    REAL(sp),         INTENT(OUT) :: data(:,:,:) ! qbo data (nyear,nmonth,nlev)
    
    INTEGER, parameter :: nmonth=12, nlev=7
    INTEGER, PARAMETER :: miss_in=-99999
    INTEGER            :: ierr          ! Error return
    INTEGER            :: i, iyear, imonth 
    INTEGER            :: nzout         ! number of output levels
    INTEGER            :: ndel, station
    INTEGER            :: idata
    INTEGER            :: nn(nlev)
    CHARACTER(len=5)   :: cdata(nlev)
    CHARACTER(len=4)   :: yymm, syymm, eyymm
    CHARACTER(len=2)   :: yy
    CHARACTER(len=54)  :: header
    LOGICAL            :: ldata
      
    ! --> open the data file, with the QBO data:                        
    ! ------------------------------------------                        
  
    open (lun, file = ifile, iostat = ierr) 
    if (ierr /= 0) then 
       print *, 'NOT open data file ',ifile 
       call exit(1)
    end if

    read (sdate,"(2x,a4)") syymm
    read (edate,"(2x,a4)") eyymm

    !     ignore header data until startdate:                               
    !     -----------------------------------                               
    do i = 1, 9 
       read(lun,'(a54)') header
    end do
100 format( i5, 1x, a4, i1, 7(a5,i2) ) 

    data  = missing
    ldata = .false.
    iyear = 0
    do  
       idata = 0
       read(lun, 100, end=110) station, yymm, ndel,     &
            &           ( cdata(i), nn(i), i = 1, nlev )
       if (yymm == syymm) ldata = .true.
       if (ldata) then
          !
          ! initialise iyear at the first data month
          !
          if (iyear == 0) iyear = 1
          read(yymm,"(a2,i2)") yy, imonth
          !print *,'iyear:',iyear,' imonth:',imonth,' yymm:',yymm,':cdata(:):',cdata(:)
          DO i = 1, nlev 
             IF ( TRIM(cdata(i)) == '' ) THEN
                data(i,imonth,iyear) = missing
             ELSE IF (TRIM(cdata(i)) == ' -999') THEN
                PRINT *,'yy:',yy,' imonth:',imonth, &
                        ':TRIM(cdata(i):',TRIM(cdata(i)),':'
                data(i,imonth,iyear) = missing
             ELSE
                READ(cdata(i),"(i5)") idata
                data(i,imonth,iyear) = REAL(idata,sp)*0.1_sp
             END IF
          END DO
          print *,'iyear:',iyear,' imonth:',imonth,' yymm:',yymm,':cdata(:):',cdata(:)
          ! 
          ! increment iyear at the of the year
          !
          if (imonth == nmonth) iyear = iyear + 1
       end if
       if (yymm == eyymm) ldata = .false.
    end do
110 close(lun) 
    RETURN
  END SUBROUTINE rqbo
  SUBROUTINE rqbo_sing(ifile, sdate, edate, lun, missing, data)
    ! INPUT:
    ! ======
    CHARACTER(LEN=*), INTENT(IN) :: ifile        ! input file name
    CHARACTER(LEN=*), INTENT(IN) :: sdate, edate ! start end end date
    INTEGER,          INTENT(IN) :: lun          ! logical unit number
    REAL,             INTENT(IN) :: missing      ! value for missing data
    ! OUTPUT:
    ! =======
    REAL(sp),         INTENT(OUT) :: data(:,:,:) ! qbo data (nyear,nmonth,nlev)
 
    INTEGER             :: i, ierr, yyyy, isyear, ieyear, iyear
    INTEGER             :: lev, year, month
    INTEGER, parameter  :: nlev=15, nmonth=12, start=1987
    INTEGER             :: idata(nmonth)
    CHARACTER(len=3)    :: level
    CHARACTER(len=54)   :: header
    logical             :: ldata

    ! --> open the data file, with the QBO data:                        
    ! ------------------------------------------

    open (lun, file = ifile, iostat = ierr) 
    if (ierr /= 0) then 
       print *, 'NOT open data file ',ifile 
       call exit(1)
    end if

    read (sdate,"(i4,2x)") isyear
    read (edate,"(i4,2x)") ieyear

    ! --> ignore header lines:
    ! ------------------------                                     
    do i = 1, 3 
       read(lun,'(a54)') header
    end do

100 format( a3, 12(i5) ) 
    
    ldata = .false.
    if (isyear < start) then
       iyear = start - isyear
    else
       iyear = 0
    end if
    year  = 0
    do
       year = year + 1
       do i = 1, 2 
          read(lun,'(i4,a)', end=120) yyyy, header
          if (yyyy == start + year - 1) goto 110 
       end do
110    read(lun,'(a54)', end=120) header
       if (isyear < yyyy .AND. ieyear > yyyy) ldata = .true.
       if (ldata) iyear = iyear + 1
       do lev = nlev, 1, -1 
          read(lun, 100, end=120) level,( idata(month), month = 1, nmonth )
          PRINT *,level, idata
          if (ldata .AND. TRIM(level) /=  '') then
             do month = 1, nmonth 
                if (idata(month) == -999) then
                   data(lev,month,iyear) = missing
                else
                   data(lev,month,iyear) = real(idata(month))*0.1
                end if
             end do
          else
             data(lev,:,iyear) = missing
          end if
       end do
       if (ieyear == yyyy) ldata = .false.
    end do
120 close(lun)
    RETURN
  END SUBROUTINE rqbo_sing
  SUBROUTINE read_100(missing, file, sdate, edate, lun, qbo)
    ! INPUT:
    ! ======
    REAL(sp),         INTENT(IN) :: missing      ! value for missing data
    CHARACTER(LEN=*), INTENT(IN) :: file         ! input file name
    CHARACTER(LEN=*), INTENT(IN) :: sdate, edate ! start end end date
    INTEGER,          INTENT(IN) :: lun          ! logical unit number
    ! OUTPUT:
    ! =======
    REAL(sp), DIMENSION(:,:)   :: qbo
    
    INTEGER, parameter :: nlev=1, nmonth=12, start=1997
    INTEGER            :: ierr, yyyy, mm, month, i, iyear
    INTEGER            :: isyear, ieyear, iyyyy
    real(sp)           :: direction, data
    CHARACTER(len=80)  :: header
    logical            :: ldata

    open (lun, file = file,  &
         &   status = 'OLD', &
         &   iostat = ierr)
    if (ierr /= 0) then
       print *, 'NOT open data file ',file
       stop
    end if
    
    ! - read integer start and end year - 
    ! -----------------------------------
    
    read (sdate,"(i4,2x)") isyear
    read (edate,"(i4,2x)") ieyear

    !     ignore header data until startdate:
    !     -----------------------------------
    do i = 1, 21
       read(lun,'(a80)') header
    end do
    
    if (isyear < start) then
       iyear = start - isyear + 1
       iyyyy = isyear + iyear - 1
    else
       iyear = 1
    end if
    years:do
       do month = 1, nmonth
          read(lun, *, end = 11) yyyy, mm, data, direction
          !print *, 'iyyyy:',iyyyy,' iyear:',iyear,' ',yyyy, mm, data, direction
          if (iyyyy == yyyy) ldata = .TRUE.
          if (ldata) then
             !print *, 'iyear:',iyear,' ',yyyy, mm, data, direction
             qbo(month,iyear) = data
             if (month == nmonth) iyear = iyear + 1
          end if
       end do
       if (yyyy == ieyear) ldata = .FALSE.
       read(lun,'(a)', end = 11) header
    end do years
11  close(lun)
    
    RETURN
  END SUBROUTINE read_100
  SUBROUTINE rshea(ifile, sdate, edate, lun, missing, data)
    ! -
    USE mod_monthbeside, ONLY : cmonthbeside
    USE mod_monthcount,  ONLY : monthcount
    ! INPUT:
    ! ======
    CHARACTER(LEN=*), INTENT(IN) :: ifile        ! input file name
    CHARACTER(LEN=*), INTENT(IN) :: sdate, edate ! start end end date
    INTEGER,          INTENT(IN) :: lun          ! logical unit number
    REAL(sp),         INTENT(IN) :: missing      ! value for missing data
    ! OUTPUT:
    ! =======
    REAL(sp),         INTENT(OUT) :: data(:,:,:) ! qbo data (nyear,nmonth,nlev)
    
    INTEGER, parameter :: nmonth=12, nlev=15
    INTEGER            :: ierr          ! Error return
    INTEGER            :: i, iyear
    INTEGER            :: ik            ! sort of data ik=3 (zonal wind)
    INTEGER            :: imm, iyy      ! input month and year
    INTEGER            :: nzout         ! number of output levels
    INTEGER            :: station
    INTEGER            :: nm_skip
    INTEGER            :: idata(nlev)
    INTEGER            :: imissing=-999 ! value for missing input data
    CHARACTER(len=8),PARAMETER :: sdate_shea='19570101'
    CHARACTER(len=8),PARAMETER :: edate_shea='19961201'
    CHARACTER(len=8)   :: date
    CHARACTER(len=4)   :: yymm, syymm, eyymm
    CHARACTER(len=2)   :: yy
    CHARACTER(len=54)  :: header
    logical            :: ldata
      
    ! --> open the data file, with the QBO data:                        
    ! ------------------------------------------                        
  
    open (lun, file = ifile, iostat = ierr) 
    if (ierr /= 0) then 
       print *, 'NOT open data file ',ifile 
       call exit(1)
    end if
    
    read (sdate,"(2x,a4)") syymm
    read (edate,"(2x,a4)") eyymm

100 format( i7, i2, 2i3, 15i5 ) 
    
    nm_skip = monthcount(sdate//'01', sdate_shea) - 1
    PRINT *,':nm_skip:',nm_skip,':years:',INT(REAL(nm_skip,sp) / 12._sp)
    IF (nm_skip <= 0) THEN
       iyear = 0
       date  = sdate//'01'
    ELSE
       iyear = INT(REAL(nm_skip,sp) / 12._sp) + 1
       date  = cmonthbeside(sdate//'01', nm_skip)
    END IF
    data  = missing
    ldata = .false.
    
    DO
       idata = 0
       READ (lun, 100, end=110) station, ik, iyy, imm, &
            &                 ( idata(i), i = 1, nlev )
       !PRINT *,station, ik, iyy, imm, &
       !     &                 ( idata(i), i = 1, nlev )
       WRITE (yymm,"(i2.2,i2.2)") iyy, imm
       READ  (date,"(2x,a4,2x)") syymm
       if (yymm == syymm) ldata = .true.
       !PRINT *,':yymm:',yymm,':syymm:',syymm,':ldata:',ldata, &
       !        ':date:',date,':iyear:',iyear,':imm:',imm
       if (ldata) then
          !
          ! initialise iyear at the first data month
          !
          if (iyear == 0) iyear = 1
          print *,'iyear:',iyear,' imm:',imm,' yymm:',yymm,' idata(10)',idata(10)
          do i = 1, nlev 
             if ( idata(i) == 0 ) then
                data(i,imm,iyear) = missing
             else
                data(i,imm,iyear) = REAL(idata(i),sp)*0.1_sp
             end if
          end do
          ! 
          ! increment iyear at the of the year
          !
          if (imm == nmonth) iyear = iyear + 1
          date = cmonthbeside(date)
       end if
       if (yymm == eyymm) ldata = .false.
    END DO
110 close(lun) 
    RETURN
  END SUBROUTINE rshea
  ! -
END MODULE mod_read_qbo
