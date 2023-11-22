MODULE READ_QBO_DATA

  IMPLICIT NONE
  
CONTAINS
  
  SUBROUTINE READ_HIGHRES(missing, file, isyear, qbo)
    ! INPUT:
    ! ======
    REAL,             INTENT(IN) :: missing
    CHARACTER(LEN=*), INTENT(IN) :: file
    INTEGER,          INTENT(IN) :: isyear

    ! OUTPUT:
    ! =======
    REAL, DIMENSION(:,:,:) :: qbo

    integer, parameter :: nlev=14, nmonth=12
    integer            :: ierr, yyyy, mm, month, i, iyear
    character(len=80)  :: header

    open (79, file = file, iostat = ierr)
    if (ierr /= 0) then
       print *, 'NOT open data file ',file
       stop
    end if
    
    !     ignore header data until startdate:
    !     -----------------------------------
    do i = 1, 8
       read(79,'(a80)') header
       print *,header
    end do
    
!100 format( i4.4,1x,i2.2,6x,14(f8.4) )

    iyear = 1
    years:do
       do month = 1, nmonth
          read(79, *, end = 11) yyyy, mm, qbo(1:nlev,month,iyear+(yyyy-isyear))
          print *,yyyy, mm, iyear+(yyyy-isyear)
       end do
       read(79,'(a)', end = 11) header
    end do years
11  close(79)
    
    RETURN
  END SUBROUTINE READ_HIGHRES
    SUBROUTINE READ_100(missing, file, isyear, qbo)
    ! INPUT:
    ! ======
    REAL,             INTENT(IN) :: missing
    CHARACTER(LEN=*), INTENT(IN) :: file
    INTEGER,          INTENT(IN) :: isyear
    ! OUTPUT:
    ! =======
    REAL, DIMENSION(:,:)   :: qbo

    integer, parameter :: nlev=1, nmonth=12
    integer            :: ierr, yyyy, mm, month, i, iyear
    real               :: direction
    character(len=80)  :: header

    open (80, file = file,  &
         &  status = 'OLD', &
         &  iostat = ierr)
    if (ierr /= 0) then
       print *, 'NOT open data file ',file
       stop
    end if
    
    !     ignore header data until startdate:
    !     -----------------------------------
    do i = 1, 21
       read(80,'(a80)') header
    end do
    
!100 format( i4.4,1x,i2.2,6x,14(f8.4) )

    iyear = 1
    years:do
       do month = 1, nmonth
          read(80, *, end = 11) yyyy, mm,  &
               qbo(month,iyear+(yyyy-isyear)), direction
       end do
       read(80,'(a)', end = 11) header
    end do years
11  close(80)
    
    RETURN
  END SUBROUTINE READ_100


END MODULE READ_QBO_DATA
