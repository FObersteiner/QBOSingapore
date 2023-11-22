!======================================================================
! MODULE: MESSAGES_MOD
! ======================
!
! Autor: Markus Kunze 11.04.2002
!
! $Id: messages_mod.f90,v 1.4 2008/10/22 06:06:09 cvs Exp $
! ======================================================================
MODULE MESSAGES_MOD

CONTAINS

  SUBROUTINE ERR(string)
    ! ==================================
    ! SUBROUTINE ERR
    !  - write an error message and exit
    ! INPUT:
    ! ------
    ! string   - error message to write
    ! ==================================
    CHARACTER(LEN=*), INTENT(IN)   :: string

    write(*,*)'---> ERROR:'//trim(string)
    call abort
  END SUBROUTINE ERR
  SUBROUTINE NOTOPEN(file, unit_number)

    ! Schreiben einer Fehlermeldung, 
    ! wenn eine Datei nicht geöffnet werden konnte:
    !----------------------------------------------

    character(len=*)      :: file
    integer, intent(in)   :: unit_number

    write(*,*)'---> ERROR while trying to open ' &
         &    //trim(file)//' as unit:',unit_number
    call abort
    RETURN
  END SUBROUTINE NOTOPEN
  SUBROUTINE NOTCLOSED (number, file)

    CHARACTER (LEN=*), INTENT(IN)   :: file
    INTEGER,           INTENT(IN)   :: number

    ! Schreiben einer Fehlermeldung, 
    ! wenn eine Datei nicht geöffnet werden konnte:
    !----------------------------------------------

    write(*,*)'---> ERROR while trying to close '//trim(file)//' unit:',number
    call abort
    return
  END SUBROUTINE NOTCLOSED
  SUBROUTINE MESSAGE ( file, string, number, io)
    
    CHARACTER (LEN=*),           INTENT(IN) :: file, string
    INTEGER,           OPTIONAL, INTENT(IN) :: number       ! LUN
    INTEGER,           OPTIONAL, INTENT(IN) :: io           ! error status
    
    integer             :: status
    character(len=7)    :: err_string
    character(len=256)  :: mess
    
    err_string = ''
    status = 1
    if (present (io)) status = io
    if (status /= 0) err_string = 'ERROR: '
    
    if (present (number)) then
       write(mess,"(a,i2,a)") '---> '//trim(err_string)//trim(file)// &
            &                 ' '//trim(string)//', unit:',number
    else
       write(mess,"(a)") '---> '//trim(err_string)//trim(file)// &
            &                 ' '//trim(string)
    end if
    print *,trim(mess)
    call abort
    return
  END SUBROUTINE MESSAGE

END MODULE MESSAGES_MOD
