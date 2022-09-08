MODULE irt_tools
USE irt_parameters, ONLY: domainsize_x, domainsize_y,time_steps
IMPLICIT NONE

CONTAINS
SUBROUTINE read_single_dat(fname,itime,outdata)
CHARACTER(len=90), INTENT(in)  :: fname
INTEGER          , INTENT(in)  :: itime
REAL             , INTENT(out) :: outdata(domainsize_x,domainsize_y)
INTEGER                        :: lrec

INQUIRE(IOLENGTH=lrec) outdata
OPEN(10, FILE=fname, FORM='unformatted', ACCESS='direct',RECL=lrec)
READ(10,rec=itime) outdata
CLOSE(10)
END SUBROUTINE read_single_dat




SUBROUTINE write_dat(file_id,itime,field)
REAL, INTENT(IN)      :: field(domainsize_x,domainsize_y)
INTEGER, INTENT(IN)   :: file_id, itime
LOGICAL               :: itsopen

!inquire(unit=file_id, opened=itsopen) 
!print*, itsopen
WRITE (file_id,rec=itime) field
END SUBROUTINE write_dat



END MODULE irt_tools
