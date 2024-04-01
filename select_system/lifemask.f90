PROGRAM lifemask
USE irt_parameters, ONLY: lperiodic_x, lperiodic_y, miss, n_fields
USE new_parameter, ONLY : domainsize_x, domainsize_y, &
                          lat_first, lon_first, time_steps, &
                          lat_inc, lon_inc
IMPLICIT NONE

!read tracklinks output
CHARACTER(len=300) :: fname_tracklinks
!INTEGER            :: tracks_header(10), cinit_timestep, cduration
INTEGER            :: tracks_header(9), cinit_timestep, cduration
INTEGER            :: ctrackid, ctimestep, cID1, cID2, cage1, cage2,&
                      cx1, cx2, cy1, cy2,&
                      clfid2, cslfid2, clbid2, cslbid2
REAL               :: carea, cfield(3*(n_fields+1)), cxm, cym, cvx, cvy
INTEGER            :: ii, ix, iy, iz

! read trackmask map
CHARACTER(len=300) :: fname_trackmask, fname_trackmask_out
REAL               :: trackmask(domainsize_x,domainsize_y)
REAL               :: trackmaskout(domainsize_x,domainsize_y)
REAL               :: dummyreal1, dummyreal2
INTEGER            :: dummyint1, dummyint2
INTEGER            :: reclraw
INTEGER            :: it

! target trackID
INTEGER            :: trackID2duration(5000000)

!fname_tracklinks = 'irt_tracklinks_output.txt'
fname_tracklinks = 'irt_tracklinks_output_select.txt'
OPEN(10, FILE=trim(fname_tracklinks), FORM='formatted', ACTION='read')

trackID2duration=0
DO
  read(10,*,end=999) ! newline
  read(10,*) tracks_header(:)
  ctrackid=tracks_header(1)
  cinit_timestep=tracks_header(3)  ! for select version
  cduration = tracks_header(4)     ! for select version
  DO ii=1, cduration
    read(10,*) dummyint1, ctimestep, cID1, cID2, cage1, cage2,&
               carea, cfield(:),&
               cx1, cx2, cy1, cy2, cxm, cym,&
               cvx, cvy, clfid2, cslfid2, clbid2, cslbid2
  ENDDO !cduration
  trackID2duration(ctrackid) = cduration
ENDDO
999 continue
close(10)


CALL write_grid_info(domainsize_x, domainsize_y, time_steps,&
                     lon_first, lat_first,&
                     lon_inc, lat_inc)

INQUIRE (IOLENGTH=reclraw) trackmask
!fname_trackmask = 'irt_tracks_mask.dat'
fname_trackmask = 'irt_tracks_mask_select.dat'
fname_trackmask_out = 'lifemask.dat'
OPEN(10,FILE=trim(fname_trackmask),FORM='unformatted',ACTION='read',access='direct',recl=reclraw)
OPEN(20,FILE=trim(fname_trackmask_out),FORM='unformatted',ACTION='write',access='direct',recl=reclraw)
DO it=1,time_steps
  READ(10,rec=it) trackmask(:,:)
  trackmaskout(:,:)=0.
  DO ix=1,domainsize_x
    DO iy=1,domainsize_y
      if(trackmask(ix,iy).le.0) trackmaskout(ix,iy)=trackmask(ix,iy)
      if(trackmask(ix,iy).gt.0)then
        trackmaskout(ix,iy) = trackID2duration(int(trackmask(ix,iy)))
      endif
    ENDDO !iy
  ENDDO !ix
  WRITE(20,rec=it) trackmaskout(:,:)
ENDDO !it
CLOSE(10)
CLOSE(20)



ENDPROGRAM lifemask

SUBROUTINE write_grid_info(nx, ny, nt, lon0, lat0, dlon, dlat)
INTEGER, INTENT(IN) :: nx, ny, nt
REAL, INTENT(IN)    :: lon0, lat0, dlon, dlat

OPEN(40, FILE='lifemask.ctl',&
         FORM='formatted', ACTION='write')
WRITE(40, *) 'dset ^lifemask.dat'
WRITE(40, *) 'undef -999000000.000000'
write(40, *) 'xdef ',nx,'linear',lon0,dlon
write(40, *) 'ydef ',ny,'linear',lat0,dlat
WRITE(40, *) 'zdef 1 levels 1000'
WRITE(40, *) 'tdef ',nt,'linear OOXXTIMEXXOO 1hr'
WRITE(40, *) 'vars 1'
WRITE(40, *) 'life 0 99 track_id'
WRITE(40, *) 'endvars'
CLOSE(40)
ENDSUBROUTINE write_grid_info
