module mod_avg
  use run_param
  use calendar_sub
  implicit none
  ! Time array
  integer :: ntime_avg,iavg
  real(idx),allocatable :: time_avg(:)
  integer,allocatable :: istep_avg(:)  
  integer :: out_avg_flag,out_avg_int
  character(maxlen) :: fname_out_avg
  ! Shallow water
  real(idx),allocatable :: u_avg(:,:,:),v_avg(:,:,:),p_avg(:,:,:)
  ! Namelist
  namelist/output_avg/out_avg_flag,out_avg_int
  namelist/output_avg/fname_out_avg
contains
  !==========================!
  ! Creation of average file !
  !==========================!
  subroutine create_avg(fname,nx,ny,nm,x_p,y_p,x_u,y_u,x_v,y_v,&
       & lon_p,lat_p,lon_u,lat_u,lon_v,lat_v,&
       & start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss,&
       & out_flag,out_int,missing_value,istep,nt)
    use ncdf_write
    implicit none
    character(len=*),intent(in) :: fname
    integer,intent(inout) :: nx,ny,nm
    real(idx),intent(in) :: x_p(0:nx+1),y_p(0:ny+1),x_u(1:nx+1),y_u(0:ny+1),x_v(0:nx+1),y_v(1:ny+1)
    real(idx),intent(in) :: lon_p(0:nx+1),lat_p(0:ny+1),lon_u(1:nx+1),lat_u(0:ny+1),lon_v(0:nx+1),lat_v(1:ny+1)
    integer,intent(in) :: start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss,out_flag,out_int
    real(idx),intent(in) :: missing_value
    integer,allocatable,intent(inout) :: istep(:)
    integer,intent(inout) :: nt
    real(idx),allocatable :: time(:)
    integer :: im,it
    real(idx) :: modes(nm),tmp
    character(len=maxlen) :: ref_time
    integer :: tmp_yymmdd,tmp_hhmmss
        
    ref_time=calendar_create_time_att(start_yymmdd,start_hhmmss,out_flag)
    do im = 1,nm
       modes(im) = im
    end do
    call calendar_cal_length_ymdhms(start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss,out_flag,tmp)
    nt= int(tmp / out_int)
    nt=max(nt,1)
    allocate(time(nt)); allocate(istep(nt))
    do it=1,nt
       time(it) = (real(it))* out_int
       call calendar_cal_ymdhms_after(start_yymmdd,start_hhmmss,time(it),out_flag,tmp_yymmdd,tmp_hhmmss)
       call calendar_cal_length_ymdhms(start_yymmdd,start_hhmmss,tmp_yymmdd,tmp_hhmmss,-10000,tmp)
       istep(it)=int(tmp/dt)
    end do
    call writenet_pre(fname,nx+2,ny+2,nm,nt,"x_p","y_p","mode","time","m","m","",trim(ref_time),&
         & x_p(0:nx+1),y_p(0:ny+1),modes(1:nm),time(1:nt))
    call writenet_dd(trim(fname),nx+1,"x_u")
    call writenet_dd(trim(fname),ny+2,"y_u")
    call writenet_dd(trim(fname),nx+2,"x_v")
    call writenet_dd(trim(fname),ny+1,"y_v")

    call writenet_dv(trim(fname),"x_u",2,(/"x_u","lon_u"/),(/"m","degrees_east"/),missing_value)
    call writenet_dv(trim(fname),"y_u",2,(/"y_u","lat_u"/),(/"m","degrees_north"/),missing_value)
    call writenet_dv(trim(fname),"x_v",2,(/"x_v","lon_v"/),(/"m","degrees_east"/),missing_value)
    call writenet_dv(trim(fname),"y_v",2,(/"y_v","lat_v"/),(/"m","degrees_north"/),missing_value)
    call writenet_dv(trim(fname),"x_p",1,(/"lon_p"/),(/"degrees_east"/),missing_value)
    call writenet_dv(trim(fname),"y_p",1,(/"lat_p"/),(/"degrees_north"/),missing_value)

    call writenet_dv(trim(fname),"x_u","y_u","mode","time",1,(/"u"/),(/"m/s"/),missing_value)
    call writenet_dv(trim(fname),"x_v","y_v","mode","time",1,(/"v"/),(/"m/s"/),missing_value)
    call writenet_dv(trim(fname),"x_p","y_p","mode","time",1,(/"p"/),(/"m^2/s^2"/),missing_value)

    ! Write coordinate variables
    call writenet_wv(trim(fname),"x_p",1,nx+2,x_p(0:nx+1))
    call writenet_wv(trim(fname),"y_p",1,ny+2,y_p(0:ny+1))
    call writenet_wv(trim(fname),"x_u",1,nx+1,x_u(1:nx+1))
    call writenet_wv(trim(fname),"y_u",1,ny+2,y_u(0:ny+1))
    call writenet_wv(trim(fname),"x_v",1,nx+2,x_v(0:nx+1))
    call writenet_wv(trim(fname),"y_v",1,ny+1,y_v(1:ny+1))
    call writenet_wv(trim(fname),"lon_p",1,nx+2,lon_p(0:nx+1))
    call writenet_wv(trim(fname),"lat_p",1,ny+2,lat_p(0:ny+1))
    call writenet_wv(trim(fname),"lon_u",1,nx+1,lon_u(1:nx+1))
    call writenet_wv(trim(fname),"lat_u",1,ny+2,lat_u(0:ny+1))
    call writenet_wv(trim(fname),"lon_v",1,nx+2,lon_v(0:nx+1))
    call writenet_wv(trim(fname),"lat_v",1,ny+1,lat_v(1:ny+1))
  end subroutine create_avg
  subroutine write_avg(fname,nx,ny,nm,irec,u,v,p,mask_u,mask_v,mask_p,missing_value,avg_count)
    use ncdf_write
    implicit none
    character(len=*),intent(in) :: fname
    integer,intent(inout) :: nx,ny,nm,irec
    real(idx),intent(inout) :: u(1:nm,1:nx+1,0:ny+1),v(1:nm,0:nx+1,1:nx+1),p(1:nm,0:nx+1,0:ny+1)
    real(idx),intent(in) :: mask_u(1:nx+1,0:ny+1),mask_v(0:nx+1,1:nx+1),mask_p(0:nx+1,0:ny+1)
    real(idx),intent(in) :: missing_value
    integer,intent(in) :: avg_count
    real(idx) :: u_2d(1:nx+1,0:ny+1,1,1),v_2d(0:nx+1,1:nx+1,1,1),p_2d(0:nx+1,0:ny+1,1,1)
    integer :: im,ix,iy
    do im = 1,nm
       do iy=0,ny+1
          do ix=0,nx+1
             if (mask_p(ix,iy) .eq. 0.0_idx) then
                p_2d(ix,iy,1,1)=missing_value
             else
                p_2d(ix,iy,1,1)=p(im,ix,iy)/avg_count
             end if
             p(im,ix,iy)=0.0_idx
          end do
       end do
       do iy=0,ny+1
          do ix=1,nx+1
             if (mask_u(ix,iy) .eq. 0.0_idx) then
                u_2d(ix,iy,1,1)=missing_value
             else
                u_2d(ix,iy,1,1)=u(im,ix,iy)/avg_count
             end if
             u(im,ix,iy)=0.0_idx
          end do
       end do
       do iy=1,ny+1
          do ix=0,nx+1
             if (mask_v(ix,iy) .eq. 0.0_idx) then
                v_2d(ix,iy,1,1)=missing_value
             else
                v_2d(ix,iy,1,1)=v(im,ix,iy)/avg_count
             end if
             v(im,ix,iy)=1.0_idx
          end do
       end do
       call writenet_wv(trim(fname),"u",1,nx+1,1,ny+2,im,im,irec,irec,u_2d(1:nx+1,0:ny+1,1:1,1:1))
       call writenet_wv(trim(fname),"v",1,nx+2,1,ny+1,im,im,irec,irec,v_2d(0:nx+1,1:ny+1,1:1,1:1))
       call writenet_wv(trim(fname),"p",1,nx+2,1,ny+2,im,im,irec,irec,p_2d(0:nx+1,0:ny+1,1:1,1:1))
    end do
  end subroutine write_avg

end module mod_avg
