module mod_hist
  use run_param
  use calendar_sub
  use ncdf_write
  implicit none
  ! Time array
  integer :: ntime_hist,ihist
  real(idx),allocatable :: time_hist(:)
  integer,allocatable :: istep_hist(:)  
contains
  !==========================
  ! Creation of history file
  !==========================
  subroutine create_hist(fname,nx,ny,nm,x_p,y_p,x_u,y_u,x_v,y_v,&
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
    character(len=maxlen) :: dim_names(8),dim_types(8)
    ref_time=calendar_create_time_att(start_yymmdd,start_hhmmss,1)
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
       call calendar_cal_length_ymdhms(start_yymmdd,start_hhmmss,tmp_yymmdd,tmp_hhmmss,1,tmp)
       istep(it)=int(tmp/(dt*sec_to_day))
       time(it) = real(tmp)
    end do
    dim_names(1)="x_p"
    dim_names(2)="y_p"
    dim_names(3)="x_u"
    dim_names(4)="y_u"
    dim_names(5)="x_v"
    dim_names(6)="y_v"
    dim_names(7)="mode"
    dim_names(8)="time"
    do it=1,8
       dim_types(it)="double"
    end do
    call writenet_def_dim(trim(fname),8,(/nx+2,ny+2,&
         & nx+1,ny+2,nx+2,ny+1,nm,nt/),dim_names,dim_types)
    call add_var_att(trim(fname),"mode","units","")         
    call add_var_att(trim(fname),"time","units",ref_time)         
    call writenet_dv(trim(fname),"x_u",1,(/"lon_u"/),(/"degrees_east"/),missing_value)
    call writenet_dv(trim(fname),"y_u",1,(/"lat_u"/),(/"degrees_north"/),missing_value)
    call writenet_dv(trim(fname),"x_v",1,(/"lon_v"/),(/"degrees_east"/),missing_value)
    call writenet_dv(trim(fname),"y_v",1,(/"lat_v"/),(/"degrees_north"/),missing_value)
    call writenet_dv(trim(fname),"x_p",1,(/"lon_p"/),(/"degrees_east"/),missing_value)
    call writenet_dv(trim(fname),"y_p",1,(/"lat_p"/),(/"degrees_north"/),missing_value)
    call writenet_dv(trim(fname),"x_u","y_u","mode","time",1,(/"u"/),(/"m/s"/),missing_value)
    call writenet_dv(trim(fname),"x_v","y_v","mode","time",1,(/"v"/),(/"m/s"/),missing_value)
    call writenet_dv(trim(fname),"x_p","y_p","mode","time",1,(/"p"/),(/"m^2/s^2"/),missing_value)
    call writenet_dv(trim(fname),"x_p","y_p","time",2,(/"uw","vw"/),(/"N/m^2","N/m^2"/),missing_value)
    ! Write coordinate variables
    call writenet_wv(trim(fname),"x_p",(/1/),(/nx+2/),x_p(0:nx+1))
    call writenet_wv(trim(fname),"y_p",(/1/),(/ny+2/),y_p(0:ny+1))
    call writenet_wv(trim(fname),"x_u",(/1/),(/nx+1/),x_u(1:nx+1))
    call writenet_wv(trim(fname),"y_u",(/1/),(/ny+2/),y_u(0:ny+1))
    call writenet_wv(trim(fname),"x_v",(/1/),(/nx+2/),x_v(0:nx+1))
    call writenet_wv(trim(fname),"y_v",(/1/),(/ny+1/),y_v(1:ny+1))
    call writenet_wv(trim(fname),"lon_p",(/1/),(/nx+2/),lon_p(0:nx+1))
    call writenet_wv(trim(fname),"lat_p",(/1/),(/ny+2/),lat_p(0:ny+1))
    call writenet_wv(trim(fname),"lon_u",(/1/),(/nx+1/),lon_u(1:nx+1))
    call writenet_wv(trim(fname),"lat_u",(/1/),(/ny+2/),lat_u(0:ny+1))
    call writenet_wv(trim(fname),"lon_v",(/1/),(/nx+2/),lon_v(0:nx+1))
    call writenet_wv(trim(fname),"lat_v",(/1/),(/ny+1/),lat_v(1:ny+1))
    call writenet_wv(trim(fname),"mode",(/1/),(/nm/),modes(1:nm))
    call writenet_wv(trim(fname),"time",(/1/),(/nt/),time(1:nt))
  end subroutine create_hist
  subroutine write_hist(fname,nx,ny,nm,irec,u,v,p,mask_u,mask_v,mask_p,missing_value)
    use ncdf_write
    implicit none
    character(len=*),intent(in) :: fname
    integer,intent(inout) :: nx,ny,nm,irec
    real(idx),intent(in) :: u(1:nm,1:nx+1,0:ny+1),v(1:nm,0:nx+1,1:nx+1),p(1:nm,0:nx+1,0:ny+1)
    real(idx),intent(in) :: mask_u(1:nx+1,0:ny+1),mask_v(0:nx+1,1:nx+1),mask_p(0:nx+1,0:ny+1)
    real(idx),intent(in) :: missing_value
    real(idx) :: u_2d(1:nx+1,0:ny+1,1,1),v_2d(0:nx+1,1:nx+1,1,1),p_2d(0:nx+1,0:ny+1,1,1)
    integer :: im,ix,iy
    do im = 1,nm
       do iy=0,ny+1
          do ix=0,nx+1
             if (mask_p(ix,iy) .eq. 0.0_idx) then
                p_2d(ix,iy,1,1)=missing_value
             else
                p_2d(ix,iy,1,1)=p(im,ix,iy)
             end if
          end do
       end do
       do iy=0,ny+1
          do ix=1,nx+1
             if (mask_u(ix,iy) .eq. 0.0_idx) then
                u_2d(ix,iy,1,1)=missing_value
             else
                u_2d(ix,iy,1,1)=u(im,ix,iy)
             end if
          end do
       end do
       do iy=1,ny+1
          do ix=0,nx+1
             if (mask_v(ix,iy) .eq. 0.0_idx) then
                v_2d(ix,iy,1,1)=missing_value
             else
                v_2d(ix,iy,1,1)=v(im,ix,iy)
             end if
          end do
       end do
       call writenet_wv(trim(fname),"u",(/1,1,im,irec/),(/nx+1,ny+2,im,irec/),u_2d(1:nx+1,0:ny+1,1:1,1:1))
       call writenet_wv(trim(fname),"v",(/1,1,im,irec/),(/nx+2,ny+1,im,irec/),v_2d(0:nx+1,1:ny+1,1:1,1:1))
       call writenet_wv(trim(fname),"p",(/1,1,im,irec/),(/nx+2,ny+2,im,irec/),p_2d(0:nx+1,0:ny+1,1:1,1:1))
    end do
  end subroutine write_hist
  subroutine write_hist_2d(fname,nx,ny,irec,uw,vw,mask_p,missing_value)
    use ncdf_write
    implicit none
    character(len=*),intent(in) :: fname
    integer,intent(inout) :: nx,ny,irec
    real(idx),intent(inout) :: uw(0:nx+1,0:ny+1),vw(0:nx+1,0:nx+1)
    real(idx),intent(in) :: mask_p(0:nx+1,0:ny+1)
    real(idx),intent(in) :: missing_value
    real(idx) :: uw_2d(0:nx+1,0:ny+1,1),vw_2d(0:nx+1,0:ny+1,1)
    integer :: ix,iy
    do iy=0,ny+1
       do ix=0,nx+1
          if (mask_p(ix,iy) .eq. 0.0_idx) then
             uw_2d(ix,iy,1)=missing_value
             vw_2d(ix,iy,1)=missing_value
          else
             uw_2d(ix,iy,1)=uw(ix,iy)
             vw_2d(ix,iy,1)=vw(ix,iy)
          end if
       end do
    end do
    call writenet_wv(trim(fname),"uw",(/1,1,irec/),(/nx+2,ny+2,irec/),uw_2d(0:nx+1,0:ny+1,1:1))
    call writenet_wv(trim(fname),"vw",(/1,1,irec/),(/nx+2,ny+2,irec/),vw_2d(0:nx+1,0:ny+1,1:1))
  end subroutine write_hist_2d
end module mod_hist
