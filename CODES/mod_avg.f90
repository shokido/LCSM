module mod_avg
  use run_param
  use run_types
  use calendar_sub
  use ncdf_write
  implicit none
  ! Time array
  integer :: ntime_avg,iavg
  real(idx),allocatable :: time_avg(:)
  integer,allocatable :: istep_avg(:)  
contains
  !==========================!
  ! Creation of average file !
  !==========================!
  subroutine create_avg(fname,grd,oset,&
       & start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss,&
       & out_flag,out_int,missing_value,istep,nt)
    use ncdf_write
    implicit none
    character(len=*),intent(in) :: fname
    type(ocn_dta),intent(in) :: grd
    type(ocn_set),intent(in) :: oset
    integer,intent(in) :: start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss,out_flag,out_int
    real(idx),intent(in) :: missing_value
    integer,allocatable,intent(inout) :: istep(:)
    integer,intent(inout) :: nt
    real(idx),allocatable :: time(:)
    integer :: nx,ny,nm,im,it
    real(idx) :: tmp
    character(len=maxlen) :: ref_time
    integer :: tmp_yymmdd,tmp_hhmmss
    character(len=maxlen) :: dim_names(8),dim_types(8)
    nx=grd%nx_p;ny=grd%ny_p;nm=grd%nm
    ref_time=calendar_create_time_att(start_yymmdd,start_hhmmss,1)
    call calendar_cal_length_ymdhms(start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss,out_flag,tmp)
    nt= int(tmp / out_int)
    nt=max(nt,1)
    if (allocated(istep) .eqv. .true.) then
       deallocate(istep)
    end if    
    if (allocated(time) .eqv. .true.) then
       deallocate(time)
    end if    
    allocate(time(nt)); allocate(istep(nt))
    do it=1,nt
       time(it) = (real(it))* out_int       
       call calendar_cal_ymdhms_after(start_yymmdd,start_hhmmss,time(it),out_flag,tmp_yymmdd,tmp_hhmmss)
       call calendar_cal_length_ymdhms(start_yymmdd,start_hhmmss,tmp_yymmdd,tmp_hhmmss,1,tmp)
       istep(it)=int(tmp/(dt*sec_to_day))
       time(it) = (real(it-0.5))* out_int
       call calendar_cal_ymdhms_after(start_yymmdd,start_hhmmss,time(it),out_flag,tmp_yymmdd,tmp_hhmmss)
       call calendar_cal_length_ymdhms(start_yymmdd,start_hhmmss,tmp_yymmdd,tmp_hhmmss,1,tmp)
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
    call writenet_dv(trim(fname),"x_p","y_p","time",1,(/"uw"/),(/"N/m^2"/),missing_value)
    call writenet_dv(trim(fname),"x_p","y_p","time",1,(/"vw"/),(/"N/m^2"/),missing_value)
    ! Write coordinate variables
    call writenet_wv(trim(fname),"lon_p",(/1/),(/nx+2/),grd%lon_p%val(0:nx+1))
    call writenet_wv(trim(fname),"lat_p",(/1/),(/ny+2/),grd%lat_p%val(0:ny+1))
    call writenet_wv(trim(fname),"lon_u",(/1/),(/nx+1/),grd%lon_u%val(1:nx+1))
    call writenet_wv(trim(fname),"lat_u",(/1/),(/ny+2/),grd%lat_u%val(0:ny+1))
    call writenet_wv(trim(fname),"lon_v",(/1/),(/nx+2/),grd%lon_v%val(0:nx+1))
    call writenet_wv(trim(fname),"lat_v",(/1/),(/ny+1/),grd%lat_v%val(1:ny+1))
    call writenet_wv(trim(fname),"mode",(/1/),(/nm/),grd%modes%val(1:nm))
    call writenet_wv(trim(fname),"time",(/1/),(/nt/),time(1:nt))
  end subroutine create_avg
  subroutine allocate_ocn_avg(grd)
    implicit none
    type(ocn_dta),intent(inout) :: grd
    integer :: nx_p,ny_p,nm
    nx_p=grd%nx_p;ny_p=grd%ny_p;nm=grd%nm
    allocate(grd%p_avg%val(1:nm,0:nx_p+1,0:ny_p+1))
    allocate(grd%u_avg%val(1:nm,1:nx_p+1,0:ny_p+1))
    allocate(grd%v_avg%val(1:nm,0:nx_p+1,1:ny_p+1))
    allocate(grd%tau_x_avg%val(0:nx_p+1,0:ny_p+1))
    allocate(grd%tau_y_avg%val(0:nx_p+1,0:ny_p+1))
   end subroutine allocate_ocn_avg
  subroutine clean_ocn_avg(grd)
    implicit none
    type(ocn_dta),intent(inout) :: grd
    integer :: nx_p,ny_p,nm
    nx_p=grd%nx_p;ny_p=grd%ny_p;nm=grd%nm
    grd%p_avg%val(1:nm,0:nx_p+1,0:ny_p+1)=0.0_idx
    grd%u_avg%val(1:nm,1:nx_p+1,0:ny_p+1)=0.0_idx
    grd%v_avg%val(1:nm,0:nx_p+1,1:ny_p+1)=0.0_idx
    grd%tau_x_avg%val(0:nx_p+1,0:ny_p+1)=0.0_idx
    grd%tau_y_avg%val(0:nx_p+1,0:ny_p+1)=0.0_idx
   end subroutine clean_ocn_avg
  subroutine initialize_ocn_avg(grd)
    implicit none
    type(ocn_dta),intent(inout) :: grd
    call allocate_ocn_avg(grd)
    call clean_ocn_avg(grd)
   end subroutine initialize_ocn_avg
   subroutine deallocate_ocn_avg(grd)
    implicit none
    type(ocn_dta),intent(inout) :: grd
    deallocate(grd%p_avg%val)
    deallocate(grd%u_avg%val)
    deallocate(grd%v_avg%val)
    deallocate(grd%tau_x_avg%val)
    deallocate(grd%tau_y_avg%val)
   end subroutine deallocate_ocn_avg
   subroutine oper_avg_ocn(ogrd)
    implicit none
    type(ocn_dta),intent(inout) :: ogrd
    ! U-sw
    ogrd%u_avg%val=ogrd%u_avg%val+ogrd%u%val
    ogrd%v_avg%val=ogrd%v_avg%val+ogrd%v%val
    ogrd%p_avg%val=ogrd%p_avg%val+ogrd%p%val
    ogrd%tau_x_avg%val=ogrd%tau_x_avg%val+ogrd%tau_x%val
    ogrd%tau_y_avg%val=ogrd%tau_y_avg%val+ogrd%tau_y%val
    end subroutine oper_avg_ocn
   subroutine write_avg(fname,grd,irec,avg_count)
    use ncdf_write
    implicit none
    character(len=*),intent(in) :: fname
    type(ocn_dta),intent(inout) :: grd
    integer,intent(inout) :: irec,avg_count
    real(idx),allocatable :: u_tmp(:,:,:,:),v_tmp(:,:,:,:),p_tmp(:,:,:,:)
    real(idx),allocatable :: uw_tmp(:,:,:),vw_tmp(:,:,:)
    integer :: im,ix,iy,nx,ny,nm
    nx=grd%nx_p;ny=grd%ny_p;nm=grd%nm
    allocate(u_tmp(1:nx+1,0:ny+1,1,1))
    allocate(v_tmp(0:nx+1,1:ny+1,1,1))
    allocate(p_tmp(0:nx+1,0:ny+1,1,1))
    allocate(uw_tmp(0:nx+1,0:ny+1,1))
    allocate(vw_tmp(0:nx+1,0:ny+1,1))
    do im = 1,nm
       do iy=0,ny+1
          do ix=1,nx+1
             if (grd%mask_u%val(ix,iy) .eq. 0.0_idx) then
                u_tmp(ix,iy,1,1)=missing_value
             else
                u_tmp(ix,iy,1,1)=grd%u_avg%val(im,ix,iy)/avg_count
             end if
          end do
       end do
       call writenet_wv(trim(fname),"u",(/1,1,im,irec/),(/nx+1,ny+2,im,irec/),u_tmp(1:nx+1,0:ny+1,1:1,1:1))
       do iy=1,ny+1
          do ix=0,nx+1
             if (grd%mask_v%val(ix,iy) .eq. 0.0_idx) then
                v_tmp(ix,iy,1,1)=missing_value
             else
                v_tmp(ix,iy,1,1)=grd%v_avg%val(im,ix,iy)/avg_count
             end if
          end do
       end do
       call writenet_wv(trim(fname),"v",(/1,1,im,irec/),(/nx+2,ny+1,im,irec/),v_tmp(0:nx+1,1:ny+1,1:1,1:1))
       do iy=0,ny+1
          do ix=0,nx+1
             if (grd%mask_p%val(ix,iy) .eq. 0.0_idx) then
                p_tmp(ix,iy,1,1)=missing_value
             else
                p_tmp(ix,iy,1,1)=grd%p_avg%val(im,ix,iy)/avg_count
             end if
          end do
       end do
       call writenet_wv(trim(fname),"p",(/1,1,im,irec/),(/nx+2,ny+2,im,irec/),p_tmp(0:nx+1,0:ny+1,1:1,1:1))
      end do
    do iy=0,ny+1
       do ix=0,nx+1
          if (grd%mask_p%val(ix,iy) .eq. 0.0_idx) then
             uw_tmp(ix,iy,1)=missing_value
             vw_tmp(ix,iy,1)=missing_value
          else
             uw_tmp(ix,iy,1)=grd%tau_x_avg%val(ix,iy)/avg_count
             vw_tmp(ix,iy,1)=grd%tau_y_avg%val(ix,iy)/avg_count
          end if
       end do
    end do
    deallocate(p_tmp);deallocate(u_tmp);deallocate(v_tmp)
    deallocate(uw_tmp);deallocate(vw_tmp)
   end subroutine write_avg
end module mod_avg
