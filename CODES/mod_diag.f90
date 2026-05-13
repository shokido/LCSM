module mod_diag
  use run_param
  use run_types
  use calendar_sub
  use ncdf_write
  implicit none
  integer :: ntime_diag,idiag,idiag_count
  real(idx),allocatable :: time_diag(:)
  integer,allocatable :: istep_diag(:)  
contains
  subroutine create_diag(fname,grd,oset,&
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

    call writenet_dv(trim(fname),"x_u","y_u","mode","time",1,(/"u_rate"/),(/"m/s^2"/),missing_value)
    call writenet_dv(trim(fname),"x_u","y_u","mode","time",1,(/"u_drag"/),(/"m/s^2"/),missing_value)
    call writenet_dv(trim(fname),"x_u","y_u","mode","time",1,(/"u_cori"/),(/"m/s^2"/),missing_value)
    call writenet_dv(trim(fname),"x_u","y_u","mode","time",1,(/"u_prgf"/),(/"m/s^2"/),missing_value)
    call writenet_dv(trim(fname),"x_u","y_u","mode","time",1,(/"u_wind"/),(/"m/s^2"/),missing_value)
    call writenet_dv(trim(fname),"x_u","y_u","mode","time",1,(/"u_hdif"/),(/"m/s^2"/),missing_value)

    call writenet_dv(trim(fname),"x_v","y_v","mode","time",1,(/"v_rate"/),(/"m/s^2"/),missing_value)
    call writenet_dv(trim(fname),"x_v","y_v","mode","time",1,(/"v_drag"/),(/"m/s^2"/),missing_value)
    call writenet_dv(trim(fname),"x_v","y_v","mode","time",1,(/"v_cori"/),(/"m/s^2"/),missing_value)
    call writenet_dv(trim(fname),"x_v","y_v","mode","time",1,(/"v_prgf"/),(/"m/s^2"/),missing_value)
    call writenet_dv(trim(fname),"x_v","y_v","mode","time",1,(/"v_wind"/),(/"m/s^2"/),missing_value)
    call writenet_dv(trim(fname),"x_v","y_v","mode","time",1,(/"v_hdif"/),(/"m/s^2"/),missing_value)

    call writenet_dv(trim(fname),"x_p","y_p","mode","time",1,(/"p_rate"/),(/"m^2/s^3"/),missing_value)
    call writenet_dv(trim(fname),"x_p","y_p","mode","time",1,(/"p_drag"/),(/"m^2/s^3"/),missing_value)
    call writenet_dv(trim(fname),"x_p","y_p","mode","time",1,(/"p_dudx"/),(/"m^2/s^3"/),missing_value)
    call writenet_dv(trim(fname),"x_p","y_p","mode","time",1,(/"p_dvdy"/),(/"m^2/s^3"/),missing_value)
    ! Write coordinate variables
    call writenet_wv(trim(fname),"lon_p",(/1/),(/nx+2/),grd%lon_p%val(0:nx+1))
    call writenet_wv(trim(fname),"lat_p",(/1/),(/ny+2/),grd%lat_p%val(0:ny+1))
    call writenet_wv(trim(fname),"lon_u",(/1/),(/nx+1/),grd%lon_u%val(1:nx+1))
    call writenet_wv(trim(fname),"lat_u",(/1/),(/ny+2/),grd%lat_u%val(0:ny+1))
    call writenet_wv(trim(fname),"lon_v",(/1/),(/nx+2/),grd%lon_v%val(0:nx+1))
    call writenet_wv(trim(fname),"lat_v",(/1/),(/ny+1/),grd%lat_v%val(1:ny+1))
    call writenet_wv(trim(fname),"mode",(/1/),(/nm/),grd%modes%val(1:nm))
    call writenet_wv(trim(fname),"time",(/1/),(/nt/),time(1:nt))
  end subroutine create_diag
  subroutine allocate_ocn_diag(grd)
    implicit none
    type(ocn_dta),intent(inout) :: grd
    integer :: nx_p,ny_p,nm
    nx_p=grd%nx_p;ny_p=grd%ny_p;nm=grd%nm
    allocate(grd%u_rate%val(1:nm,1:nx_p+1,0:ny_p+1))
    allocate(grd%u_drag%val(1:nm,1:nx_p+1,0:ny_p+1))
    allocate(grd%u_cori%val(1:nm,1:nx_p+1,0:ny_p+1))
    allocate(grd%u_prgf%val(1:nm,1:nx_p+1,0:ny_p+1))
    allocate(grd%u_wind%val(1:nm,1:nx_p+1,0:ny_p+1))
    allocate(grd%u_hdif%val(1:nm,1:nx_p+1,0:ny_p+1))
    allocate(grd%v_rate%val(1:nm,0:nx_p+1,1:ny_p+1))
    allocate(grd%v_drag%val(1:nm,0:nx_p+1,1:ny_p+1))
    allocate(grd%v_cori%val(1:nm,0:nx_p+1,1:ny_p+1))
    allocate(grd%v_prgf%val(1:nm,0:nx_p+1,1:ny_p+1))
    allocate(grd%v_wind%val(1:nm,0:nx_p+1,1:ny_p+1))
    allocate(grd%v_hdif%val(1:nm,0:nx_p+1,1:ny_p+1))
    allocate(grd%p_rate%val(1:nm,0:nx_p+1,0:ny_p+1))
    allocate(grd%p_drag%val(1:nm,0:nx_p+1,0:ny_p+1))
    allocate(grd%p_dudx%val(1:nm,0:nx_p+1,0:ny_p+1))
    allocate(grd%p_dvdy%val(1:nm,0:nx_p+1,0:ny_p+1))
   end subroutine allocate_ocn_diag
  subroutine clean_ocn_diag(grd)
    implicit none
    type(ocn_dta),intent(inout) :: grd
    integer :: nx_p,ny_p,nm
    nx_p=grd%nx_p;ny_p=grd%ny_p;nm=grd%nm
    grd%u_rate%val(1:nm,1:nx_p+1,0:ny_p+1)=0.0_idx
    grd%u_drag%val(1:nm,1:nx_p+1,0:ny_p+1)=0.0_idx
    grd%u_cori%val(1:nm,1:nx_p+1,0:ny_p+1)=0.0_idx
    grd%u_prgf%val(1:nm,1:nx_p+1,0:ny_p+1)=0.0_idx
    grd%u_wind%val(1:nm,1:nx_p+1,0:ny_p+1)=0.0_idx
    grd%u_hdif%val(1:nm,1:nx_p+1,0:ny_p+1)=0.0_idx
    grd%v_rate%val(1:nm,0:nx_p+1,1:ny_p+1)=0.0_idx
    grd%v_drag%val(1:nm,0:nx_p+1,1:ny_p+1)=0.0_idx
    grd%v_cori%val(1:nm,0:nx_p+1,1:ny_p+1)=0.0_idx
    grd%v_prgf%val(1:nm,0:nx_p+1,1:ny_p+1)=0.0_idx
    grd%v_wind%val(1:nm,0:nx_p+1,1:ny_p+1)=0.0_idx
    grd%v_hdif%val(1:nm,0:nx_p+1,1:ny_p+1)=0.0_idx
    grd%p_rate%val(1:nm,0:nx_p+1,0:ny_p+1)=0.0_idx
    grd%p_drag%val(1:nm,0:nx_p+1,0:ny_p+1)=0.0_idx
    grd%p_dudx%val(1:nm,0:nx_p+1,0:ny_p+1)=0.0_idx
    grd%p_dvdy%val(1:nm,0:nx_p+1,0:ny_p+1)=0.0_idx
   end subroutine clean_ocn_diag
  subroutine initialize_ocn_diag(grd)
    implicit none
    type(ocn_dta),intent(inout) :: grd
    call allocate_ocn_diag(grd)
    call clean_ocn_diag(grd)
   end subroutine initialize_ocn_diag
  subroutine deallocate_ocn_diag(grd)
    implicit none
    type(ocn_dta),intent(inout) :: grd
    deallocate(grd%u_rate%val)
    deallocate(grd%u_drag%val)
    deallocate(grd%u_cori%val)
    deallocate(grd%u_prgf%val)
    deallocate(grd%u_wind%val)
    deallocate(grd%u_hdif%val)
    deallocate(grd%v_rate%val)
    deallocate(grd%v_drag%val)
    deallocate(grd%v_cori%val)
    deallocate(grd%v_prgf%val)
    deallocate(grd%v_wind%val)
    deallocate(grd%v_hdif%val)
    deallocate(grd%p_rate%val)
    deallocate(grd%p_drag%val)
    deallocate(grd%p_dudx%val)
    deallocate(grd%p_dvdy%val)
   end subroutine deallocate_ocn_diag
   subroutine oper_diag_ocn(ogrd)
    implicit none
    type(ocn_dta),intent(inout) :: ogrd
    ogrd%u_rate%val=ogrd%u_rate%val+ogrd%u_rate%val
    ogrd%u_drag%val=ogrd%u_drag%val+ogrd%u_drag%val
    ogrd%u_cori%val=ogrd%u_cori%val+ogrd%u_cori%val
    ogrd%u_prgf%val=ogrd%u_prgf%val+ogrd%u_prgf%val
    ogrd%u_wind%val=ogrd%u_wind%val+ogrd%u_wind%val
    ogrd%u_hdif%val=ogrd%u_hdif%val+ogrd%u_hdif%val
    ogrd%v_rate%val=ogrd%v_rate%val+ogrd%v_rate%val
    ogrd%v_drag%val=ogrd%v_drag%val+ogrd%v_drag%val
    ogrd%v_cori%val=ogrd%v_cori%val+ogrd%v_cori%val
    ogrd%v_prgf%val=ogrd%v_prgf%val+ogrd%v_prgf%val
    ogrd%v_wind%val=ogrd%v_wind%val+ogrd%v_wind%val
    ogrd%v_hdif%val=ogrd%v_hdif%val+ogrd%v_hdif%val
    ogrd%p_rate%val=ogrd%p_rate%val+ogrd%p_rate%val
    ogrd%p_drag%val=ogrd%p_drag%val+ogrd%p_drag%val
    ogrd%p_dudx%val=ogrd%p_dudx%val+ogrd%p_dudx%val
    ogrd%p_dvdy%val=ogrd%p_dvdy%val+ogrd%p_dvdy%val
   end subroutine oper_diag_ocn
   subroutine write_diag(fname,grd,irec,avg_count)
    use ncdf_write
    implicit none
    character(len=*),intent(in) :: fname
    type(ocn_dta),intent(inout) :: grd
    integer,intent(inout) :: irec,avg_count
    real(idx),allocatable :: u_tmp(:,:,:,:),v_tmp(:,:,:,:),p_tmp(:,:,:,:)
    integer :: im,ix,iy,nx,ny,nm
    nx=grd%nx_p;ny=grd%ny_p;nm=grd%nm
    allocate(u_tmp(1:nx+1,0:ny+1,1,1))
    allocate(v_tmp(0:nx+1,1:ny+1,1,1))
    allocate(p_tmp(0:nx+1,0:ny+1,1,1))
    do im = 1,nm
       do iy=0,ny+1
          do ix=1,nx+1
             if (grd%mask_u%val(ix,iy) .eq. 0.0_idx) then
                u_tmp(ix,iy,1,1)=missing_value
             else
                u_tmp(ix,iy,1,1)=grd%u_rate%val(im,ix,iy)/avg_count
             end if
          end do
       end do
       call writenet_wv(trim(fname),"u_rate",(/1,1,im,irec/),(/nx+1,ny+2,im,irec/),u_tmp(1:nx+1,0:ny+1,1:1,1:1))
       do iy=0,ny+1
          do ix=1,nx+1
             if (grd%mask_u%val(ix,iy) .eq. 0.0_idx) then
                u_tmp(ix,iy,1,1)=missing_value
             else
                u_tmp(ix,iy,1,1)=grd%u_drag%val(im,ix,iy)/avg_count
             end if
          end do
       end do
       call writenet_wv(trim(fname),"u_drag",(/1,1,im,irec/),(/nx+1,ny+2,im,irec/),u_tmp(1:nx+1,0:ny+1,1:1,1:1))
       do iy=0,ny+1
          do ix=1,nx+1
             if (grd%mask_u%val(ix,iy) .eq. 0.0_idx) then
                u_tmp(ix,iy,1,1)=missing_value
             else
                u_tmp(ix,iy,1,1)=grd%u_cori%val(im,ix,iy)/avg_count
             end if
          end do
       end do
       call writenet_wv(trim(fname),"u_cori",(/1,1,im,irec/),(/nx+1,ny+2,im,irec/),u_tmp(1:nx+1,0:ny+1,1:1,1:1))
       do iy=0,ny+1
          do ix=1,nx+1
             if (grd%mask_u%val(ix,iy) .eq. 0.0_idx) then
                u_tmp(ix,iy,1,1)=missing_value
             else
                u_tmp(ix,iy,1,1)=grd%u_prgf%val(im,ix,iy)/avg_count
             end if
          end do
       end do
       call writenet_wv(trim(fname),"u_prgf",(/1,1,im,irec/),(/nx+1,ny+2,im,irec/),u_tmp(1:nx+1,0:ny+1,1:1,1:1))
       do iy=0,ny+1
          do ix=1,nx+1
             if (grd%mask_u%val(ix,iy) .eq. 0.0_idx) then
                u_tmp(ix,iy,1,1)=missing_value
             else
                u_tmp(ix,iy,1,1)=grd%u_wind%val(im,ix,iy)/avg_count
             end if
          end do
       end do
       call writenet_wv(trim(fname),"u_wind",(/1,1,im,irec/),(/nx+1,ny+2,im,irec/),u_tmp(1:nx+1,0:ny+1,1:1,1:1))
       do iy=0,ny+1
          do ix=1,nx+1
             if (grd%mask_u%val(ix,iy) .eq. 0.0_idx) then
                u_tmp(ix,iy,1,1)=missing_value
             else
                u_tmp(ix,iy,1,1)=grd%u_hdif%val(im,ix,iy)/avg_count
             end if
          end do
       end do
       call writenet_wv(trim(fname),"u_hdif",(/1,1,im,irec/),(/nx+1,ny+2,im,irec/),u_tmp(1:nx+1,0:ny+1,1:1,1:1))
      ! V
       do iy=1,ny+1
          do ix=0,nx+1
             if (grd%mask_v%val(ix,iy) .eq. 0.0_idx) then
                v_tmp(ix,iy,1,1)=missing_value
             else
                v_tmp(ix,iy,1,1)=grd%v_rate%val(im,ix,iy)/avg_count
             end if
          end do
       end do
       call writenet_wv(trim(fname),"v_rate",(/1,1,im,irec/),(/nx+2,ny+1,im,irec/),v_tmp(0:nx+1,1:ny+1,1:1,1:1))
       do iy=1,ny+1
          do ix=0,nx+1
             if (grd%mask_v%val(ix,iy) .eq. 0.0_idx) then
                v_tmp(ix,iy,1,1)=missing_value
             else
                v_tmp(ix,iy,1,1)=grd%v_drag%val(im,ix,iy)/avg_count
             end if
          end do
       end do
       call writenet_wv(trim(fname),"v_drag",(/1,1,im,irec/),(/nx+2,ny+1,im,irec/),v_tmp(0:nx+1,1:ny+1,1:1,1:1))
       do iy=1,ny+1
          do ix=0,nx+1
             if (grd%mask_v%val(ix,iy) .eq. 0.0_idx) then
                v_tmp(ix,iy,1,1)=missing_value
             else
                v_tmp(ix,iy,1,1)=grd%v_cori%val(im,ix,iy)/avg_count
             end if
          end do
       end do
       call writenet_wv(trim(fname),"v_cori",(/1,1,im,irec/),(/nx+2,ny+1,im,irec/),v_tmp(0:nx+1,1:ny+1,1:1,1:1))
       do iy=1,ny+1
          do ix=0,nx+1
             if (grd%mask_v%val(ix,iy) .eq. 0.0_idx) then
                v_tmp(ix,iy,1,1)=missing_value
             else
                v_tmp(ix,iy,1,1)=grd%v_prgf%val(im,ix,iy)/avg_count
             end if
          end do
       end do
       call writenet_wv(trim(fname),"v_prgf",(/1,1,im,irec/),(/nx+2,ny+1,im,irec/),v_tmp(0:nx+1,1:ny+1,1:1,1:1))
       do iy=1,ny+1
          do ix=0,nx+1
             if (grd%mask_v%val(ix,iy) .eq. 0.0_idx) then
                v_tmp(ix,iy,1,1)=missing_value
             else
                v_tmp(ix,iy,1,1)=grd%v_wind%val(im,ix,iy)/avg_count
             end if
          end do
       end do
       call writenet_wv(trim(fname),"v_wind",(/1,1,im,irec/),(/nx+2,ny+1,im,irec/),v_tmp(0:nx+1,1:ny+1,1:1,1:1))
       do iy=1,ny+1
          do ix=0,nx+1
             if (grd%mask_v%val(ix,iy) .eq. 0.0_idx) then
                v_tmp(ix,iy,1,1)=missing_value
             else
                v_tmp(ix,iy,1,1)=grd%v_hdif%val(im,ix,iy)/avg_count
             end if
          end do
       end do
       call writenet_wv(trim(fname),"v_hdif",(/1,1,im,irec/),(/nx+2,ny+1,im,irec/),v_tmp(0:nx+1,1:ny+1,1:1,1:1))
       ! P
       do iy=0,ny+1
          do ix=0,nx+1
             if (grd%mask_p%val(ix,iy) .eq. 0.0_idx) then
                p_tmp(ix,iy,1,1)=missing_value
             else
                p_tmp(ix,iy,1,1)=grd%p_rate%val(im,ix,iy)/avg_count
             end if
          end do
       end do
       call writenet_wv(trim(fname),"p_rate",(/1,1,im,irec/),(/nx+2,ny+2,im,irec/),p_tmp(0:nx+1,0:ny+1,1:1,1:1))
       do iy=0,ny+1
          do ix=0,nx+1
             if (grd%mask_p%val(ix,iy) .eq. 0.0_idx) then
                p_tmp(ix,iy,1,1)=missing_value
             else
                p_tmp(ix,iy,1,1)=grd%p_drag%val(im,ix,iy)/avg_count
             end if
          end do
       end do
       call writenet_wv(trim(fname),"p_drag",(/1,1,im,irec/),(/nx+2,ny+2,im,irec/),p_tmp(0:nx+1,0:ny+1,1:1,1:1))
       do iy=0,ny+1
          do ix=0,nx+1
             if (grd%mask_p%val(ix,iy) .eq. 0.0_idx) then
                p_tmp(ix,iy,1,1)=missing_value
             else
                p_tmp(ix,iy,1,1)=grd%p_dudx%val(im,ix,iy)/avg_count
             end if
          end do
       end do
       call writenet_wv(trim(fname),"p_dudx",(/1,1,im,irec/),(/nx+2,ny+2,im,irec/),p_tmp(0:nx+1,0:ny+1,1:1,1:1))
       do iy=0,ny+1
          do ix=0,nx+1
             if (grd%mask_p%val(ix,iy) .eq. 0.0_idx) then
                p_tmp(ix,iy,1,1)=missing_value
             else
                p_tmp(ix,iy,1,1)=grd%p_dvdy%val(im,ix,iy)/avg_count
             end if
          end do
       end do
       call writenet_wv(trim(fname),"p_dvdy",(/1,1,im,irec/),(/nx+2,ny+2,im,irec/),p_tmp(0:nx+1,0:ny+1,1:1,1:1))
    end do
    deallocate(p_tmp);deallocate(u_tmp);deallocate(v_tmp)
   end subroutine write_diag

end module mod_diag
