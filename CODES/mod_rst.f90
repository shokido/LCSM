module mod_rst
  use ncdf_read
  use run_param
  use run_types
  use calendar_sub
  use ncdf_write
  implicit none
  ! Restart==============================================================
  ! Input
contains
  !===============================
  ! Read restart file
  !===============================
  subroutine read_rst_data(fname_rst,grd)
    implicit none
    character(len=*),intent(in) :: fname_rst
    type(ocn_dta),intent(inout) :: grd
    real(idx),allocatable :: v_3d(:,:,:)
    integer :: im,nx,ny,nm
    nx=grd%nx_p;ny=grd%ny_p;nm=grd%nm
    do im = 1,nm
       call get_variable(trim(fname_rst),"u",(/1,1,im/),(/nx+1,ny+2,im/),v_3d)
       grd%u%val(im,1:nx+1,0:ny+1)=v_3d(1:nx+1,1:ny+2,1)
       call get_variable(trim(fname_rst),"u_past",(/1,1,im/),(/nx+1,ny+2,im/),v_3d)
       grd%u_past%val(im,1:nx+1,0:ny+1)=v_3d(1:nx+1,1:ny+2,1)
       call get_variable(trim(fname_rst),"v",(/1,1,im/),(/nx+2,ny+1,im/),v_3d)
       grd%v%val(im,0:nx+1,1:ny+1)=v_3d(1:nx+2,1:ny+1,1)
       call get_variable(trim(fname_rst),"v_past",(/1,1,im/),(/nx+2,ny+1,im/),v_3d)
       grd%v_past%val(im,0:nx+1,1:ny+1)=v_3d(1:nx+2,1:ny+1,1)
       call get_variable(trim(fname_rst),"p",(/1,1,im/),(/nx+2,ny+2,im/),v_3d)
       grd%p%val(im,0:nx+1,0:ny+1)=v_3d(1:nx+2,1:ny+2,1)
       call get_variable(trim(fname_rst),"p_past",(/1,1,im/),(/nx+2,ny+2,im/),v_3d)
       grd%p_past%val(im,0:nx+1,0:ny+1)=v_3d(1:nx+2,1:ny+2,1)
    end do
    deallocate(v_3d)
  end subroutine read_rst_data
  !==========================
  !Creation of restart file
  !==========================
  subroutine create_rst(fname_rst,grd,missing_value)
    use ncdf_write
    implicit none
    character(len=*),intent(in) :: fname_rst
    type(ocn_dta),intent(inout) :: grd
    real(idx),intent(in) :: missing_value
    integer :: nx,ny,nm
    real(idx),allocatable :: tmp_3d(:,:,:)
    integer :: im,ix,iy,it
    character(len=maxlen) :: dim_names(7),dim_types(7)
    nx=grd%nx_p;ny=grd%ny_p;nm=grd%nm
    dim_names(1)="x_p"
    dim_names(2)="y_p"
    dim_names(3)="x_u"
    dim_names(4)="y_u"
    dim_names(5)="x_v"
    dim_names(6)="y_v"
    dim_names(7)="mode"
    do it=1,7
       dim_types(it)="double"
    end do    
    call writenet_def_dim(trim(fname_rst),7,(/nx+2,ny+2,&
         & nx+1,ny+2,nx+2,ny+1,nm/),dim_names,dim_types)
    call writenet_dv(trim(fname_rst),"x_u",1,(/"lon_u"/),(/"degrees_east"/),missing_value)
    call writenet_dv(trim(fname_rst),"y_u",1,(/"lat_u"/),(/"degrees_north"/),missing_value)
    call writenet_dv(trim(fname_rst),"x_v",1,(/"lon_v"/),(/"degrees_east"/),missing_value)
    call writenet_dv(trim(fname_rst),"y_v",1,(/"lat_v"/),(/"degrees_north"/),missing_value)
    call writenet_dv(trim(fname_rst),"x_p",1,(/"lon_p"/),(/"degrees_east"/),missing_value)
    call writenet_dv(trim(fname_rst),"y_p",1,(/"lat_p"/),(/"degrees_north"/),missing_value)
    ! Write coordinate variables
    call writenet_wv(trim(fname_rst),"mode",(/1/),(/nm/),grd%modes%val(1:nm))
    call writenet_wv(trim(fname_rst),"lon_p",(/1/),(/nx+2/),grd%lon_p%val(0:nx+1))
    call writenet_wv(trim(fname_rst),"lat_p",(/1/),(/ny+2/),grd%lat_p%val(0:ny+1))
    call writenet_wv(trim(fname_rst),"lon_u",(/1/),(/nx+1/),grd%lon_u%val(1:nx+1))
    call writenet_wv(trim(fname_rst),"lat_u",(/1/),(/ny+2/),grd%lat_u%val(0:ny+1))
    call writenet_wv(trim(fname_rst),"lon_v",(/1/),(/nx+2/),grd%lon_v%val(0:nx+1))
    call writenet_wv(trim(fname_rst),"lat_v",(/1/),(/ny+1/),grd%lat_v%val(1:ny+1))
    call writenet_dv(trim(fname_rst),"x_u","y_u","mode",1,(/"u"/),(/"m/s"/),missing_value)
    call writenet_dv(trim(fname_rst),"x_u","y_u","mode",1,(/"u_past"/),(/"m/s"/),missing_value)
    call writenet_dv(trim(fname_rst),"x_v","y_v","mode",1,(/"v"/),(/"m/s"/),missing_value)
    call writenet_dv(trim(fname_rst),"x_v","y_v","mode",1,(/"v_past"/),(/"m/s"/),missing_value)
    call writenet_dv(trim(fname_rst),"x_p","y_p","mode",1,(/"p"/),(/"m^2/s^2"/),missing_value)
    call writenet_dv(trim(fname_rst),"x_p","y_p","mode",1,(/"p_past"/),(/"m^2/s^2"/),missing_value)
    do im = 1,nm
       allocate(tmp_3d(1:nx+1,1:ny+2,1:1))
       do iy = 0,ny+1
          do ix = 1,nx+1
             tmp_3d(ix,iy+1,1:1)=grd%u%val(im:im,ix,iy)
          end do
       end do
       call writenet_wv(trim(fname_rst),"u",(/1,1,im/),(/nx+1,ny+2,im/),tmp_3d)
       do iy = 0,ny+1
          do ix = 1,nx+1
             tmp_3d(ix,iy+1,1:1)=grd%u_past%val(im:im,ix,iy)
          end do
       end do
       call writenet_wv(trim(fname_rst),"u_past",(/1,1,im/),(/nx+1,ny+2,im/),tmp_3d)
       deallocate(tmp_3d)
       allocate(tmp_3d(1:nx+2,1:ny+1,1:1))
       do iy = 1,ny+1
          do ix = 0,nx+1
             tmp_3d(ix+1,iy,1:1)=grd%v%val(im:im,ix,iy)
          end do
       end do
       call writenet_wv(trim(fname_rst),"v",(/1,1,im/),(/nx+2,ny+1,im/),tmp_3d)
       do iy = 1,ny+1
          do ix = 0,nx+1
             tmp_3d(ix+1,iy,1:1)=grd%v_past%val(im:im,ix,iy)
          end do
       end do
       call writenet_wv(trim(fname_rst),"v_past",(/1,1,im/),(/nx+2,ny+1,im/),tmp_3d)
       deallocate(tmp_3d)
       allocate(tmp_3d(1:nx+2,1:ny+2,1:1))
       do iy = 0,ny+1
          do ix = 0,nx+1
             tmp_3d(ix+1,iy+1,1:1)=grd%p%val(im:im,ix,iy)
          end do
       end do
       call writenet_wv(trim(fname_rst),"p",(/1,1,im/),(/nx+2,ny+2,im/),tmp_3d)
       do iy = 0,ny+1
          do ix = 0,nx+1
             tmp_3d(ix+1,iy+1,1:1)=grd%p_past%val(im:im,ix,iy)
          end do
       end do
       call writenet_wv(trim(fname_rst),"p_past",(/1,1,im/),(/nx+2,ny+2,im/),tmp_3d)
       deallocate(tmp_3d)
    end do
  end subroutine create_rst
end module mod_rst
