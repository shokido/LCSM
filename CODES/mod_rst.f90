module mod_rst
  use ncdf_read
  use run_param
  use calendar_sub
  use ncdf_write
  implicit none
  ! Restart==============================================================
  ! Input
contains
  !===============================
  ! Read restart file
  !===============================
  subroutine read_rst_data(fname_rst,nx,ny,nm,u,v,p,u_past,v_past,p_past,&
       & u_next,v_next,p_next)
    implicit none
    character(len=*),intent(in) :: fname_rst
    integer,intent(in) :: nx,ny,nm
    real(idx),intent(inout),allocatable :: u(:,:,:),v(:,:,:),p(:,:,:)
    real(idx),intent(inout),allocatable :: u_past(:,:,:),v_past(:,:,:),p_past(:,:,:)
    real(idx),intent(inout),allocatable :: u_next(:,:,:),v_next(:,:,:),p_next(:,:,:)
!    real(idx),allocatable :: u_tmp(:,:,:),v_tmp(:,:,:),p_tmp(:,:,:)
    real(idx),allocatable :: v_3d(:,:,:)
    integer :: im,iy,ix
    character :: ichar*2,vname*10
    allocate(u(1:nm,1:nx+1,0:ny+1)); allocate(v(1:nm,0:nx+1,1:nx+1))
    allocate(p(1:nm,0:nx+1,0:ny+1))
    allocate(u_past(1:nm,1:nx+1,0:ny+1)); allocate(v_past(1:nm,0:nx+1,1:nx+1))
    allocate(p_past(1:nm,0:nx+1,0:ny+1))
    allocate(u_next(1:nm,1:nx+1,0:ny+1)); allocate(v_next(1:nm,0:nx+1,1:nx+1))
    allocate(p_next(1:nm,0:nx+1,0:ny+1))
   !  allocate(u_tmp(1:nx+1,0:ny+1,1))
   !  allocate(v_tmp(0:nx+1,1:ny+1,1))
   !  allocate(p_tmp(0:nx+1,0:ny+1,1))
    
    do im = 1,nm
       vname="u"
       call get_variable(trim(fname_rst),"u",(/1,1,im/),(/nx+1,ny+2,im/),v_3d)
       u(im,1:nx+1,0:ny+1)=v_3d(1:nx+1,1:ny+2,1)
       call get_variable(trim(fname_rst),"u_past",(/1,1,im/),(/nx+1,ny+2,im/),v_3d)
       u_past(im,1:nx+1,0:ny+1)=v_3d(1:nx+1,1:ny+2,1)
       call get_variable(trim(fname_rst),"v",(/1,1,im/),(/nx+2,ny+1,im/),v_3d)
       v(im,0:nx+1,1:ny+1)=v_3d(1:nx+2,1:ny+1,1)
       call get_variable(trim(fname_rst),"v_past",(/1,1,im/),(/nx+2,ny+1,im/),v_3d)
       v_past(im,0:nx+1,1:ny+1)=v_3d(1:nx+2,1:ny+1,1)
       call get_variable(trim(fname_rst),"p",(/1,1,im/),(/nx+2,ny+2,im/),v_3d)
       p(im,0:nx+1,0:ny+1)=v_3d(1:nx+2,1:ny+2,1)
       call get_variable(trim(fname_rst),"p_past",(/1,1,im/),(/nx+2,ny+2,im/),v_3d)
       p_past(im,0:nx+1,0:ny+1)=v_3d(1:nx+2,1:ny+2,1)
    end do
    deallocate(v_3d)
  end subroutine read_rst_data

  !==========================
  !Creation of restart file
  !==========================
  subroutine create_rst(fname_rst,nx,ny,nm,x_p,y_p,x_u,y_u,x_v,y_v,&
       & lon_p,lat_p,lon_u,lat_u,lon_v,lat_v,&
       & start_yymmdd,start_hhmmss,time,&
       & u,v,p,u_past,v_past,p_past,missing_value)
    use ncdf_write
    implicit none
    character(len=*),intent(in) :: fname_rst
    integer,intent(in) :: nx,ny,nm
    real(idx),intent(in) :: x_p(0:nx+1),y_p(0:ny+1),x_u(1:nx+1),y_u(0:ny+1),x_v(0:nx+1),y_v(1:ny+1)
    real(idx),intent(in) :: lon_p(0:nx+1),lat_p(0:ny+1),lon_u(1:nx+1),lat_u(0:ny+1),lon_v(0:nx+1),lat_v(1:ny+1)
    real(idx),intent(in) :: time
    integer,intent(in) :: start_yymmdd,start_hhmmss
    real(idx),intent(inout) :: u(1:nm,1:nx+1,0:ny+1),v(1:nm,0:nx+1,1:nx+1),p(1:nm,0:nx+1,0:ny+1)
    real(idx),intent(inout) :: u_past(1:nm,1:nx+1,0:ny+1),v_past(1:nm,0:nx+1,1:nx+1),p_past(1:nm,0:nx+1,0:ny+1)
    real(idx) :: tmp_array_u(1:nx+1,0:ny+1,1),tmp_array_v(0:nx+1,1:nx+1,1),tmp_array_p(0:nx+1,0:ny+1,1)
    real(idx),intent(in) :: missing_value
    integer :: im,ix,iy,it
    character(len=maxlen) :: ref_time
    real(idx) :: modes(nm)
    character(len=maxlen) :: dim_names(7),dim_types(7)
    do im = 1,nm
       modes(im) = im
    end do
    ref_time=calendar_create_time_att(start_yymmdd,start_hhmmss,-10000)
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
    call writenet_wv(trim(fname_rst),"x_p",(/1/),(/nx+2/),x_p(0:nx+1))
    call writenet_wv(trim(fname_rst),"y_p",(/1/),(/ny+2/),y_p(0:ny+1))
    call writenet_wv(trim(fname_rst),"x_u",(/1/),(/nx+1/),x_u(1:nx+1))
    call writenet_wv(trim(fname_rst),"y_u",(/1/),(/ny+2/),y_u(0:ny+1))
    call writenet_wv(trim(fname_rst),"x_v",(/1/),(/nx+2/),x_v(0:nx+1))
    call writenet_wv(trim(fname_rst),"y_v",(/1/),(/ny+1/),y_v(1:ny+1))
    call writenet_wv(trim(fname_rst),"lon_p",(/1/),(/nx+2/),lon_p(0:nx+1))
    call writenet_wv(trim(fname_rst),"lat_p",(/1/),(/ny+2/),lat_p(0:ny+1))
    call writenet_wv(trim(fname_rst),"lon_u",(/1/),(/nx+1/),lon_u(1:nx+1))
    call writenet_wv(trim(fname_rst),"lat_u",(/1/),(/ny+2/),lat_u(0:ny+1))
    call writenet_wv(trim(fname_rst),"lon_v",(/1/),(/nx+2/),lon_v(0:nx+1))
    call writenet_wv(trim(fname_rst),"lat_v",(/1/),(/ny+1/),lat_v(1:ny+1))
    call writenet_dv(trim(fname_rst),"x_u","y_u","mode",1,(/"u"/),(/"m/s"/),missing_value)
    call writenet_dv(trim(fname_rst),"x_u","y_u","mode",1,(/"u_past"/),(/"m/s"/),missing_value)
    call writenet_dv(trim(fname_rst),"x_v","y_v","mode",1,(/"v"/),(/"m/s"/),missing_value)
    call writenet_dv(trim(fname_rst),"x_v","y_v","mode",1,(/"v_past"/),(/"m/s"/),missing_value)
    call writenet_dv(trim(fname_rst),"x_p","y_p","mode",1,(/"p"/),(/"m^2/s^2"/),missing_value)
    call writenet_dv(trim(fname_rst),"x_p","y_p","mode",1,(/"p_past"/),(/"m^2/s^2"/),missing_value)
    do im = 1,nm
       do iy = 0,ny+1
          do ix = 1,nx+1
             tmp_array_u(ix,iy,1:1)=u(im:im,ix,iy)
          end do
       end do
       call writenet_wv(trim(fname_rst),"u",(/1,1,im/),(/nx+1,ny+2,im/),tmp_array_u)
       do iy = 0,ny+1
          do ix = 1,nx+1
             tmp_array_u(ix,iy,1:1)=u_past(im:im,ix,iy)
          end do
       end do
       call writenet_wv(trim(fname_rst),"u_past",(/1,1,im/),(/nx+1,ny+2,im/),tmp_array_u)
       do iy = 1,ny+1
          do ix = 0,nx+1
             tmp_array_v(ix,iy,1:1)=v(im:im,ix,iy)
          end do
       end do
       call writenet_wv(trim(fname_rst),"v",(/1,1,im/),(/nx+2,ny+1,im/),tmp_array_v)
       do iy = 1,ny+1
          do ix = 0,nx+1
             tmp_array_v(ix,iy,1:1)=v_past(im:im,ix,iy)
          end do
       end do
       call writenet_wv(trim(fname_rst),"v_past",(/1,1,im/),(/nx+2,ny+1,im/),tmp_array_v)
       do iy = 0,ny+1
          do ix = 0,nx+1
             tmp_array_p(ix,iy,1:1)=p(im:im,ix,iy)
          end do
       end do
       call writenet_wv(trim(fname_rst),"p",(/1,1,im/),(/nx+2,ny+2,im/),tmp_array_p)
       do iy = 0,ny+1
          do ix = 0,nx+1
             tmp_array_p(ix,iy,1:1)=p_past(im:im,ix,iy)
          end do
       end do
       call writenet_wv(trim(fname_rst),"p_past",(/1,1,im/),(/nx+2,ny+2,im/),tmp_array_p)
    end do
  end subroutine create_rst
end module mod_rst
