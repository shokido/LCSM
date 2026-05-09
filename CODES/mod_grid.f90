module mod_grid
  use run_param
  use input_files
  use ncdf_read
  implicit none
  private
  public :: set_mask
  public :: read_ocn_grid
contains
  !=============================================
  ! Create mask
  !=============================================
  subroutine set_mask(nx,ny,mask_p,mask_u,mask_v,mask_phi_u,mask_phi_v,slip)
    implicit none
    integer,intent(in) :: nx,ny
    real(idx),intent(in) :: mask_p(0:nx+1,0:ny+1)
    real(idx),intent(inout) :: mask_u(1:nx+1,0:ny+1),mask_v(0:nx+1,1:ny+1)
    real(idx),intent(inout) :: mask_phi_u(1:nx+1,1:ny+1), mask_phi_v(1:nx+1,1:ny+1)
    real(idx),intent(in) :: slip
    integer :: ix,iy
    ! slip_0 (dun/dx=0),
    ! slip=2 (un=0-> dun/dx = (un - (-un))/dx)
    mask_u(:,:)=1.0_idx
    mask_v(:,:)=1.0_idx
    mask_phi_u(:,:)=1.0_idx
    mask_phi_v(:,:)=1.0_idx
    iy = 0
    do ix = 1,nx+1
       mask_u(ix,iy)=mask_p(ix-1,iy)*mask_p(ix,iy)
    end do

    do iy = 1,ny+1
       ix = 0
       mask_v(ix,iy)=mask_p(ix,iy-1)*mask_p(ix,iy)       
       do ix = 1,nx+1
          mask_u(ix,iy)=mask_p(ix-1,iy)*mask_p(ix,iy)
          mask_v(ix,iy)=mask_p(ix,iy-1)*mask_p(ix,iy)
          if (mask_p(ix-1,iy-1) == 0.0_idx .and. mask_p(ix,iy-1)==0.0_idx) then
             mask_phi_u(ix,iy) = slip
          end if
          if (mask_p(ix-1,iy) == 0.0_idx .and. mask_p(ix,iy)==0.0_idx) then
             mask_phi_u(ix,iy) = slip
          end if
          if (mask_p(ix-1,iy-1) == 0.0_idx .and. mask_p(ix-1,iy) == 0.0_idx) then
             mask_phi_v(ix,iy) = slip
          end if
          if (mask_p(ix,iy-1) == 0.0_idx .and. mask_p(ix,iy) == 0.0_idx) then
             mask_phi_v(ix,iy) = slip
          end if
       end do
    end do
  end subroutine set_mask
  subroutine read_ocn_grid(fname,nx,ny,x_p,y_p,x_u,y_u,x_v,y_v,&
       & lon_p,lat_p,lon_u,lat_u,lon_v,lat_v,f,mask_p,&
       & damp_p,damp_u,damp_v)
    implicit none
    character(len=*),intent(in) :: fname
    integer,intent(inout) :: nx,ny
    real(idx),allocatable,intent(inout) :: x_p(:),y_p(:),x_u(:),y_u(:),x_v(:),y_v(:)
    real(idx),allocatable,intent(inout) :: lon_p(:),lat_p(:),lon_u(:),lat_u(:),lon_v(:),lat_v(:)
    real(idx),allocatable,intent(inout) :: f(:)
    real(idx),allocatable,intent(inout) :: mask_p(:,:)
    real(idx),allocatable,intent(inout) :: damp_p(:,:),damp_u(:,:),damp_v(:,:)
    integer ::ntmp
    ! Get grid
    ! Grid generation------------------------------------------------
    ! p(0:nx+1,0:ny+1)
    call get_dimsize(fname,"x_p",ntmp)
    nx=ntmp-2
    call get_dimsize(fname,"y_p",ntmp)
    ny=ntmp-2
    allocate(x_p(0:nx+1)) ; allocate(y_p(0:ny+1))
    allocate(x_u(1:nx+1)) ; allocate(y_u(0:ny+1))
    allocate(x_v(0:nx+1)) ; allocate(y_v(1:ny+1))
    allocate(lon_p(0:nx+1)) ;  allocate(lat_p(0:ny+1))
    allocate(lon_u(1:nx+1)) ; allocate(lat_u(0:ny+1))
    allocate(lon_v(0:nx+1)) ; allocate(lat_v(1:ny+1))
    allocate(f(0:ny+1))
    allocate(mask_p(0:nx+1,0:ny+1))
    allocate(damp_p(0:nx+1,0:ny+1))
    allocate(damp_u(1:nx+1,0:ny+1))
    allocate(damp_v(0:nx+1,1:ny+1))
    call get_var_1D(fname,"x_p",x_p(0:nx+1))
    call get_var_1D(fname,"y_p",y_p(0:ny+1))
    call get_var_1D(fname,"x_u",x_u(1:nx+1))
    call get_var_1D(fname,"y_u",y_u(0:ny+1))
    call get_var_1D(fname,"x_v",x_v(0:nx+1))
    call get_var_1D(fname,"y_v",y_v(1:ny+1))
    call get_var_1D(fname,"lon_p",lon_p(0:nx+1))
    call get_var_1D(fname,"lat_p",lat_p(0:ny+1))
    call get_var_1D(fname,"lon_u",lon_u(1:nx+1))
    call get_var_1D(fname,"lat_u",lat_u(0:ny+1))
    call get_var_1D(fname,"lon_v",lon_v(0:nx+1))
    call get_var_1D(fname,"lat_v",lat_v(1:ny+1))
    call get_var_1D(fname,"f",f(0:ny+1))
    call get_var_2D(fname,"mask_p",mask_p(0:nx+1,0:ny+1))
    call get_var_2D(fname,"damp_p",damp_p(0:nx+1,0:ny+1))
    call get_var_2D(fname,"damp_u",damp_u(1:nx+1,0:ny+1))
    call get_var_2D(fname,"damp_v",damp_v(0:nx+1,1:ny+1))
  end subroutine read_ocn_grid
end module mod_grid
