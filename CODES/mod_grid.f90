module mod_grid
  use run_param
  use run_types
  use ncdf_read
  implicit none
contains
  !=============================================
  ! Create mask
  !=============================================
  subroutine read_ocn_grd(fname,grd)
    implicit none
    character(len=maxlen),intent(in) :: fname
    type(ocn_dta),intent(inout) :: grd
    integer :: ntmp
    real(idx),allocatable :: v_1d(:)
    real(idx),allocatable :: v_2d(:,:)
    call get_dimsize(fname,"x_p",ntmp)
    grd%nx_p=ntmp-2
    call get_dimsize(fname,"y_p",ntmp)
    grd%ny_p=ntmp-2
    allocate(grd%lon_p%val(0:grd%nx_p+1))
    call get_variable(fname,"lon_p",(/1/),(/grd%nx_p+2/),v_1d)
    grd%lon_p%val(0:grd%nx_p+1)=v_1d(1:grd%nx_p+2)
    allocate(grd%lon_u%val(1:grd%nx_p+1))
    call get_variable(fname,"lon_u",(/1/),(/grd%nx_p+1/),v_1d)
    grd%lon_u%val(1:grd%nx_p+1)=v_1d(1:grd%nx_p+1)
    allocate(grd%lon_v%val(0:grd%nx_p+1))
    call get_variable(fname,"lon_v",(/1/),(/grd%nx_p+2/),v_1d)
    grd%lon_v%val(0:grd%nx_p+1)=v_1d(1:grd%nx_p+2)
    allocate(grd%lat_p%val(0:grd%ny_p+1))
    call get_variable(fname,"lat_p",(/1/),(/grd%ny_p+2/),v_1d)
    grd%lat_p%val(0:grd%ny_p+1)=v_1d(1:grd%ny_p+2)
    allocate(grd%lat_u%val(0:grd%ny_p+1))
    call get_variable(fname,"lat_u",(/1/),(/grd%ny_p+2/),v_1d)
    grd%lat_u%val(0:grd%ny_p+1)=v_1d(1:grd%ny_p+2)
    allocate(grd%lat_v%val(0:grd%ny_p+1))
    call get_variable(fname,"lat_v",(/1/),(/grd%ny_p+1/),v_1d)
    grd%lat_v%val(1:grd%ny_p+1)=v_1d(1:grd%ny_p+1)
    call get_variable(fname,"f",(/1,1/),(/grd%nx_p+2,grd%ny_p+2/),v_2d)
    allocate(grd%f%val(0:grd%nx_p+1,0:grd%ny_p+1))
    grd%f%val(0:grd%nx_p+1,0:grd%ny_p+1)=v_2d(1:grd%nx_p+2,1:grd%ny_p+2)
    call get_variable(fname,"x_p_2d",(/1,1/),(/grd%nx_p+2,grd%ny_p+2/),v_2d)
    allocate(grd%x_p%val(0:grd%nx_p+1,0:grd%ny_p+1))
    grd%x_p%val(0:grd%nx_p+1,0:grd%ny_p+1)=v_2d(1:grd%nx_p+2,1:grd%ny_p+2)
    call get_variable(fname,"y_p_2d",(/1,1/),(/grd%nx_p+2,grd%ny_p+2/),v_2d)
    allocate(grd%y_p%val(0:grd%nx_p+1,0:grd%ny_p+1))
    grd%y_p%val(0:grd%nx_p+1,0:grd%ny_p+1)=v_2d(1:grd%nx_p+2,1:grd%ny_p+2)
    call get_variable(fname,"x_u_2d",(/1,1/),(/grd%nx_p+1,grd%ny_p+2/),v_2d)
    allocate(grd%x_u%val(1:grd%nx_p+1,0:grd%ny_p+1))
    grd%x_u%val(1:grd%nx_p+1,0:grd%ny_p+1)=v_2d(1:grd%nx_p+1,1:grd%ny_p+2)
    call get_variable(fname,"y_u_2d",(/1,1/),(/grd%nx_p+1,grd%ny_p+2/),v_2d)
    allocate(grd%y_u%val(1:grd%nx_p+1,0:grd%ny_p+1))
    grd%y_u%val(1:grd%nx_p+1,0:grd%ny_p+1)=v_2d(1:grd%nx_p+1,1:grd%ny_p+2)
    call get_variable(fname,"x_v_2d",(/1,1/),(/grd%nx_p+2,grd%ny_p+1/),v_2d)
    allocate(grd%x_v%val(0:grd%nx_p+1,1:grd%ny_p+1))
    grd%x_v%val(0:grd%nx_p+1,1:grd%ny_p+1)=v_2d(1:grd%nx_p+2,1:grd%ny_p+1)
    call get_variable(fname,"y_v_2d",(/1,1/),(/grd%nx_p+2,grd%ny_p+1/),v_2d)
    allocate(grd%y_v%val(0:grd%nx_p+1,1:grd%ny_p+1))
    grd%y_v%val(0:grd%nx_p+1,1:grd%ny_p+1)=v_2d(1:grd%nx_p+2,1:grd%ny_p+1)
    call get_variable(fname,"mask_p",(/1,1/),(/grd%nx_p+2,grd%ny_p+2/),v_2d)
    allocate(grd%mask_p%val(0:grd%nx_p+1,0:grd%ny_p+1))
    grd%mask_p%val(0:grd%nx_p+1,0:grd%ny_p+1)=v_2d(1:grd%nx_p+2,1:grd%ny_p+2)
    call get_variable(fname,"damp_u",(/1,1/),(/grd%nx_p+1,grd%ny_p+2/),v_2d)
    allocate(grd%damp_u%val(1:grd%nx_p+1,0:grd%ny_p+1))
    grd%damp_u%val(1:grd%nx_p+1,0:grd%ny_p+1)=v_2d(1:grd%nx_p+1,1:grd%ny_p+2)
    call get_variable(fname,"damp_v",(/1,1/),(/grd%nx_p+2,grd%ny_p+1/),v_2d)
    allocate(grd%damp_v%val(0:grd%nx_p+1,1:grd%ny_p+1))
    grd%damp_v%val(0:grd%nx_p+1,1:grd%ny_p+1)=v_2d(1:grd%nx_p+2,1:grd%ny_p+1)
    call get_variable(fname,"damp_p",(/1,1/),(/grd%nx_p+2,grd%ny_p+2/),v_2d)
    allocate(grd%damp_p%val(0:grd%nx_p+1,0:grd%ny_p+1))
    grd%damp_p%val(0:grd%nx_p+1,0:grd%ny_p+1)=v_2d(1:grd%nx_p+2,1:grd%ny_p+2)
  end subroutine read_ocn_grd
  subroutine set_mask(grd,slip)
    type(ocn_dta),intent(inout) :: grd
    real(idx),intent(in) :: slip
    integer :: iy,ix,nx,ny
    nx=grd%nx_p;ny=grd%ny_p
    allocate(grd%mask_u%val(1:nx+1,0:ny+1))
    allocate(grd%mask_v%val(0:nx+1,1:ny+1))
    allocate(grd%mask_phi_u%val(1:nx+1,1:ny+1))
    allocate(grd%mask_phi_v%val(1:nx+1,1:ny+1))
    grd%mask_u%val(:,:)=1.0_idx
    grd%mask_v%val(:,:)=1.0_idx
    grd%mask_phi_u%val(:,:)=1.0_idx
    grd%mask_phi_v%val(:,:)=1.0_idx
    iy = 0
    do ix = 1,nx+1
       grd%mask_u%val(ix,iy)=grd%mask_p%val(ix-1,iy)*grd%mask_p%val(ix,iy)
    end do

    do iy = 1,ny+1
       ix = 0
       grd%mask_v%val(ix,iy)=grd%mask_p%val(ix,iy-1)*grd%mask_p%val(ix,iy)       
       do ix = 1,nx+1
          grd%mask_u%val(ix,iy)=grd%mask_p%val(ix-1,iy)*grd%mask_p%val(ix,iy)
          grd%mask_v%val(ix,iy)=grd%mask_p%val(ix,iy-1)*grd%mask_p%val(ix,iy)
          if (grd%mask_p%val(ix-1,iy-1) == 0.0_idx .and. grd%mask_p%val(ix,iy-1)==0.0_idx) then
             grd%mask_phi_u%val(ix,iy) = slip
          end if
          if (grd%mask_p%val(ix-1,iy) == 0.0_idx .and. grd%mask_p%val(ix,iy)==0.0_idx) then
             grd%mask_phi_u%val(ix,iy) = slip
          end if
          if (grd%mask_p%val(ix-1,iy-1) == 0.0_idx .and. grd%mask_p%val(ix-1,iy) == 0.0_idx) then
             grd%mask_phi_v%val(ix,iy) = slip
          end if
          if (grd%mask_p%val(ix,iy-1) == 0.0_idx .and. grd%mask_p%val(ix,iy) == 0.0_idx) then
             grd%mask_phi_v%val(ix,iy) = slip
          end if
       end do
    end do
  end subroutine set_mask
  subroutine initialize_ocn_visc(grd,oset)
    implicit none
    type(ocn_dta),intent(inout) :: grd
    type(ocn_set),intent(in) :: oset
    integer :: nx_p,ny_p
    nx_p=grd%nx_p;ny_p=grd%ny_p
    allocate(grd%nu%val(0:nx_p+1,0:ny_p+1))
    grd%nu%val(0:nx_p+1,0:ny_p+1)=oset%nu_h
  end subroutine initialize_ocn_visc
  subroutine allocate_ocn_arrays(grd)
    implicit none
    type(ocn_dta),intent(inout) :: grd
    integer :: nx_p,ny_p,nm
    nx_p=grd%nx_p;ny_p=grd%ny_p;nm=grd%nm
    allocate(grd%p%val(1:nm,0:nx_p+1,0:ny_p+1))
    allocate(grd%u%val(1:nm,1:nx_p+1,0:ny_p+1))
    allocate(grd%v%val(1:nm,0:nx_p+1,1:ny_p+1))
    allocate(grd%p_past%val(1:nm,0:nx_p+1,0:ny_p+1))
    allocate(grd%u_past%val(1:nm,1:nx_p+1,0:ny_p+1))
    allocate(grd%v_past%val(1:nm,0:nx_p+1,1:ny_p+1))
    allocate(grd%p_next%val(1:nm,0:nx_p+1,0:ny_p+1))
    allocate(grd%u_next%val(1:nm,1:nx_p+1,0:ny_p+1))
    allocate(grd%v_next%val(1:nm,0:nx_p+1,1:ny_p+1))
    allocate(grd%tau_x%val(0:nx_p+1,0:ny_p+1))
    allocate(grd%tau_y%val(0:nx_p+1,0:ny_p+1))
    allocate(grd%cn%val(1:nm,0:nx_p+1,0:ny_p+1))
    allocate(grd%obn%val(1:nm,0:nx_p+1,0:ny_p+1))
  end subroutine allocate_ocn_arrays
  subroutine clean_ocn_arrays(grd)
    implicit none
    type(ocn_dta),intent(inout) :: grd
    integer :: nx_p,ny_p,nm
    nx_p=grd%nx_p;ny_p=grd%ny_p;nm=grd%nm
    grd%p%val(1:nm,0:nx_p+1,0:ny_p+1)=0.0_idx
    grd%u%val(1:nm,1:nx_p+1,0:ny_p+1)=0.0_idx
    grd%v%val(1:nm,0:nx_p+1,1:ny_p+1)=0.0_idx
    grd%p_past%val(1:nm,0:nx_p+1,0:ny_p+1)=0.0_idx
    grd%u_past%val(1:nm,1:nx_p+1,0:ny_p+1)=0.0_idx
    grd%v_past%val(1:nm,0:nx_p+1,1:ny_p+1)=0.0_idx
    grd%p_next%val(1:nm,0:nx_p+1,0:ny_p+1)=0.0_idx
    grd%u_next%val(1:nm,1:nx_p+1,0:ny_p+1)=0.0_idx
    grd%v_next%val(1:nm,0:nx_p+1,1:ny_p+1)=0.0_idx
    grd%tau_x%val(0:nx_p+1,0:ny_p+1)=0.0_idx
    grd%tau_y%val(0:nx_p+1,0:ny_p+1)=0.0_idx
  end subroutine clean_ocn_arrays
  subroutine deallocate_ocn_arrays(grd)
    implicit none
    type(ocn_dta),intent(inout) :: grd
    deallocate(grd%x_p%val);deallocate(grd%y_p%val)
    deallocate(grd%x_u%val);deallocate(grd%y_u%val)
    deallocate(grd%x_v%val);deallocate(grd%y_v%val)
    deallocate(grd%lon_p%val);deallocate(grd%lat_p%val)
    deallocate(grd%lon_u%val);deallocate(grd%lat_u%val)
    deallocate(grd%lon_v%val);deallocate(grd%lat_v%val)
    deallocate(grd%f%val)
    deallocate(grd%mask_p%val); deallocate(grd%mask_u%val); deallocate(grd%mask_v%val)
    deallocate(grd%mask_phi_u%val); deallocate(grd%mask_phi_v%val)
    deallocate(grd%damp_p%val); deallocate(grd%damp_u%val); deallocate(grd%damp_v%val)
    deallocate(grd%nu%val)
    deallocate(grd%p%val);deallocate(grd%u%val);deallocate(grd%v%val)
    deallocate(grd%p_past%val);deallocate(grd%u_past%val);deallocate(grd%v_past%val)
    deallocate(grd%p_next%val);deallocate(grd%u_next%val);deallocate(grd%v_next%val)
    deallocate(grd%tau_x%val)
    deallocate(grd%tau_y%val)
    deallocate(grd%cn%val)
    deallocate(grd%obn%val)
  end subroutine deallocate_ocn_arrays
end module mod_grid
