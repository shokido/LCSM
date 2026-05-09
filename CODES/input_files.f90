module input_files
  use run_param
  use ncdf_read
  implicit none
  private
  public :: get_var_units,get_dimsize_r
  public :: get_var_1D,get_var_2D,get_var_3D,get_var_4D
contains
  ! Check_R subroutine--------------------------------------------------------------
  subroutine check_net(status)
    use netcdf
    implicit none
    integer, intent ( in) :: status
    if(status /= nf90_noerr) then
       print *, trim(nf90_strerror(status))
       stop "Stopped"
    end if
  end subroutine check_net
  !------------------------------------------------------
  function get_dimsize_r(fname_grid,dimname) result(ndim)
    use netcdf
    implicit none
    character(len=*),intent(in) :: fname_grid,dimname
    integer :: ncid,dimid,ndim
    call check_net(nf90_open(trim(fname_grid),nf90_nowrite,ncid))
    call check_net(nf90_inq_dimid(ncid,dimname, dimid) )
    call check_net(nf90_inquire_dimension(ncid, dimid, len=ndim))
    call check_net(nf90_close(ncid))
  end function get_dimsize_r
  !-----------------------------------------------------
  subroutine get_var_1D(fname_grid,dimname,dims)
    use netcdf
    implicit none
    character(len=*),intent(in) :: fname_grid,dimname
    real(idx),intent(inout) :: dims(:)
    integer :: ncid,varid
    call check_net(nf90_open(trim(fname_grid),nf90_nowrite,ncid))
    call check_net(nf90_inq_varid(ncid,dimname,varid))
    call check_net( nf90_get_var(ncid,varid, dims))
    call check_net(nf90_close(ncid))
  end subroutine get_var_1D
  !-----------------------------------------------------
  subroutine get_var_2D(fname_grid,dimname,dims)
    use netcdf
    implicit none
    character(len=*),intent(in) :: fname_grid,dimname
    real(idx),intent(inout) :: dims(:,:)
    integer :: ncid,varid
    call check_net(nf90_open(trim(fname_grid),nf90_nowrite,ncid))
    call check_net(nf90_inq_varid(ncid,trim(dimname),varid))
    call check_net(nf90_get_var(ncid,varid, dims))
    call check_net(nf90_close(ncid))
  end subroutine get_var_2D
  subroutine get_var_3D(fname_grid,dimname,dims)
    use netcdf
    implicit none
    character(len=*),intent(in) :: fname_grid,dimname
    real(idx),intent(inout) :: dims(:,:,:)
    integer :: ncid,varid
    call check_net(nf90_open(trim(fname_grid),nf90_nowrite,ncid))
    call check_net(nf90_open(trim(fname_grid),nf90_nowrite,ncid))
    call check_net(nf90_inq_varid(ncid,trim(dimname),varid))
    call check_net(nf90_get_var(ncid,varid, dims))
    call check_net(nf90_close(ncid))
  end subroutine get_var_3D
  subroutine get_var_4D(fname_grid,dimname,dims)
    use netcdf
    implicit none
    character(len=*),intent(in) :: fname_grid,dimname
    real(idx),intent(inout) :: dims(:,:,:,:)
    integer :: ncid,varid
    call check_net(nf90_open(trim(fname_grid),nf90_nowrite,ncid))
    call check_net(nf90_open(trim(fname_grid),nf90_nowrite,ncid))
    call check_net(nf90_inq_varid(ncid,trim(dimname),varid))
    call check_net(nf90_get_var(ncid,varid, dims))
    call check_net(nf90_close(ncid))
  end subroutine get_var_4D
  subroutine get_var_units(fname_in,varname,units)
    use netcdf
    implicit none
    character(len=*),  intent(in) :: fname_in,varname
    character(len=*), intent(out) :: units
    integer :: ncid,varid
    call check_net(nf90_open(trim(fname_in),nf90_nowrite,ncid))
    call check_net(nf90_inq_varid(ncid,varname, varid))
    call check_net(nf90_get_att(ncid, varid, 'units', units) )
  end subroutine get_var_units
end module input_files
