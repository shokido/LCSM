module mod_strf
  use run_param
  use input_files
  implicit none
  ! BN file name
  character(maxlen) :: fname_in_cn
  character(maxlen) :: fname_in_bn,varname_bn
  integer :: ind1_bn,ind2_bn
  real(idx) :: wgt1_bn,wgt2_bn
  character(1) :: Lcycle_bn
  real(idx) :: Tcycle_bn
  real(idx),allocatable :: time_bn(:)
  real(idx),allocatable :: bn_in(:,:,:,:)
  namelist/strf/fname_in_cn
  namelist/strf/fname_in_bn,varname_bn,Lcycle_bn,Tcycle_bn
contains
  subroutine read_vm_data_nd(nx,ny,nm,fname,varname,data,time,start_yymmdd,start_hhmmss)
    implicit none
    integer,intent(inout) :: nx,ny,nm
    character(len=*),intent(in) :: fname,varname
    real(idx),allocatable,intent(inout) :: data(:,:,:,:)
    real(idx),allocatable,intent(inout) :: time(:)
    integer,intent(in) :: start_yymmdd,start_hhmmss
    integer :: nt,im
    real(idx),allocatable :: time_tmp(:)
    character(len=maxlen) :: time_units
    nt=get_dimsize(fname,"time")
    allocate(time_tmp(nt)) ; allocate(time(nt))
    call get_var_1D(fname,"time",time_tmp)
    call get_var_units(fname,"time",time_units)
    time=modify_time(nt,time_tmp,time_units,start_yymmdd,start_hhmmss)
    allocate(data(0:nx+1,0:ny+1,1:nm,1:nt))
    call get_var_4D(fname,varname,data)
  end subroutine read_vm_data_nd
end module mod_strf
