module mod_wind
  use run_param
  implicit none
  ! UW file name
  character(maxlen) :: fname_in_uw,varname_uw
  integer :: ind1_uw,ind2_uw
  real(idx) :: wgt1_uw,wgt2_uw
  real(idx),allocatable :: time_uw(:),wind_x(:,:,:)
  character(1) :: Lcycle_uw
  real(idx) :: Tcycle_uw
  ! VW file name
  character(maxlen) :: fname_in_vw,varname_vw
  integer :: ind1_vw,ind2_vw
  real(idx) :: wgt1_vw,wgt2_vw
  real(idx),allocatable :: time_vw(:),wind_y(:,:,:)
  character(1) :: Lcycle_vw
  real(idx) :: Tcycle_vw

  ! Wind stress array
  real(idx),allocatable :: tau_x(:,:),tau_y(:,:)
  namelist/wind/fname_in_uw,varname_uw,Lcycle_uw,Tcycle_uw
  namelist/wind/fname_in_vw,varname_vw,Lcycle_vw,Tcycle_vw
end module mod_wind
