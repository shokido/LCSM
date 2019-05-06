module mod_wind
  use run_param
  implicit none
  ! UW file name
  character(maxlen) :: fname_uw,varname_uw
  integer :: ind1_uw,ind2_uw
  real(idx) :: wgt1_uw,wgt2_uw
  real(idx),allocatable :: time_uw(:),wind_x(:,:,:)
  ! VW file name
  character(maxlen) :: fname_vw,varname_vw
  integer :: ind1_vw,ind2_vw
  real(idx) :: wgt1_vw,wgt2_vw
  real(idx),allocatable :: time_vw(:),wind_y(:,:,:)
  real(idx),allocatable :: tau_x(:,:),tau_y(:,:)
  namelist/wind/fname_uw,varname_uw
  namelist/wind/fname_vw,varname_vw
end module mod_wind
