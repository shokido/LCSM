module run_param
  implicit none
  integer(8),parameter :: idx = 8
  integer,parameter :: maxlen=300
  real(idx),parameter :: missing_value=9999.0_idx
  ! Namelist
  integer,parameter :: nmlf_master=10,nmlf_param=20,nmlf_io=30
  character(len=*),parameter :: namelist_master="master.nml"
  character(maxlen) :: namelist_param,namelist_io
  ! Time setting
  real(idx) :: dt
  integer :: start_yymmdd,start_hhmmss
  integer :: end_yymmdd,end_hhmmss
  ! Model parameters
  ! General parameters
  real(idx),parameter :: pi = 4.0_idx *atan(1.0_idx)
  real(idx),parameter :: day_to_sec=60.0_idx * 60.0_idx * 24.0_idx  ! [s/day]
  real(idx),parameter :: sec_to_day=1.0_idx / (60.0_idx * 60.0_idx*24.0_idx)  ! [day/s]
  real(idx),parameter :: year_to_sec=60.0_idx * 60.0_idx * 24.0_idx * 365 ! [s/year]
  ! Namelist
  namelist/master/namelist_param,namelist_io
  namelist/time/dt,start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss
end module run_param
