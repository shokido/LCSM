module io_param
  use run_param
  implicit none
  ! Restart==============================================================
  character(1) :: rst_in_flag,rst_out_flag
  character(maxlen) :: fname_rst_in,fname_rst_out
  ! Input==============================================================
  ! Ocean grid file name
  character(maxlen) :: fname_ocn_grid
  ! Vertical coordinate file name
  character(maxlen) :: fname_strf_1d
  ! Vertical coordinate file name
  character(maxlen) :: fname_strf_2d
  ! Density file name
  character(maxlen) :: fname_sdens
  namelist/init/fname_ocn_grid
  namelist/init/rst_in_flag
  namelist/init/fname_rst_in
  namelist/strf/fname_strf_1d
!  namelist/strf2d/fname_strf_2d
!  namelist/dens/fname_sdens
end module io_param
