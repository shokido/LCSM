module lcsm_param
  use run_param
  implicit none  
!  real(idx),parameter :: A=1.3e-8 ! in  [m^2/s^3]
  real(idx) :: A ! in  [m^2/s^3]
  real(idx),parameter :: rho0=1024.0 ! in  [kg/m^3]
  ! Masking parameter
  ! no-slip condition => slip = 2 (un=0 at the boundary)
  ! free-slip condition => slip = 0 (du /dn = 0 at the boundary)
  real(idx) :: slip,nu_h
  ! Sponge layer parameter
  ! Boundary
  ! p
  character(len=*),parameter :: wbc_p="Clo" ! Western boundary condition
  character(len=*),parameter :: ebc_p="Clo" ! Eastern boundary condition
  character(len=*),parameter :: nbc_p="Clo" ! Northern boundary condition
  character(len=*),parameter :: sbc_p="Clo" ! Southern boundary condition
  
  ! u
  character(len=*),parameter :: wbc_u="Clo" ! Western boundary condition
  character(len=*),parameter :: ebc_u="Clo" ! Eastern boundary condition
  character(len=*),parameter :: nbc_u="Clo" ! Northern boundary condition
  !character(len=*),parameter :: sbc_u="Clo" ! Southern boundary condition
  !character(len=*),parameter :: wbc_u="Gra" ! Western boundary condition
  !character(len=*),parameter :: ebc_u="Gra" ! Eastern boundary condition
  !character(len=*),parameter :: nbc_u="Gra" ! Northern boundary condition
  character(len=*),parameter :: sbc_u="Gra" ! Southern boundary condition

  ! v
  character(len=*),parameter :: wbc_v="Clo" ! Western boundary condition
  character(len=*),parameter :: ebc_v="Clo" ! Eastern boundary condition
  character(len=*),parameter :: nbc_v="Clo" ! Northern boundary condition
  character(len=*),parameter :: sbc_v="Clo" ! Southern boundary condition
  !character(len=*),parameter :: wbc_v="Gra" ! Western boundary condition
  !character(len=*),parameter :: ebc_v="Gra" ! Eastern boundary condition
  !character(len=*),parameter :: nbc_v="Gra" ! Northern boundary condition
  !character(len=*),parameter :: sbc_v="Gra" ! Southern boundary condition
  namelist/ocn_bdry/slip,nu_h
  namelist/ocn_visc/A
end module lcsm_param
