module run_types
  use run_param
  implicit none  
  type :: vector_1d
     real(idx),allocatable :: val(:)
  end type vector_1d
  type :: vector_2d
     real(idx),allocatable :: val(:,:)
  end type vector_2d
  type :: int_2d
     integer,allocatable :: val(:,:)
  end type int_2d
  type :: int_3d
     integer,allocatable :: val(:,:,:)
  end type int_3d
  type :: vector_3d
     real(idx),allocatable :: val(:,:,:)
  end type vector_3d
  type :: vector_4d
     real(idx),allocatable :: val(:,:,:,:)
  end type vector_4d
   type :: ocn_dta
      integer :: nx_p,ny_p,nm
      type(vector_1d) :: modes
      type(vector_1d) :: lon_p, lat_p
      type(vector_1d) :: lon_u, lat_u
      type(vector_1d) :: lon_v, lat_v
      type(vector_2d) :: x_p, y_p
      type(vector_2d) :: x_u, y_u
      type(vector_2d) :: x_v, y_v
      type(vector_2d) :: f,mask_p,mask_u,mask_v
      type(vector_2d) :: mask_phi_u,mask_phi_v
      type(vector_2d) :: damp_u,damp_v,damp_p      
      type(vector_2d) :: tau_x,tau_y,nu
      type(vector_3d) :: cn,obn
      type(vector_3d) :: u,v,p
      type(vector_3d) :: u_past,v_past,p_past
      type(vector_3d) :: u_next,v_next,p_next
      type(vector_2d) :: tau_x_avg,tau_y_avg
      type(vector_3d) :: u_avg,v_avg,p_avg
      type(vector_3d) :: u_rate,u_drag,u_cori,u_prgf,u_wind,u_hdif
      type(vector_3d) :: v_rate,v_drag,v_cori,v_prgf,v_wind,v_hdif
      type(vector_3d) :: p_rate,p_drag,p_dudx,p_dvdy
     end type ocn_dta
  type :: ocn_set
  real(idx) :: A ! in  [m^2/s^3]
  real(idx) :: rho0=1024.0 ! in  [kg/m^3]
  real(idx) :: slip_ind=0.0_idx ! slip_ind=0 (du/dx=0), slip_ind=1 (u=0)
  real(idx) :: nu_h=2.0e4! Horizontal viscocity in [m^2/s]
  ! Boundary
  ! p
  character(len=maxlen) :: wbc_p="Clo" ! Western boundary condition
  character(len=maxlen) :: ebc_p="Clo" ! Eastern boundary condition
  character(len=maxlen) :: nbc_p="Clo" ! Northern boundary condition
  character(len=maxlen) :: sbc_p="Clo" ! Southern boundary condition
  ! u
  character(len=maxlen) :: wbc_u="Clo" ! Western boundary condition
  character(len=maxlen) :: ebc_u="Clo" ! Eastern boundary condition
  character(len=maxlen) :: nbc_u="Clo" ! Northern boundary condition
  character(len=maxlen) :: sbc_u="Clo" ! Southern boundary condition

  ! v
  character(len=maxlen) :: wbc_v="Clo" ! Western boundary condition
  character(len=maxlen) :: ebc_v="Clo" ! Eastern boundary condition
  character(len=maxlen) :: nbc_v="Clo" ! Northern boundary condition
  character(len=maxlen) :: sbc_v="Clo" ! Southern boundary condition
  end type ocn_set
  type :: TLL_dta
     integer :: ind1,ind2,ntime
     real(idx) :: wgt1,wgt2
     type(vector_1d) :: time
     type(vector_1d) :: time_cyc 
     type(vector_2d) :: data_mod
     type(vector_2d) :: data_now
     type(vector_3d) :: data
     character(1) :: Lcycle='F'
     real(idx) :: Tcycle=365.0_idx
  end type TLL_dta
  type :: TLLL_dta
     integer :: ind1,ind2,ntime
     real(idx) :: wgt1,wgt2
     type(vector_1d) :: time
     type(vector_1d) :: time_cyc 
     type(vector_3d) :: data_mod
     type(vector_3d) :: data_now
     type(vector_4d) :: data
     character(1) :: Lcycle='F'
     real(idx) :: Tcycle=365.0_idx
  end type TLLL_dta
end module run_types
