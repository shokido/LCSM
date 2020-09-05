module lcsm_array
  use run_param
  use lcsm_param
  !================================================
  ! Module for storing arrays neccesary for
  ! numerical integration of LCSM
  !================================================
  implicit none
  ! Longitude and Latitude arrays
  real(idx),allocatable :: lon_vert(:),lat_vert(:)
  real(idx),allocatable :: lon_p(:),lat_p(:)
  real(idx),allocatable :: lon_u(:),lat_u(:)
  real(idx),allocatable :: lon_v(:),lat_v(:)

  ! Horizontal arrays
  real(idx),allocatable :: x_vert(:),y_vert(:)
  real(idx),allocatable :: x_p(:),y_p(:)
  real(idx),allocatable :: x_u(:),y_u(:)
  real(idx),allocatable :: x_v(:),y_v(:)

  ! Coriolis parameter
  real(idx),allocatable :: f(:)

  ! Mask arrays
  real(idx),allocatable :: mask_u(:,:),mask_v(:,:),mask_p(:,:)
  real(idx),allocatable :: mask_phi_u(:,:),mask_phi_v(:,:)

  ! Sponge array
  real(idx),allocatable :: sponge(:,:)
  ! Damper arrays
  real(idx),allocatable :: damp_p(:,:),damp_u(:,:),damp_v(:,:)

  ! Shallow water arrays
  real(idx),allocatable :: u(:,:,:),v(:,:,:),p(:,:,:)
  real(idx),allocatable :: u_past(:,:,:),v_past(:,:,:),p_past(:,:,:)
  real(idx),allocatable :: u_next(:,:,:),v_next(:,:,:),p_next(:,:,:)

  ! Vertical modal function arrays
  real(idx),allocatable :: cn(:)
  real(idx),allocatable :: bn(:,:,:)
  real(idx),allocatable :: hn(:,:,:)

  ! Viscocity
  real(idx),allocatable :: nu(:,:)
end module lcsm_array
