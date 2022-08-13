module ocn_dyn
  use run_param
  use lcsm_param
  implicit none
  private
  !=============================================
  ! Apply Asselin filter
  !  subroutine asselin_filter(Nx,Ny,mask_p,mask_u,mask_v,u,v,p,&
  !     & u_past,v_past,p_past,u_next,v_next,p_next)
  !=============================================
  !=============================================
  ! Perform shallow water calculation
  !   subroutine dyn_shallow(nx,ny,x_p,y_p,x_u,y_u,x_v,y_v,f,&
  !      & mask_p,mask_u,mask_v,mask_phi_u,mask_phi_v,nu, &
  !      & tau_x,tau_y,u,v,p,u_past,v_past,p_past,u_next,v_next,p_next,&
  !      & dt)
  !=============================================

  public :: check_cfl
  public :: initialize_ocn
  public :: dyn_shallow_p,dyn_shallow_u,dyn_shallow_v
  public :: asselin_filter_p,asselin_filter_u,asselin_filter_v
  public :: set_bc_u,set_bc_v,set_bc_p
contains
  !=============================================
  ! Check CFL condition
  !=============================================
  subroutine check_cfl(nx,ny,x_rho,y_rho,nu,dt)
    integer,intent(in) :: nx,ny
    real(idx),intent(in) :: x_rho(0:nx+1), y_rho(0:ny+1)
    real(idx),intent(in) :: nu(0:nx+1,0:ny+1)
    real(idx),intent(in) :: dt
    real(idx) :: dx_min,dy_min,dx0_min,c
    real(idx) :: nu_min,nu_max
    real(idx) :: t_damp
    real(idx) :: L_BL
    dx_min = minval(x_rho(1:nx+1)-x_rho(0:nx))
    dy_min = minval(y_rho(1:ny+1)-y_rho(0:ny))
    dx0_min= min(dx_min,dy_min)
    nu_min = minval(nu)
    nu_max = maxval(nu)
    c= dx0_min / dt
    ! Spindown time
    t_damp = (dx0_min**2) / (pi**2 * nu_max)
    ! Boundary layer width
    L_BL = (nu_min / (2.26e-11)) ** (1.0 / 3.0)
    write(*,*) "============================================="
    write(*,*) "Diagnozing validity of choise of parameters"
    write(*,*) "============================================="
    write(*,*) "Spindown test"
    if (dt .lt. t_damp) then
       write(*,*) "Clear Spindown test"
    else
       write(*,*) "Change nu_h or dt"
    end if
    write(*,*) "WBC test"
    if (dx_min .lt. L_BL) then
       write(*,*) "Clear WBC test"
    else
       write(*,*) "This grid is too coarse to resolve western boundary currents!"
       write(*,*) "Change nu_h or grid setting"
    end if
  end subroutine check_cfl
  !=============================================
  ! Prepare ocean array
  !=============================================
  !--------------------------------------------------------------
  ! For full integration
  subroutine initialize_ocn(nx,ny,nm,u,v,p,u_past,v_past,p_past &
       & ,u_next,v_next,p_next)
    integer,intent(in) :: nx,ny,nm
    real(idx),allocatable,intent(inout) :: u(:,:,:),v(:,:,:),p(:,:,:)
    real(idx),allocatable,intent(inout) :: u_past(:,:,:),v_past(:,:,:),p_past(:,:,:)
    real(idx),allocatable,intent(inout) :: u_next(:,:,:),v_next(:,:,:),p_next(:,:,:)
    allocate(u(1:nm,1:nx+1,0:ny+1)) ; allocate(v(1:nm,0:nx+1,1:ny+1)) ;  allocate(p(1:nm,0:nx+1,0:ny+1))
    allocate(u_past(1:nm,1:nx+1,0:ny+1)) ; allocate(v_past(1:nm,0:nx+1,1:ny+1)) ;  allocate(p_past(1:nm,0:nx+1,0:ny+1))
    allocate(u_next(1:nm,1:nx+1,0:ny+1)) ; allocate(v_next(1:nm,0:nx+1,1:ny+1)) ;  allocate(p_next(1:nm,0:nx+1,0:ny+1))
    u=0.0_idx ; v=0.0_idx ; p=0.0_idx
    u_past=0.0_idx ; v_past=0.0_idx ; p_past=0.0_idx
    u_next=0.0_idx ; v_next=0.0_idx ; p_next=0.0_idx
  end subroutine initialize_ocn
  !==================================================================
  ! Solve Shallow water equation under wind forcing
  !==================================================================
  subroutine dyn_shallow_p(nx,ny,x_rho,y_rho,x_u,y_u,x_v,y_v,mask_rho,damp_p,&     
       & u,v,p,p_past,p_next,cn,obn,dt)
    implicit none
    integer,intent(in) :: nx,ny
    real(idx),intent(in) :: x_rho(0:nx+1), y_rho(0:ny+1), x_u(1:nx+1), y_u(0:ny+1), x_v(0:nx+1),y_v(1:ny+1)
    real(idx),intent(in) :: mask_rho(0:nx+1,0:ny+1)
    real(idx),intent(in) :: damp_p(0:nx+1,0:ny+1)
    real(idx),intent(in) :: u(1:nx+1,0:ny+1),v(0:nx+1,1:ny+1),p(0:nx+1,0:ny+1)
    real(idx),intent(in) :: p_past(0:nx+1,0:ny+1)
    real(idx),intent(inout) :: p_next(0:nx+1,0:ny+1)
    real(idx),intent(in) :: dt,cn,obn(0:nx+1,0:ny+1)
    integer :: ix,iy
    real(idx) :: dudx,dvdy,drag_p,rhs_p
    !
    ! p    x:1~nx # 0:WB nx+1:EB
    !      y:1~ny # 0:SB ny+1:NB
    do iy = 1,ny
       do ix = 1,nx
          !=================================
          ! p equation
          !=================================
          drag_p = -1.0_idx * p(ix,iy) * (A / (cn**2)+damp_p(ix,iy))
          dudx=-1.0_idx*(cn**2)*(u(ix+1,iy)-u(ix,iy)) / (x_u(ix+1)-x_u(ix))
          dvdy=-1.0_idx*(cn**2)*(v(ix,iy+1)-v(ix,iy)) / (y_v(iy+1)-y_v(iy))
          rhs_p=drag_p + dudx + dvdy
          p_next(ix,iy)=p_past(ix,iy)+2.0_idx*dt*mask_rho(ix,iy)*rhs_p          
       end do
    end do
  end subroutine dyn_shallow_p
  subroutine dyn_shallow_u(nx,ny,x_rho,y_rho,x_u,y_u,x_v,y_v,f,&
       & mask_u,mask_phi_u,damp_u,nu, &       
       & tau_x,u,v,p,u_past,u_next,cn,obn,dt,A_vis)
    implicit none
    integer,intent(in) :: nx,ny
    real(idx),intent(in) :: x_rho(0:nx+1), y_rho(0:ny+1), x_u(1:nx+1), y_u(0:ny+1), x_v(0:nx+1),y_v(1:ny+1)
    real(idx),intent(in) :: f(0:ny+1)
    real(idx),intent(in) :: mask_u(1:nx+1,0:ny+1),mask_phi_u(1:nx+1,1:ny+1),damp_u(1:nx+1,0:ny+1)
    real(idx),intent(in) :: nu(0:nx+1,0:ny+1)
    real(idx),intent(in) :: tau_x(0:nx+1,0:ny+1)
    real(idx),intent(in) :: u(1:nx+1,0:ny+1),v(0:nx+1,1:ny+1),p(0:nx+1,0:ny+1)
    real(idx),intent(in) :: u_past(1:nx+1,0:ny+1)
    real(idx),intent(inout) :: u_next(1:nx+1,0:ny+1)
    real(idx),intent(in) :: dt,A_vis,cn,obn(0:nx+1,0:ny+1)
    integer :: ix,iy
    real(idx) :: corx,pgrdx,tx,diffu,drag_u,rhs_u
    real(idx) :: dudx_e,dudx_w,dudy_n,dudy_s
    real(idx) :: sigma_n,sigma_s,sigma_w,sigma_e
    !
    ! u    x:2~nx # 1:WB nx+1:EB
    !      y:1~ny # 0:SB ny+1:NB
    do iy = 1,ny
       do ix = 2,nx
          !=================================
          ! U equation
          !=================================
          drag_u = -1.0_idx * u(ix,iy) * (A_vis / (cn**2)+damp_u(ix,iy))
          corx= 0.125_idx * ((f(iy)+f(iy+1))*(v(ix,iy+1)+v(ix-1,iy+1)) + &
               & (f(iy-1)+f(iy)) * (v(ix,iy)+v(ix-1,iy)))                 ! Coriolis force
          !corx= 0.25_idx * f(iy) * (v(ix,iy+1)+v(ix-1,iy+1)+v(ix,iy)+v(ix-1,iy)) ! Coriolis force (ENS conserve) 
          pgrdx= -1.0_idx*(p(ix,iy)-p(ix-1,iy)) / (x_rho(ix)-x_rho(ix-1)) !Pressure gradient force
          tx= 0.5_idx*(tau_x(ix-1,iy) + tau_x(ix,iy)) * (obn(ix-1,iy)+obn(ix,iy)) / rho0            ! Wind forcing
          ! U-viscosity
          dudx_e = (u(ix+1,iy) - u(ix,iy)) / (x_u(ix+1)-x_u(ix))
          dudx_w = (u(ix,iy) - u(ix-1,iy)) / (x_u(ix)-x_u(ix-1))
          dudy_n = mask_phi_u(ix,iy+1) * (u(ix,iy+1)-u(ix,iy)) / (y_u(iy+1)-y_u(iy))
          dudy_s = mask_phi_u(ix,iy) * (u(ix,iy)-u(ix,iy-1)) / (y_u(iy)-y_u(iy-1))
          sigma_n = 0.25_idx * (nu(ix-1,iy)+nu(ix,iy)+nu(ix-1,iy+1)+nu(ix,iy+1)) * dudy_n
          sigma_s = 0.25_idx * (nu(ix-1,iy-1)+nu(ix,iy-1)+nu(ix-1,iy)+nu(ix,iy)) * dudy_s
          sigma_e = nu(ix,iy) * dudx_e
          sigma_w = nu(ix-1,iy) * dudx_w
          diffu = (sigma_e-sigma_w) / (x_rho(ix)-x_rho(ix-1)) + (sigma_n - sigma_s) / (y_v(iy+1)-y_v(iy))
          rhs_u=drag_u + corx + pgrdx + tx +diffu
          u_next(ix,iy)=u_past(ix,iy) + 2.0_idx * dt * mask_u(ix,iy) * rhs_u
       end do
    end do
  end subroutine dyn_shallow_u
  subroutine dyn_shallow_v(nx,ny,x_rho,y_rho,x_u,y_u,x_v,y_v,f,&
       & mask_v,mask_phi_v,damp_v,nu,tau_y,u,v,p,v_past,v_next,cn,obn,dt,A_vis)
    implicit none
    integer,intent(in) :: nx,ny
    real(idx),intent(in) :: x_rho(0:nx+1), y_rho(0:ny+1), x_u(1:nx+1), y_u(0:ny+1), x_v(0:nx+1),y_v(1:ny+1)
    real(idx),intent(in) :: f(0:ny+1)
    real(idx),intent(in) :: mask_v(0:nx+1,1:ny+1),mask_phi_v(1:nx+1,1:ny+1)
    real(idx),intent(in) :: damp_v(0:nx+1,1:ny+1)
    real(idx),intent(in) :: nu(0:nx+1,0:ny+1),tau_y(0:nx+1,0:ny+1)
    real(idx),intent(in) :: u(1:nx+1,0:ny+1),v(0:nx+1,1:ny+1),p(0:nx+1,0:ny+1)
    real(idx),intent(in) :: v_past(0:nx+1,1:ny+1)
    real(idx),intent(inout) :: v_next(0:nx+1,1:ny+1)
    real(idx),intent(in) :: dt,A_vis,cn,obn(0:nx+1,0:ny+1)
    integer :: ix,iy
    real(idx) :: cory,pgrdy,ty,diffv,drag_v,rhs_v
    real(idx) :: dvdx_e,dvdx_w,dvdy_n,dvdy_s
    real(idx) :: sigma_n,sigma_s,sigma_w,sigma_e
    ! v    x:1~nx # 0:WB nx+1:EB
    !      y:2~ny # 1:SB ny+1:NB
    do iy = 2,ny
       do ix = 1,nx
          !=================================
          ! V equation
          !=================================
          drag_v = -1.0_idx * v(ix,iy) * (A_vis / (cn**2)+damp_v(ix,iy))
          cory = -0.125_idx * (f(iy)+f(iy-1))*(u(ix,iy-1)+u(ix+1,iy-1)+u(ix,iy)+u(ix+1,iy)) ! Coriolis force(ENS conserve)
          !cory = -0.25_idx *  (f(iy) * u(ix,iy)+ f(iy)* u(ix+1,iy)+f(iy-1) * u(ix,iy-1)+ f(iy-1)* u(ix+1,iy-1))! Coriolis force (ENG conserve) !
          pgrdy=-1.0_idx*(p(ix,iy)-p(ix,iy-1)) / (y_rho(iy)-y_rho(iy-1)) ! pressure gradient force
          ty=0.5_idx*(tau_y(ix,iy-1) + tau_y(ix,iy)) * (obn(ix,iy-1)+obn(ix,iy)) / rho0
          dvdx_e = mask_phi_v(ix+1,iy) * (v(ix+1,iy) -  v(ix,iy)) / (x_v(ix+1)-x_v(ix))
          dvdx_w = mask_phi_v(ix,iy) * (v(ix,iy) -  v(ix-1,iy)) / (x_v(ix)-x_v(ix-1))
          dvdy_n = (v(ix,iy+1) - v(ix,iy)) / (y_v(iy+1)-y_v(iy))
          dvdy_s = (v(ix,iy) - v(ix,iy-1)) / (y_v(iy)-y_v(iy-1))
          sigma_n = nu(ix,iy)*dvdy_n
          sigma_s = nu(ix,iy-1)*dvdy_s
          sigma_e = 0.25_idx * (nu(ix,iy-1)+nu(ix+1,iy-1)+nu(ix,iy)+nu(ix+1,iy))*dvdx_e
          sigma_w = 0.25_idx * (nu(ix-1,iy-1)+nu(ix,iy-1)+nu(ix-1,iy)+nu(ix,iy))*dvdx_w
          diffv = (sigma_e-sigma_w) / (x_u(ix+1)-x_u(ix)) + (sigma_n - sigma_s) / (y_rho(iy)-y_rho(iy-1))
          rhs_v=drag_v + cory + pgrdy + ty + diffv
          v_next(ix,iy)=v_past(ix,iy)+2.0_idx * dt*mask_v(ix,iy)*rhs_v
       end do
    end do
  end subroutine dyn_shallow_v


  ! ! DIAGNOSTICS
  ! subroutine dyn_shallow_p_diag(nx,ny,x_rho,y_rho,x_u,y_u,x_v,y_v,mask_rho,damp_p,&     
  !      & u,v,p,p_past,p_next,p_rate,p_zconv,p_mconv,p_drag,cn,obn,dt)
  !   implicit none
  !   integer,intent(in) :: nx,ny
  !   real(idx),intent(in) :: x_rho(0:nx+1), y_rho(0:ny+1), x_u(1:nx+1), y_u(0:ny+1), x_v(0:nx+1),y_v(1:ny+1)
  !   real(idx),intent(in) :: mask_rho(0:nx+1,0:ny+1)
  !   real(idx),intent(in) :: damp_p(0:nx+1,0:ny+1)
  !   real(idx),intent(in) :: u(1:nx+1,0:ny+1),v(0:nx+1,1:ny+1),p(0:nx+1,0:ny+1)
  !   real(idx),intent(in) :: p_past(0:nx+1,0:ny+1)
  !   real(idx),intent(in) :: p_zconv(0:nx+1,0:ny+1),p_mconv(0:nx+1,0:ny+1),p_drag(0:nx+1,0:ny+1)
  !   real(idx),intent(inout) :: p_next(0:nx+1,0:ny+1)
  !   real(idx),intent(in) :: dt,cn,obn(0:nx+1,0:ny+1)
  !   integer :: ix,iy
  !   real(idx) :: dudx,dvdy,drag_p,rhs_p
  !   !
  !   ! p    x:1~nx # 0:WB nx+1:EB
  !   !      y:1~ny # 0:SB ny+1:NB
  !   do iy = 1,ny
  !      do ix = 1,nx
  !         !=================================
  !         ! p equation
  !         !=================================
  !         drag_p = -1.0_idx * p(ix,iy) * (A / (cn**2)+damp_p(ix,iy))
  !         dudx=-1.0_idx*(cn**2)*(u(ix+1,iy)-u(ix,iy)) / (x_u(ix+1)-x_u(ix))
  !         dvdy=-1.0_idx*(cn**2)*(v(ix,iy+1)-v(ix,iy)) / (y_v(iy+1)-y_v(iy))
  !         rhs_p=drag_p + dudx + dvdy
  !         p_next(ix,iy)=p_past(ix,iy)+2.0_idx*dt*mask_rho(ix,iy)*rhs_p
  !         p_drag(ix,iy)=p_drag(ix,iy)+2.0_idx*dt*mask_rho(ix,iy)*(drag_p)
  !         p_zconv(ix,iy)=p_zconv(ix,iy)+2.0_idx*dt*mask_rho(ix,iy)*(dudx)
  !         p_mconv(ix,iy)=p_mconv(ix,iy)+2.0_idx*dt*mask_rho(ix,iy)*(dvdy)
  !      end do
  !   end do
  ! end subroutine dyn_shallow_p_diag
  !=============================================
  ! Apply Asselin filter
  !=============================================
 subroutine asselin_filter_p(nx,ny,mask_rho,p,p_past,p_next)
    integer,intent(in) :: nx,ny
    real(idx),intent(in) :: mask_rho(0:nx+1,0:ny+1)
    real(idx),intent(inout) :: p(0:nx+1,0:ny+1)
    real(idx),intent(in) :: p_past(0:nx+1,0:ny+1),p_next(0:nx+1,0:ny+1)
    integer :: ix,iy
    ! Filter-------------------------------------------
    ! p    x:1~nx # 0:WB nx+1:EB
    !      y:1~ny # 0:SB ny+1:NB
   do iy = 1,ny
      do ix =1,nx
         p(ix,iy) = p(ix,iy) + 0.5_idx * 0.2_idx * mask_rho(ix,iy)*(p_next(ix,iy) + p_past(ix,iy)-2.0_idx*p(ix,iy))
      end do
   end do
!    p = p + 0.5_idx * 0.2_idx * mask_rho*(p_next + p_past-2.0_idx*p)
  end subroutine asselin_filter_p
  subroutine asselin_filter_u(nx,ny,mask_u,u,u_past,u_next)
    integer,intent(in) :: nx,ny
    real(idx),intent(in) :: mask_u(1:nx+1,0:ny+1)
    real(idx),intent(inout) :: u(1:nx+1,0:ny+1)
    real(idx),intent(in) :: u_past(1:nx+1,0:ny+1),u_next(1:nx+1,0:ny+1)
    integer :: ix,iy
    ! Filter-------------------------------------------
    ! u    x:2~nx # 1:WB nx+1:EB
    !      y:1~ny # 0:SB ny+1:NB
    do iy = 1,ny
       do ix =2,nx
          u(ix,iy) = u(ix,iy) + 0.5_idx * 0.2_idx * mask_u(ix,iy)*(u_next(ix,iy) + u_past(ix,iy)-2.0_idx*u(ix,iy))
       end do
    end do
    !u = u + 0.5_idx * 0.2_idx * mask_u*(u_next + u_past-2.0_idx*u)
  end subroutine asselin_filter_u
  
  subroutine asselin_filter_v(nx,ny,mask_v,v,v_past,v_next)
    integer,intent(in) :: nx,ny
    real(idx),intent(in) :: mask_v(0:nx+1,1:ny+1)
    real(idx),intent(inout) :: v(0:nx+1,1:ny+1)
    real(idx),intent(in) :: v_past(0:nx+1,1:ny+1),v_next(1:nx+1,1:ny+1)
    integer :: ix,iy
    ! Filter-------------------------------------------
    ! v    x:1~nx # 1:WB nx+1:EB
    !      y:2~ny # 0:SB ny+1:NB
    do iy = 2,ny
       do ix =1,nx
          v(ix,iy) = v(ix,iy) + 0.5_idx * 0.2_idx * mask_v(ix,iy)*(v_next(ix,iy) + v_past(ix,iy)-2.0_idx*v(ix,iy))
       end do
    end do
    !v = v + 0.5_idx * 0.2_idx * mask_v*(v_next + v_past-2.0_idx*v)
          
  end subroutine asselin_filter_v
  !=================================================================================
  ! Set Boundary conditions
  !=================================================================================
  subroutine set_bc_u(nx,ny,u,u_past,u_next,wbc_flag,ebc_flag,nbc_flag,sbc_flag,slip_ind)
    implicit none
    integer,intent(in) :: nx,ny
    real(idx),intent(inout) :: u(1:nx+1,0:ny+1),u_past(1:nx+1,0:ny+1),u_next(1:nx+1,0:ny+1)
    character(len=*),intent(in) :: wbc_flag,ebc_flag,nbc_flag,sbc_flag
    real(idx),intent(in) :: slip_ind
    real(idx) :: gamma2
    integer :: ix,iy
    real(idx) :: mu,mux,muy
    real(idx) :: tiny=1.0e-20
    gamma2 = 1.0_8 - slip_ind ! gamma2=1 for slip_ind=0 (du/dx=0),gamma2=-1 for slip_ind=2 (u=0)
    !=================================
    ! Western boundary condition
    !=================================
    select case(wbc_flag)
    case ("Clo","CLO","clo") ! Closed bondary
       do iy = 1,ny
          u_next(1,iy) = 0.0_8
       end do
    case ("Gra","GRA","gra") ! Gradient boundary
       do iy = 1,ny
          u_next(1,iy) = u_next(2,iy)
       end do
    case ("Rad","RAD","rad") ! Radiation boundary
       do iy = 1,ny
          mu = (u_next(2,iy)-u_past(2,iy)) / (2.0 * u(3,iy)-u_past(2,iy)-u_next(2,iy)+tiny)
          mu = max(0.0_8,mu)
          mu = min(1.0_8,mu)
          u_next(1,iy) = ((1.0_8-mu) * u(1,iy) + 2.0_8 * mu * u(2,iy)) / (1 + mu)
       end do
    case ("Rad2","RAD2","rad2") ! Radiation boundary2
       do iy = 1,ny
          mu = (u(2,iy)-u_past(2,iy)) / (u_past(3,iy)-u_past(2,iy)+tiny)
          mu = max(0.0_8,mu)
          mu = min(1.0_8,mu)
          u_next(1,iy) = (1.0_8-mu) * u(1,iy) + mu * u(2,iy)
       end do    
    end select
    !=================================
    ! Eastern boundary condition
    !=================================    
    select case(ebc_flag)
    case ("Clo","CLO","clo") ! Closed bondary condition
       do iy = 1,ny
          u_next(nx+1,iy) = 0.0_8
       end do
    case ("Gra","GRA","gra") ! Gradient boundary condition
       do iy = 1,ny
          u_next(nx+1,iy) = u_next(nx,iy)
       end do
    case ("Rad","RAD","rad") ! Radiation boundary
       do iy = 1,ny
          mu = (u_next(nx,iy)-u_past(nx,iy)) / (2.0*u(nx-1,iy)-u_past(nx,iy)-u_next(nx,iy)+tiny)
          mu = max(0.0_8,mu)
          mu = min(1.0_8,mu)
          u_next(nx+1,iy) = ((1.0_8-mu) * u(nx+1,iy) + 2.0_8 * mu * u(nx,iy)) / (1.0_8 + mu)
       end do       
    case ("Rad2","RAD2","rad2") ! Radiation boundary mod
       do iy = 1,ny
          mu = (u(nx,iy)-u_past(nx,iy)) / (u_past(nx-1,iy)-u_past(nx,iy)+tiny)
          mu = max(0.0_8,mu)
          mu = min(1.0_8,mu)
          u_next(nx+1,iy) = (1.0_8-mu) * u(nx+1,iy) +  mu * u(nx,iy)
       end do
    end select
    !=================================
    ! Southern boundary condition
    !=================================
    select case(sbc_flag)
    case ("Clo","CLO","clo") ! Closed bondary
       do ix = 2,nx
          u_next(ix,0) = gamma2 * u_next(ix,1)
       end do
    case ("Gra","GRA","gra") ! Gradient boundary
       do ix = 2,nx
          u_next(ix,0) = u_next(ix,1)
       end do
    case ("Rad","RAD","rad") ! Radiation boundary
       do ix = 1,nx
          mu = (u_next(ix,1)-u_past(ix,1)) / &
               &  (2.0_8 * u(ix,2)-u_next(ix,1)-u_past(ix,1)+tiny)
          mu = max(0.0_8,mu)
          mu = min(1.0_8,mu)
          u_next(ix,0) = ((1.0_8-mu) * u(ix,0) + 2.0 * mu * u(ix,1)) / (1.0_8 + mu)
       end do
    case ("Rad2","RAD2","rad2") ! Radiation boundary
       do ix = 1,nx
          mu = (u(ix,1)-u_past(ix,1)) / (u_past(ix,2)-u_past(ix,1))
          mu = max(0.0_8,mu)
          mu = min(1.0_8,mu)
          u_next(ix,0) = (1.0_8-mu) * u(ix,0) + mu * u(ix,1)
       end do
    end select
    !=================================
    ! Northern boundary condition
    !=================================
    select case(nbc_flag)
    case ("Clo","CLO","clo") ! Closed bondary
       do ix = 2,nx
          u_next(ix,ny+1) = gamma2 * u_next(ix,ny)
       end do
    case ("Gra","GRA","gra") ! Gradient boundary
       do ix = 2,nx
          u_next(ix,ny+1) = u_next(ix,ny)
       end do
    case ("Rad","RAD","rad") ! Radiation boundary
       do ix = 2,nx
          mu = (u_next(ix,ny)-u_past(ix,ny)) / &
               &  (2.0_8 * u(ix,ny-1)-u_next(ix,ny)-u_past(ix,ny)+tiny)
          mu = max(0.0_8,mu)
          mu = min(1.0_8,mu)
          u_next(ix,ny+1) = ((1.0_8-mu) * u(ix,ny+1) + 2.0 * mu * u(ix,ny)) / (1.0_8 + mu)
       end do
    case ("Rad2","RAD2","rad2") ! Radiation boundary
       do ix = 2,nx
          mu = (u(ix,ny)-u_past(ix,ny)) / (u_past(ix,ny-1)-u_past(ix,ny))
          mu = max(0.0_8,mu)
          mu = min(1.0_8,mu)
          u_next(ix,ny+1) = (1.0_8-mu) * u(ix,ny+1) + mu * u(ix,ny)
       end do
    end select
  end subroutine set_bc_u
  !=================================================================================
  subroutine set_bc_v(nx,ny,v,v_past,v_next,wbc_flag,ebc_flag,nbc_flag,sbc_flag,slip_ind)
    implicit none
    integer,intent(in) :: nx,ny
    real(idx),intent(inout) :: v(0:nx+1,1:ny+1),v_past(0:nx+1,1:ny+1),v_next(0:nx+1,1:ny+1)
    character(len=*),intent(in) :: wbc_flag,ebc_flag,nbc_flag,sbc_flag
    real(idx),intent(in) :: slip_ind
    real(idx) :: gamma2
    integer :: ix,iy
    real(idx) :: mu,mux,muy
    real(idx) :: tiny=1.0e-20
    gamma2 = 1.0_8 - slip_ind ! gamma2=1 for slip_ind=0 (du/dx=0),gamma2=-1 for slip_ind=2 (u=0)
    !=================================
    ! Western boundary condition
    !=================================
    select case(wbc_flag)
    case ("Clo","CLO","clo") ! Closed bondary
       do iy = 2,ny
          v_next(0,iy) = gamma2 * v_next(1,iy)
       end do
    case ("Gra","GRA","gra") ! Gradient boundary
       do iy = 2,ny
          v_next(0,iy) = v_next(1,iy)
       end do
    case ("Rad","RAD","rad") ! Radiation boundary
       do iy = 2,ny
          mu = (v_next(1,iy)-v_past(1,iy)) / (2.0_8 * v(2,iy)-v_next(1,iy)-v_past(1,iy)+tiny)
          mu = max(0.0_8,mu)
          mu = min(1.0_8,mu)
          v_next(0,iy) = ((1.0_8-mu) * v(0,iy) + 2.0_8 * mu * v(1,iy)) / (1.0_8 + mu)
       end do
    case ("Rad2","RAD2","rad2") ! Radiation boundary
       do iy = 2,ny
          mu = (v(1,iy)-v_past(1,iy)) / (v_past(2,iy)-v_past(1,iy)+tiny)
          mu = max(0.0_8,mu)
          mu = min(1.0_8,mu)
          v_next(0,iy) = (1.0_8-mu) * v(0,iy) + mu * v(1,iy)
       end do
    end select
    !=================================
    ! Eastern boundary condition
    !=================================    
    select case(ebc_flag)
    case ("Clo","CLO","clo") ! Closed bondary
       do iy = 2,ny
          v_next(nx+1,iy) = gamma2 * v_next(nx,iy)
       end do
    case ("Gra","GRA","gra") ! Gradient boundary
       do iy = 2,ny
          v_next(nx+1,iy) = v_next(nx,iy)
       end do
    case ("Rad","RAD","rad") ! Radiation boundary
       do iy = 2,ny
          mu = (v_next(nx,iy)-v_past(nx,iy)) / (2.0_8 * v(nx-1,iy)-v_next(nx,iy)-v_past(nx,iy)+tiny)
          mu = max(0.0_8,mu)
          mu = min(1.0_8,mu)
          v_next(nx+1,iy) = ((1.0_8-mu) * v(nx+1,iy) + 2.0_8 * mu * v(nx,iy)) / (1.0_8 + mu)
       end do
    case ("Rad2","RAD2","rad2") ! Radiation boundary
       do iy = 2,ny
          mu = (v(nx,iy)-v_past(nx,iy)) / (v_past(nx-1,iy)-v_past(nx,iy)+tiny)
          mu = max(0.0_8,mu)
          mu = min(1.0_8,mu)
          v_next(nx+1,iy) = (1.0_8-mu) * v(nx+1,iy) + mu * v(nx,iy)
       end do
    end select
    !=================================
    ! Southern boundary condition
    !=================================
    select case(sbc_flag)
    case ("Clo","CLO","clo") ! Closed bondary
       do ix = 1,nx
          v_next(ix,1) = 0.0_8
       end do
    case ("Gra","GRA","gra") ! Gradient boundary
       do ix = 1,nx
          v_next(ix,1) = v_next(ix,2)
       end do
    case ("Rad","RAD","rad") ! Radiation boundary
       do ix = 1,nx
          mu = (v_next(ix,2)-v_past(ix,2)) / &
               &  (2.0_8 * v(ix,3)-v_next(ix,2)-v_past(ix,2)+tiny)
          mu = max(0.0_8,mu)
          mu = min(1.0_8,mu)
          v_next(ix,1) = ((1.0_8-mu) * v(ix,1) + 2.0_8 * mu * v(ix,2)) / (1.0_8 + mu)
       end do
    case ("Rad2","RAD2","rad2") ! Radiation boundary
       do ix = 1,nx
          mu = (v(ix,2)-v_past(ix,2)) / (v_past(ix,3)-v_past(ix,2)+tiny)
          mu = max(0.0_8,mu)
          mu = min(1.0_8,mu)
          v_next(ix,1) = (1.0_8-mu) * v(ix,1) + mu * v(ix,2)
       end do

    end select
    !=================================
    ! Northern boundary condition
    !=================================
    select case(nbc_flag)
    case ("Clo","CLO","clo") ! Closed bondary
       do ix = 1,nx
          v_next(ix,ny+1) = 0.0_8
       end do
    case ("Gra","GRA","gra") ! Gradient boundary
       do ix = 1,nx
          v_next(ix,ny+1) = v_next(ix,ny)
       end do
    case ("Rad","RAD","rad") ! Radiation boundary
       do ix = 1,nx
          mu = (v_next(ix,ny)-v_past(ix,ny)) / &
               &  (2.0_8 * v(ix,ny-1)-v_next(ix,ny)-v_past(ix,ny)+tiny)
          mu = max(0.0_8,mu)
          mu = min(1.0_8,mu)
          v_next(ix,ny+1) = ((1.0_8-mu) * v(ix,ny+1) + 2.0_8 * mu * v(ix,ny)) / (1.0_8 + mu)
       end do
    case ("Rad2","RAD2","rad2") ! Radiation boundary
       do ix = 1,nx
          mu = (v(ix,ny)-v_past(ix,ny)) / (v_past(ix,ny-1)-v_past(ix,ny)+tiny)
          mu = max(0.0_8,mu)
          mu = min(1.0_8,mu)
          v_next(ix,ny+1) = (1.0_8-mu) * v(ix,ny+1) + mu * v(ix,ny)
       end do
    end select
  end subroutine set_bc_v
  subroutine set_bc_p(nx,ny,p,p_past,p_next,wbc_flag,ebc_flag,nbc_flag,sbc_flag)
    implicit none
    integer,intent(in) :: nx,ny
    real(idx),intent(inout) :: p(0:nx+1,0:ny+1),p_past(0:nx+1,0:ny+1),p_next(0:nx+1,0:ny+1)
    character(len=*),intent(in) :: wbc_flag,ebc_flag,nbc_flag,sbc_flag
    integer :: ix,iy
    real(idx) :: mu,mux,muy
    !=================================
    ! Western boundary condition
    !=================================
    select case(wbc_flag)
    case ("Clo","CLO","clo") ! Closed bondary
       do iy = 1,ny
          p_next(0,iy) = 0.0_8
       end do
    case ("Gra","GRA","gra") ! Gradient boundary
       do iy = 1,ny
          p_next(0,iy) = p_next(1,iy)
       end do
    end select
    !=================================
    ! Eastern boundary condition
    !=================================    
    select case(ebc_flag)
    case ("Clo","CLO","clo") ! Closed bondary
       do iy = 1,ny
          p_next(nx+1,iy) = 0.0_4
       end do
    case ("Gra","GRA","gra") ! Gradient boundary
       do iy = 1,ny
          p_next(nx+1,iy) = p_next(nx,iy)
       end do
    end select
    !=================================
    ! Southern boundary condition
    !=================================
    select case(sbc_flag)
    case ("Clo","CLO","clo") ! Closed bondary
       do ix = 1,nx
          p_next(ix,0) = 0.0_8
       end do
    case ("Gra","GRA","gra") ! Gradient boundary
       do ix = 1,nx
          p_next(ix,0) = p_next(ix,1)
       end do
    case ("Rad","RAD","rad") ! Radiation boundary
       do ix = 1,nx
          mu =(p_next(ix,0))
          mu = max(0.0_8,mu)
          mu = min(1.0_8,mu)
          p_next(ix,0) = p(ix,1)+p(ix,2)
       end do
    end select
    !=================================
    ! Northern boundary condition
    !=================================
    select case(nbc_flag)
    case ("Clo","CLO","clo") ! Closed bondary
       do ix = 1,nx
          p_next(ix,ny+1) = 0.0_8
       end do
    case ("Gra","GRA","gra") ! Gradient boundary
       do ix = 1,nx
          p_next(ix,ny+1) = p_next(ix,ny)
       end do
    case ("Rad","RAD","rad") ! Radiation boundary
       do ix = 1,nx
          mu = (p_next(ix,ny)-p_past(ix,ny)) / &
               &  (2.0_8 * p(ix,ny-1)-p_next(ix,ny)-p_past(ix,ny))
          mu = max(0.0_8,mu)
          mu = min(1.0_8,mu)
          p_next(ix,ny+1) = ((1.0_8-mu) * p(ix,ny+1) + mu * p(ix,ny)) / (1.0_8 + mu)
       end do
    end select

  end subroutine set_bc_p
end module ocn_dyn
