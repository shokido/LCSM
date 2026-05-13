module mod_lcsm_solver
  use run_param
  use run_types
  implicit none
contains
  !=============================================
  ! Check CFL condition
  !=============================================
  subroutine check_cfl(nx,ny,x_p,y_p,nu,dt)
    integer,intent(in) :: nx,ny
    real(idx),intent(in) :: x_p(0:nx+1), y_p(0:ny+1)
    real(idx),intent(in) :: nu(0:nx+1,0:ny+1)
    real(idx),intent(in) :: dt
    real(idx) :: dx_min,dy_min,dx0_min,c
    real(idx) :: nu_min,nu_max
    real(idx) :: t_damp
    real(idx) :: L_BL
    dx_min = minval(x_p(1:nx+1)-x_p(0:nx))
    dy_min = minval(y_p(1:ny+1)-y_p(0:ny))
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
  subroutine set_bc_p(nx,ny,p,wbc_flag,ebc_flag,nbc_flag,sbc_flag)
    implicit none
    integer,intent(in) :: nx,ny
    real(idx),intent(inout) :: p(0:nx+1,0:ny+1)
    character(len=*),intent(in) :: wbc_flag,ebc_flag,nbc_flag,sbc_flag
    integer :: ix,iy
    !=================================
    ! Western boundary condition
    !=================================
    select case(wbc_flag)
    case ("Clo","CLO","clo") ! Closed boundary condition
       do iy = 1,ny
          p(0,iy) = 0.0_idx
       end do
    case ("Gra","GRA","gra") ! Gradient boundary condition
       do iy = 1,ny
          p(0,iy) = p(1,iy)
       end do
    case ("Per","PER","per") ! Periodic boundary condition
      ! x_p(1)=x_p(nx)
       do iy = 1,ny
          p(0,iy) = p(nx-1,iy)
       end do
    end select
    !=================================
    ! Eastern boundary condition
    !=================================    
    select case(ebc_flag)
    case ("Clo","CLO","clo") ! Closed boundary condition
       do iy = 1,ny
          p(nx+1,iy) = 0.0_idx
       end do
    case ("Gra","GRA","gra") ! Gradient boundary condition
       do iy = 1,ny
          p(nx+1,iy) = p(nx,iy)
       end do
    case ("Per","PER","per") ! Periodic boundary condition
      ! x_p(1)=x_p(nx)
       do iy = 1,ny
          p(nx+1,iy) = p(2,iy)
       end do
    end select
    !=================================
    ! Southern boundary condition
    !=================================
    select case(sbc_flag)
    case ("Clo","CLO","clo") ! Closed boundary condition
       do ix = 1,nx
          p(ix,0) = 0.0_idx
       end do
    case ("Gra","GRA","gra") ! Gradient boundary condition
       do ix = 1,nx
          p(ix,0) = p(ix,1)
       end do
    case ("Per","PER","per") ! Periodic boundary condition
      ! y_p(1)=y_p(ny)
       do ix = 1,nx
          p(ix,0) = p(ix,ny-1)
       end do
    end select
    !=================================
    ! Northern boundary condition
    !=================================
    select case(nbc_flag)
    case ("Clo","CLO","clo") ! Closed boundary condition
       do ix = 1,nx
          p(ix,ny+1) = 0.0_idx
       end do
    case ("Gra","GRA","gra") ! Gradient boundary condition
       do ix = 1,nx
          p(ix,ny+1) = p(ix,ny)
       end do
    case ("Per","PER","per") ! Periodic boundary condition
       do ix = 1,nx
      ! y_p(1)=y_p(ny)
          p(ix,ny+1) = p(ix,2)
       end do
    end select
  end subroutine set_bc_p
  subroutine set_bc_u(nx,ny,u,wbc_flag,ebc_flag,nbc_flag,sbc_flag,slip_ind)
    implicit none
    integer,intent(in) :: nx,ny
    real(idx),intent(inout) :: u(1:nx+1,0:ny+1)
    character(len=*),intent(in) :: wbc_flag,ebc_flag,nbc_flag,sbc_flag
    real(idx),intent(in) :: slip_ind
    real(idx) :: gamma2
    integer :: ix,iy
    gamma2 = 1.0_idx - 2*slip_ind
    ! gamma2=1 for slip_ind=0 (du/dx=0),
    ! gamma2=-1 for slip_ind=1 (u=0)
    !=================================
    ! Western boundary condition
    !=================================
    select case(wbc_flag)
    case ("Clo","CLO","clo") ! Closed boundary condition
       do iy = 1,ny
          u(1,iy) = 0.0_idx
       end do
    case ("Gra","GRA","gra") ! Gradient boundary condition
       do iy = 1,ny
          u(1,iy) = u(2,iy)
       end do
    case ("Per","PER","per") ! Periodic boundary condition
       do iy = 1,ny
          u(1,iy) = u(nx,iy)
       end do
    end select
    !=================================
    ! Eastern boundary condition
    !=================================    
    select case(ebc_flag)
    case ("Clo","CLO","clo") ! Closed boundary condition
       do iy = 1,ny
          u(nx+1,iy) = 0.0_idx
       end do
    case ("Gra","GRA","gra") ! Gradient boundary condition
       do iy = 1,ny
          u(nx+1,iy) = u(nx,iy)
       end do
    case ("Per","PER","per") ! Periodic boundary condition
       do iy = 1,ny
          u(nx+1,iy) = u(2,iy)
       end do
    end select
    !=================================
    ! Southern boundary condition
    !=================================
    select case(sbc_flag)
    case ("Clo","CLO","clo") ! Closed boundary condition
       do ix = 2,nx
          u(ix,0) = gamma2 * u(ix,1)
       end do
    case ("Gra","GRA","gra") ! Gradient boundary condition
       do ix = 2,nx
          u(ix,0) = u(ix,1)
       end do
    case ("Per","PER","per") ! Periodic boundary condition
       do ix = 2,nx
          u(ix,0) = u(ix,ny-1)
       end do
    end select
    !=================================
    ! Northern boundary condition
    !=================================
    select case(nbc_flag)
    case ("Clo","CLO","clo") ! Closed boundary condition
       do ix = 2,nx
          u(ix,ny+1) = gamma2 * u(ix,ny)
       end do
    case ("Gra","GRA","gra") ! Gradient boundary condition
       do ix = 2,nx
          u(ix,ny+1) = u(ix,ny)
       end do
    case ("Per","PER","per") ! Periodic boundary condition
       do ix = 2,nx
          u(ix,ny+1) = u(ix,2)
       end do
    end select
  end subroutine set_bc_u
  subroutine set_bc_v(nx,ny,v,wbc_flag,ebc_flag,nbc_flag,sbc_flag,slip_ind)
    implicit none
    integer,intent(in) :: nx,ny
!    real(idx),intent(in) :: v(0:nx+1,1:ny+1),v_past(0:nx+1,1:ny+1)
    real(idx),intent(inout) :: v(0:nx+1,1:ny+1)
    character(len=*),intent(in) :: wbc_flag,ebc_flag,nbc_flag,sbc_flag
    real(idx),intent(in) :: slip_ind
    real(idx) :: gamma2
    integer :: ix,iy
    gamma2 = 1.0_idx - 2*slip_ind 
    ! gamma2=1 for slip_ind=0 (du/dx=0),
    ! gamma2=-1 for slip_ind=1 (u=0)
    !=================================
    ! Western boundary condition
    !=================================
    select case(wbc_flag)
    case ("Clo","CLO","clo") ! Closed boundary condition
       do iy = 2,ny
          v(0,iy) = gamma2 * v(1,iy)
       end do
    case ("Gra","GRA","gra") ! Gradient boundary condition
       do iy = 2,ny
          v(0,iy) = v(1,iy)
       end do
    case ("Per","PER","per") ! Periodic boundary condition
       do iy = 2,ny
          v(0,iy) = v(nx-1,iy)
       end do
    end select
    !=================================
    ! Eastern boundary condition
    !=================================    
    select case(ebc_flag)
    case ("Clo","CLO","clo") ! Closed boundary condition
       do iy = 2,ny
          v(nx+1,iy) = gamma2 * v(nx,iy)
       end do
    case ("Gra","GRA","gra") ! Gradient boundary condition
       do iy = 2,ny
          v(nx+1,iy) = v(nx,iy)
       end do
    case ("Per","PER","per") ! Periodic boundary condition
       do iy = 2,ny
          v(nx+1,iy) = v(2,iy)
       end do
    end select
    !=================================
    ! Southern boundary condition
    !=================================
    select case(sbc_flag)
    case ("Clo","CLO","clo") ! Closed boundary condition
       do ix = 1,nx
          v(ix,1) = 0.0_idx
       end do
    case ("Gra","GRA","gra") ! Gradient boundary condition
       do ix = 1,nx
          v(ix,1) = v(ix,2)
       end do
    case ("Per","PER","per") ! Periodic boundary condition
       do ix = 1,nx
          v(ix,1) = v(ix,ny)
       end do
    end select
    !=================================
    ! Northern boundary condition
    !=================================
    select case(nbc_flag)
    case ("Clo","CLO","clo") ! Closed bondary condition
       do ix = 1,nx
          v(ix,ny+1) = 0.0_idx
       end do
    case ("Gra","GRA","gra") ! Gradient boundary condition
       do ix = 1,nx
          v(ix,ny+1) = v(ix,ny)
       end do
    case ("Per","PER","per") ! Periodic boundary condition
       do ix = 1,nx
          v(ix,ny+1) = v(ix,2)
       end do
    end select
  end subroutine set_bc_v
  !==================================================================
  ! Solve Shallow water equation under wind forcing
  !==================================================================
  subroutine get_rhs_sw_p(ogrd,oset,u,v,p,cn,obn,rhs_p,p_drag,p_dudx,p_dvdy)
    implicit none
    type(ocn_dta),intent(in) :: ogrd
    type(ocn_set),intent(in) :: oset
    real(idx),intent(in) :: u(1:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(in) :: v(0:ogrd%nx_p+1,1:ogrd%ny_p+1)
    real(idx),intent(in) :: p(0:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(in) :: cn(0:ogrd%nx_p+1,0:ogrd%ny_p+1),obn(0:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(inout) :: rhs_p(0:ogrd%nx_p+1,0:ogrd%ny_p+1)   
    real(idx),intent(inout) :: p_drag(0:ogrd%nx_p+1,0:ogrd%ny_p+1)   
    real(idx),intent(inout) :: p_dudx(0:ogrd%nx_p+1,0:ogrd%ny_p+1)   
    real(idx),intent(inout) :: p_dvdy(0:ogrd%nx_p+1,0:ogrd%ny_p+1)   
    integer :: ix,iy
    rhs_p=0.0_idx
    p_drag=0.0_idx
    p_dudx=0.0_idx
    p_dvdy=0.0_idx
    !
    ! p    x:1~nx # 0:WB nx+1:EB
    !      y:1~ny # 0:SB ny+1:NB
    do iy = 1,ogrd%ny_p
       do ix = 1,ogrd%nx_p
          !=================================
          ! p equation
          !=================================
          p_drag(ix,iy) = -1.0_idx *ogrd%mask_p%val(ix,iy)* p(ix,iy) * (oset%A / (cn(ix,iy)**2)+ogrd%damp_p%val(ix,iy))
          p_dudx(ix,iy)=-1.0_idx*ogrd%mask_p%val(ix,iy)*(cn(ix,iy)**2)*(u(ix+1,iy)-u(ix,iy)) / (ogrd%x_u%val(ix+1,iy)-ogrd%x_u%val(ix,iy))
          p_dvdy(ix,iy)=-1.0_idx*ogrd%mask_p%val(ix,iy)*(cn(ix,iy)**2)*(v(ix,iy+1)-v(ix,iy)) / (ogrd%y_v%val(ix,iy+1)-ogrd%y_v%val(ix,iy))
          rhs_p(ix,iy)=(p_drag(ix,iy) + p_dudx(ix,iy) + p_dvdy(ix,iy))
       end do
    end do
  end subroutine get_rhs_sw_p
  subroutine get_rhs_sw_u(ogrd,oset,u,v,p,cn,obn,tau_x,rhs_u,u_drag,u_cori,u_prgf,u_wind,u_hdif)
    implicit none
    type(ocn_dta),intent(in) :: ogrd
    type(ocn_set),intent(in) :: oset
    real(idx),intent(in) :: u(1:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(in) :: v(0:ogrd%nx_p+1,1:ogrd%ny_p+1)
    real(idx),intent(in) :: p(0:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(in) :: cn(0:ogrd%nx_p+1,0:ogrd%ny_p+1),obn(0:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(in) :: tau_x(0:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(inout) :: rhs_u(1:ogrd%nx_p+1,0:ogrd%ny_p+1)   
    real(idx),intent(inout) :: u_drag(1:ogrd%nx_p+1,0:ogrd%ny_p+1)   
    real(idx),intent(inout) :: u_cori(1:ogrd%nx_p+1,0:ogrd%ny_p+1)   
    real(idx),intent(inout) :: u_prgf(1:ogrd%nx_p+1,0:ogrd%ny_p+1)   
    real(idx),intent(inout) :: u_wind(1:ogrd%nx_p+1,0:ogrd%ny_p+1)   
    real(idx),intent(inout) :: u_hdif(1:ogrd%nx_p+1,0:ogrd%ny_p+1)   
    integer :: ix,iy
    real(idx) :: dudx_e,dudx_w,dudy_n,dudy_s
    real(idx) :: sigma_n,sigma_s,sigma_w,sigma_e
    real(idx) :: cn_local
    rhs_u=0.0_idx
    u_drag=0.0_idx;u_cori=0.0_idx;u_prgf=0.0_idx;u_wind=0.0_idx;u_hdif=0.0_idx
    !
    ! u    x:2~nx # 1:WB nx+1:EB
    !      y:1~ny # 0:SB ny+1:NB
    do iy = 1,ogrd%ny_p
       do ix = 2,ogrd%nx_p
          !=================================
          ! U equation
          !=================================
          cn_local= 0.5_idx*(cn(ix-1,iy)+cn(ix,iy))
          u_drag(ix,iy) = -1.0_idx *ogrd%mask_u%val(ix,iy)* u(ix,iy) * (oset%A / (cn_local**2)+ogrd%damp_u%val(ix,iy))
          u_cori(ix,iy)= 0.125_idx *ogrd%mask_u%val(ix,iy)* &
          & ((ogrd%f%val(ix,iy)+ogrd%f%val(ix,iy+1))*(v(ix,iy+1)+v(ix-1,iy+1)) + &
               & (ogrd%f%val(ix,iy-1)+ogrd%f%val(ix,iy)) * (v(ix,iy)+v(ix-1,iy)))                 ! Coriolis force
          u_prgf(ix,iy)= -1.0_idx*ogrd%mask_u%val(ix,iy)*(p(ix,iy)-p(ix-1,iy)) / &
          & (ogrd%x_p%val(ix,iy)-ogrd%x_p%val(ix-1,iy)) !Pressure gradient force
          u_wind(ix,iy)= 0.25_idx*ogrd%mask_u%val(ix,iy)*&
          & (tau_x(ix-1,iy) + tau_x(ix,iy)) * (obn(ix-1,iy)+obn(ix,iy)) / oset%rho0            ! Wind forcing
          ! U-viscosity
          dudx_e = (u(ix+1,iy) - u(ix,iy)) / (ogrd%x_u%val(ix+1,iy)-ogrd%x_u%val(ix,iy))
          dudx_w = (u(ix,iy) - u(ix-1,iy)) / (ogrd%x_u%val(ix,iy)-ogrd%x_u%val(ix-1,iy))
          dudy_n = ogrd%mask_phi_u%val(ix,iy+1) * (u(ix,iy+1)-u(ix,iy)) / (ogrd%y_u%val(ix,iy+1)-ogrd%y_u%val(ix,iy))
          dudy_s = ogrd%mask_phi_u%val(ix,iy) * (u(ix,iy)-u(ix,iy-1)) / (ogrd%y_u%val(ix,iy)-ogrd%y_u%val(ix,iy-1))
          sigma_n = 0.25_idx * (ogrd%nu%val(ix-1,iy)+ogrd%nu%val(ix,iy)+ogrd%nu%val(ix-1,iy+1)+ogrd%nu%val(ix,iy+1)) * dudy_n
          sigma_s = 0.25_idx * (ogrd%nu%val(ix-1,iy-1)+ogrd%nu%val(ix,iy-1)+ogrd%nu%val(ix-1,iy)+ogrd%nu%val(ix,iy)) * dudy_s
          sigma_e = ogrd%nu%val(ix,iy) * dudx_e
          sigma_w = ogrd%nu%val(ix-1,iy) * dudx_w
          u_hdif(ix,iy) = ogrd%mask_u%val(ix,iy)*(sigma_e-sigma_w) / (ogrd%x_p%val(ix,iy)-ogrd%x_p%val(ix-1,iy)) +&
          & (sigma_n - sigma_s) / (ogrd%y_v%val(ix,iy+1)-ogrd%y_v%val(ix,iy))
          rhs_u(ix,iy)=u_drag(ix,iy) + u_cori(ix,iy) + u_prgf(ix,iy) + u_wind(ix,iy) +u_hdif(ix,iy)
       end do
    end do
  end subroutine get_rhs_sw_u
  subroutine get_rhs_sw_v(ogrd,oset,u,v,p,cn,obn,tau_y,rhs_v,v_drag,v_cori,v_prgf,v_wind,v_hdif)
    implicit none
    type(ocn_dta),intent(in) :: ogrd
    type(ocn_set),intent(in) :: oset
    real(idx),intent(in) :: u(1:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(in) :: v(0:ogrd%nx_p+1,1:ogrd%ny_p+1)
    real(idx),intent(in) :: p(0:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(in) :: cn(0:ogrd%nx_p+1,0:ogrd%ny_p+1),obn(0:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(in) :: tau_y(0:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(inout) :: rhs_v(0:ogrd%nx_p+1,1:ogrd%ny_p+1)   
    real(idx),intent(inout) :: v_drag(0:ogrd%nx_p+1,1:ogrd%ny_p+1)   
    real(idx),intent(inout) :: v_cori(0:ogrd%nx_p+1,1:ogrd%ny_p+1)   
    real(idx),intent(inout) :: v_prgf(0:ogrd%nx_p+1,1:ogrd%ny_p+1)   
    real(idx),intent(inout) :: v_wind(0:ogrd%nx_p+1,1:ogrd%ny_p+1)   
    real(idx),intent(inout) :: v_hdif(0:ogrd%nx_p+1,1:ogrd%ny_p+1)   
    integer :: ix,iy
    real(idx) :: cory,pgrdy,ty,diffv,drag_v
    real(idx) :: dvdx_e,dvdx_w,dvdy_n,dvdy_s
    real(idx) :: sigma_n,sigma_s,sigma_w,sigma_e
    real(idx) :: cn_local
    rhs_v=0.0_idx
    v_drag=0.0_idx;v_cori=0.0_idx;v_prgf=0.0_idx;v_wind=0.0_idx;v_hdif=0.0_idx
    ! v    x:1~nx # 0:WB nx+1:EB
    !      y:2~ny # 1:SB ny+1:NB
    do iy = 2,ogrd%ny_p
       do ix = 1,ogrd%nx_p
          !=================================
          ! V equation
          !=================================
          cn_local= 0.5_idx*(cn(ix,iy-1)+cn(ix,iy))
          v_drag(ix,iy) = -1.0_idx *ogrd%mask_v%val(ix,iy)* v(ix,iy) * (oset%A / (cn_local**2)+ogrd%damp_v%val(ix,iy))
          v_cori(ix,iy)= -0.125_idx*ogrd%mask_v%val(ix,iy)* &
          & (ogrd%f%val(ix,iy)+ogrd%f%val(ix,iy-1))*(u(ix,iy-1)+u(ix+1,iy-1)+u(ix,iy)+u(ix+1,iy)) ! Coriolis force(ENS conserve)
          v_prgf(ix,iy)=-1.0_idx*ogrd%mask_v%val(ix,iy)*(p(ix,iy)-p(ix,iy-1)) /&
          &  (ogrd%y_p%val(ix,iy)-ogrd%y_p%val(ix,iy-1)) ! pressure gradient force
          v_wind(ix,iy)=0.25_idx*ogrd%mask_v%val(ix,iy)*(tau_y(ix,iy-1) + tau_y(ix,iy)) * (obn(ix,iy-1)+obn(ix,iy)) &
          & / oset%rho0
          dvdx_e = ogrd%mask_phi_v%val(ix+1,iy) * (v(ix+1,iy) -  v(ix,iy)) / (ogrd%x_v%val(ix+1,iy)-ogrd%x_v%val(ix,iy))
          dvdx_w = ogrd%mask_phi_v%val(ix,iy) * (v(ix,iy) -  v(ix-1,iy)) / (ogrd%x_v%val(ix,iy)-ogrd%x_v%val(ix-1,iy))
          dvdy_n = (v(ix,iy+1) - v(ix,iy)) / (ogrd%y_v%val(ix,iy+1)-ogrd%y_v%val(ix,iy))
          dvdy_s = (v(ix,iy) - v(ix,iy-1)) / (ogrd%y_v%val(ix,iy)-ogrd%y_v%val(ix,iy-1))
          sigma_n = ogrd%nu%val(ix,iy)*dvdy_n
          sigma_s = ogrd%nu%val(ix,iy-1)*dvdy_s
          sigma_e = 0.25_idx * (ogrd%nu%val(ix,iy-1)+ogrd%nu%val(ix+1,iy-1)+ogrd%nu%val(ix,iy)+ogrd%nu%val(ix+1,iy))*dvdx_e
          sigma_w = 0.25_idx * (ogrd%nu%val(ix-1,iy-1)+ogrd%nu%val(ix,iy-1)+ogrd%nu%val(ix-1,iy)+ogrd%nu%val(ix,iy))*dvdx_w
          v_hdif(ix,iy) = ogrd%mask_v%val(ix,iy)*(sigma_e-sigma_w) / (ogrd%x_u%val(ix+1,iy)-ogrd%x_u%val(ix,iy)) + &
          &  (sigma_n - sigma_s) / (ogrd%y_p%val(ix,iy)-ogrd%y_p%val(ix,iy-1))
          rhs_v(ix,iy)=v_drag(ix,iy) + v_cori(ix,iy) + v_prgf(ix,iy) + v_wind(ix,iy)+ v_hdif(ix,iy)
       end do
    end do
  end subroutine get_rhs_sw_v
  subroutine solve_sw_mode_asselin(ogrd,oset,u,v,p,u_past,v_past,p_past,u_next,v_next,p_next,&
   & cn,obn,tau_x,tau_y,dt)
    implicit none
    type(ocn_dta),intent(inout) :: ogrd
    type(ocn_set),intent(in) :: oset
    real(idx),intent(in) :: dt
    integer :: ix,iy
    real(idx),intent(inout) :: u(1:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(inout) :: v(0:ogrd%nx_p+1,1:ogrd%ny_p+1)
    real(idx),intent(inout) :: p(0:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(in) :: u_past(1:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(in) :: v_past(0:ogrd%nx_p+1,1:ogrd%ny_p+1)
    real(idx),intent(in) :: p_past(0:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(inout) :: u_next(1:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(inout) :: v_next(0:ogrd%nx_p+1,1:ogrd%ny_p+1)
    real(idx),intent(inout) :: p_next(0:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(in) :: cn(0:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(in) :: obn(0:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(in) :: tau_x(0:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(in) :: tau_y(0:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),allocatable :: p_rate(:,:),p_drag(:,:),p_dudx(:,:),p_dvdy(:,:)
    real(idx),allocatable :: u_rate(:,:),u_drag(:,:),u_cori(:,:),u_prgf(:,:),u_wind(:,:),u_hdif(:,:)
    real(idx),allocatable :: v_rate(:,:),v_drag(:,:),v_cori(:,:),v_prgf(:,:),v_wind(:,:),v_hdif(:,:)
    allocate(p_rate(0:ogrd%nx_p+1,0:ogrd%ny_p+1));
    allocate(p_drag(0:ogrd%nx_p+1,0:ogrd%ny_p+1));
    allocate(p_dudx(0:ogrd%nx_p+1,0:ogrd%ny_p+1));
    allocate(p_dvdy(0:ogrd%nx_p+1,0:ogrd%ny_p+1));
    allocate(u_rate(1:ogrd%nx_p+1,0:ogrd%ny_p+1));
    allocate(u_drag(1:ogrd%nx_p+1,0:ogrd%ny_p+1));
    allocate(u_cori(1:ogrd%nx_p+1,0:ogrd%ny_p+1));
    allocate(u_prgf(1:ogrd%nx_p+1,0:ogrd%ny_p+1));
    allocate(u_wind(1:ogrd%nx_p+1,0:ogrd%ny_p+1));
    allocate(u_hdif(1:ogrd%nx_p+1,0:ogrd%ny_p+1));
    allocate(v_rate(0:ogrd%nx_p+1,1:ogrd%ny_p+1));
    allocate(v_drag(0:ogrd%nx_p+1,1:ogrd%ny_p+1));
    allocate(v_cori(0:ogrd%nx_p+1,1:ogrd%ny_p+1));
    allocate(v_prgf(0:ogrd%nx_p+1,1:ogrd%ny_p+1));
    allocate(v_wind(0:ogrd%nx_p+1,1:ogrd%ny_p+1));
    allocate(v_hdif(0:ogrd%nx_p+1,1:ogrd%ny_p+1));
    call get_rhs_sw_p(ogrd,oset,u,v,p,cn,obn,p_rate,p_drag,p_dudx,p_dvdy)
    call get_rhs_sw_u(ogrd,oset,u,v,p,cn,obn,tau_x,u_rate,u_drag,u_cori,u_prgf,u_wind,u_hdif)
    call get_rhs_sw_v(ogrd,oset,u,v,p,cn,obn,tau_y,v_rate,v_drag,v_cori,v_prgf,v_wind,v_hdif)
    do iy = 1,ogrd%ny_p
       do ix = 2,ogrd%nx_p
          ! Update u
          u_next(ix,iy)=u_past(ix,iy) +2.0_idx*dt*ogrd%mask_u%val(ix,iy) * u_rate(ix,iy)
       end do
    end do
    do iy = 2,ogrd%ny_p
       do ix = 1,ogrd%nx_p
          ! Update u
          v_next(ix,iy)=v_past(ix,iy) +2.0_idx*dt*ogrd%mask_v%val(ix,iy) * v_rate(ix,iy)
       end do
    end do
    do iy = 1,ogrd%ny_p
       do ix = 1,ogrd%nx_p
          ! Update u
          p_next(ix,iy)=p_past(ix,iy) +2.0_idx*dt*ogrd%mask_p%val(ix,iy) * p_rate(ix,iy)
       end do
    end do
  ! Apply Asselin filter
   do iy = 1,ogrd%ny_p
      do ix =1,ogrd%nx_p
         p(ix,iy) = p(ix,iy) + 0.5_idx * 0.2_idx * ogrd%mask_p%val(ix,iy)*(p_next(ix,iy) + p_past(ix,iy)-2.0_idx*p(ix,iy))
      end do
   end do
   do iy = 1,ogrd%ny_p
      do ix =2,ogrd%nx_p
          u(ix,iy) = u(ix,iy) + 0.5_idx * 0.2_idx *ogrd%mask_u%val(ix,iy)*(u_next(ix,iy) + u_past(ix,iy)-2.0_idx*u(ix,iy))
       end do
    end do
   do iy = 2,ogrd%ny_p
      do ix =1,ogrd%nx_p
          v(ix,iy) = v(ix,iy) + 0.5_idx * 0.2_idx * ogrd%mask_v%val(ix,iy)*(v_next(ix,iy) + v_past(ix,iy)-2.0_idx*v(ix,iy))
       end do
    end do
   call set_bc_p(ogrd%nx_p,ogrd%ny_p,&
        & p_next(0:ogrd%nx_p+1,0:ogrd%ny_p+1),&
        & oset%wbc_p,oset%ebc_p,oset%nbc_p,oset%sbc_p)
   call set_bc_u(ogrd%nx_p,ogrd%ny_p,&
        & u_next(1:ogrd%nx_p+1,0:ogrd%ny_p+1),&
        & oset%wbc_u,oset%ebc_u,oset%nbc_u,oset%sbc_u,&
        & oset%slip_ind)
   call set_bc_v(ogrd%nx_p,ogrd%ny_p,&
        & v_next(0:ogrd%nx_p+1,1:ogrd%ny_p+1),&
        & oset%wbc_v,oset%ebc_v,oset%nbc_v,oset%sbc_v,&
        & oset%slip_ind)
   deallocate(p_rate);deallocate(p_drag);deallocate(p_dudx);deallocate(p_dvdy)
   deallocate(u_rate);deallocate(u_drag);deallocate(u_cori);deallocate(u_prgf);deallocate(u_wind);deallocate(u_hdif)
   deallocate(v_rate);deallocate(v_drag);deallocate(v_cori);deallocate(v_prgf);deallocate(v_wind);deallocate(v_hdif)
  end subroutine solve_sw_mode_asselin
  subroutine solve_sw_mode_asselin_diag(ogrd,oset,u,v,p,u_past,v_past,p_past,u_next,v_next,p_next,&
   & cn,obn,tau_x,tau_y,&
   & u_rate,u_drag,u_cori,u_prgf,u_wind,u_hdif,&
   & v_rate,v_drag,v_cori,v_prgf,v_wind,v_hdif,&
   & p_rate,p_drag,p_dudx,p_dvdy,&
   & dt)
    implicit none
    type(ocn_dta),intent(inout) :: ogrd
    type(ocn_set),intent(in) :: oset
    real(idx),intent(in) :: dt
    integer :: ix,iy
    real(idx),intent(inout) :: u(1:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(inout) :: v(0:ogrd%nx_p+1,1:ogrd%ny_p+1)
    real(idx),intent(inout) :: p(0:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(in) :: u_past(1:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(in) :: v_past(0:ogrd%nx_p+1,1:ogrd%ny_p+1)
    real(idx),intent(in) :: p_past(0:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(inout) :: u_next(1:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(inout) :: v_next(0:ogrd%nx_p+1,1:ogrd%ny_p+1)
    real(idx),intent(inout) :: p_next(0:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(in) :: cn(0:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(in) :: obn(0:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(in) :: tau_x(0:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(in) :: tau_y(0:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(inout):: u_rate(1:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(inout):: u_drag(1:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(inout):: u_cori(1:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(inout):: u_prgf(1:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(inout):: u_wind(1:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(inout):: u_hdif(1:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(inout) :: v_rate(0:ogrd%nx_p+1,1:ogrd%ny_p+1)
    real(idx),intent(inout) :: v_drag(0:ogrd%nx_p+1,1:ogrd%ny_p+1)
    real(idx),intent(inout) :: v_cori(0:ogrd%nx_p+1,1:ogrd%ny_p+1)
    real(idx),intent(inout) :: v_prgf(0:ogrd%nx_p+1,1:ogrd%ny_p+1)
    real(idx),intent(inout) :: v_wind(0:ogrd%nx_p+1,1:ogrd%ny_p+1)
    real(idx),intent(inout) :: v_hdif(0:ogrd%nx_p+1,1:ogrd%ny_p+1)
    real(idx),intent(inout) :: p_rate(0:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(inout) :: p_drag(0:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(inout) :: p_dudx(0:ogrd%nx_p+1,0:ogrd%ny_p+1)
    real(idx),intent(inout) :: p_dvdy(0:ogrd%nx_p+1,0:ogrd%ny_p+1)
    call get_rhs_sw_p(ogrd,oset,u,v,p,cn,obn,p_rate,p_drag,p_dudx,p_dvdy)
    call get_rhs_sw_u(ogrd,oset,u,v,p,cn,obn,tau_x,u_rate,u_drag,u_cori,u_prgf,u_wind,u_hdif)
    call get_rhs_sw_v(ogrd,oset,u,v,p,cn,obn,tau_y,v_rate,v_drag,v_cori,v_prgf,v_wind,v_hdif)
    do iy = 1,ogrd%ny_p
       do ix = 2,ogrd%nx_p
          ! Update u
          u_next(ix,iy)=u_past(ix,iy) +2.0_idx*dt*ogrd%mask_u%val(ix,iy) * u_rate(ix,iy)
       end do
    end do
    do iy = 2,ogrd%ny_p
       do ix = 1,ogrd%nx_p
          ! Update u
          v_next(ix,iy)=v_past(ix,iy) +2.0_idx*dt*ogrd%mask_v%val(ix,iy) * v_rate(ix,iy)
       end do
    end do
    do iy = 1,ogrd%ny_p
       do ix = 1,ogrd%nx_p
          ! Update u
          p_next(ix,iy)=p_past(ix,iy) +2.0_idx*dt*ogrd%mask_p%val(ix,iy) * p_rate(ix,iy)
       end do
    end do
  ! Apply Asselin filter
   do iy = 1,ogrd%ny_p
      do ix =1,ogrd%nx_p
         p(ix,iy) = p(ix,iy) + 0.5_idx * 0.2_idx * ogrd%mask_p%val(ix,iy)*(p_next(ix,iy) + p_past(ix,iy)-2.0_idx*p(ix,iy))
      end do
   end do
   do iy = 1,ogrd%ny_p
      do ix =2,ogrd%nx_p
          u(ix,iy) = u(ix,iy) + 0.5_idx * 0.2_idx *ogrd%mask_u%val(ix,iy)*(u_next(ix,iy) + u_past(ix,iy)-2.0_idx*u(ix,iy))
       end do
    end do
   do iy = 2,ogrd%ny_p
      do ix =1,ogrd%nx_p
          v(ix,iy) = v(ix,iy) + 0.5_idx * 0.2_idx * ogrd%mask_v%val(ix,iy)*(v_next(ix,iy) + v_past(ix,iy)-2.0_idx*v(ix,iy))
       end do
    end do
   call set_bc_p(ogrd%nx_p,ogrd%ny_p,&
        & p_next(0:ogrd%nx_p+1,0:ogrd%ny_p+1),&
        & oset%wbc_p,oset%ebc_p,oset%nbc_p,oset%sbc_p)
   call set_bc_u(ogrd%nx_p,ogrd%ny_p,&
        & u_next(1:ogrd%nx_p+1,0:ogrd%ny_p+1),&
        & oset%wbc_u,oset%ebc_u,oset%nbc_u,oset%sbc_u,&
        & oset%slip_ind)
   call set_bc_v(ogrd%nx_p,ogrd%ny_p,&
        & v_next(0:ogrd%nx_p+1,1:ogrd%ny_p+1),&
        & oset%wbc_v,oset%ebc_v,oset%nbc_v,oset%sbc_v,&
        & oset%slip_ind)
   end subroutine solve_sw_mode_asselin_diag
end module mod_lcsm_solver
