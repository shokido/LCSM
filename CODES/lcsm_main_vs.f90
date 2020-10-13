program lcsm_main_vs
  !*************************************************************************!
  ! Main program of linear ocean model (spatially varying stratification)   !
  ! Requirement: NetCDF library                                             !
  !*************************************************************************!
  !$ use omp_lib
  ! NetCDF
  use ncdf_write
  ! Input and output
!  use io_param
  use input_files
  use mod_grid
  use mod_avg
  use mod_hist
  use mod_rst
  use mod_wind
  use mod_strf
  ! Time routines
  use calendar_sub
  use lcsm_param
  use lcsm_array
  use ocn_dyn  
  implicit none
  ! Usefull integers
  integer :: ix,iy,im,itime
  integer :: nx_p,ny_p,nm
  real(idx) :: total_time
  integer :: ntime
  real(idx) :: time_int,time_int_uw,time_int_vw,time_int_bn
  integer :: iavg_count
  real(idx) :: tmp
  !$ double precision st, en
  !$ st = omp_get_wtime()
  !===========================================!
  ! Open namelist files                       !
  !===========================================!
  open(unit=nmlf_master,file=trim(namelist_master))
  read(unit=nmlf_master,nml=master)
  close(nmlf_master)
  open(unit=nmlf_io,file=trim(namelist_io))
  open(unit=nmlf_param,file=trim(namelist_param))
  read(unit=nmlf_io,nml=time)
  read(unit=nmlf_param,nml=ocn_bdry)
  read(unit=nmlf_param,nml=ocn_visc)
  close(nmlf_param)
  !===============================================!
  ! Get information on time                       !
  !===============================================!
  ! Total timestep
  call calendar_cal_length_ymdhms(start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss,-10000,tmp)
  total_time=int(tmp)
  ntime=int(total_time/dt)
  !===============================================!
  ! Read oceanic grid                             !
  !===============================================!
  read(unit=nmlf_io,nml=grid)
  call read_ocn_grid(trim(fname_in_grid),nx_p,ny_p,x_p,y_p,x_u,y_u,x_v,y_v,&
       & lon_p,lat_p,lon_u,lat_u,lon_v,lat_v,f,mask_p,damp_p,damp_u,damp_v)
  write(*,*) "**********************************"
  write(*,*) "Finish reading oceanic grid data"
  write(*,*) "Grid file name= "//trim(fname_in_grid)
  write(*,*) "**********************************"
  !===============================================!
  ! Set mask array                                !
  !===============================================!
  allocate(mask_u(1:nx_p+1,0:ny_p+1)) ; allocate(mask_v(0:nx_p+1,1:ny_p+1))
  allocate(mask_phi_u(1:nx_p+1,1:ny_p+1)) ;  allocate(mask_phi_v(1:nx_p+1,1:ny_p+1))
  call create_mask(nx_p,ny_p,mask_p,mask_u,mask_v,mask_phi_u,mask_phi_v,slip)
  ! Viscosity
  allocate(nu(0:nx_p+1,0:ny_p+1))
  nu = nu_h
  !===============================================
  ! Read atmospheric data
  !===============================================
  ! Wind stess
  read(unit=nmlf_io,nml=wind)
  call read_TLL_p(nx_p,ny_p,fname_in_uw,varname_uw,wind_x,time_uw,start_yymmdd,start_hhmmss)
  call read_TLL_p(nx_p,ny_p,fname_in_vw,varname_vw,wind_y,time_vw,start_yymmdd,start_hhmmss)
  allocate(tau_x(0:nx_p+1,0:ny_p+1)) ; allocate(tau_y(0:nx_p+1,0:ny_p+1))
  write(*,*) "**********************************"
  write(*,*) "Finish reading wind data"
  write(*,*) "Wind (u) file name= "//trim(fname_in_uw)
  write(*,*) "Wind (v) file name= "//trim(fname_in_vw)
  write(*,*) "**********************************"
  !===============================================
  ! Read vertical mode decomposition data (1-D)
  !===============================================
  read(unit=nmlf_io,nml=strf)
  nm=get_dimsize(fname_in_cn,"mode"); allocate(cn(1:nm))
  call get_var_1D(fname_in_cn,"cn",cn(1:nm))
  write(*,*) "**************************************"
  write(*,*) "Finish reading cn data"
  write(*,*) "VM file name= "//trim(fname_in_cn)
  write(*,*) "*************************************"
  call read_vm_data_nd(nx_p,ny_p,nm,fname_in_bn,varname_bn,bn_in,time_bn,start_yymmdd,start_hhmmss)
  allocate(bn(1:nm,0:nx_p+1,0:ny_p+1))
  write(*,*) "**********************************"
  write(*,*) "Finish reading bn data"
  write(*,*) "VM file name= "//trim(fname_in_bn)
  write(*,*) "**********************************"
  !===============================================
  ! Initialize oceanic array
  !===============================================
  read(unit=nmlf_io,nml=init)
  ! Restart case
  if (in_rst_flag .eq. "T") then
     call read_rst_data(trim(fname_in_rst),nx_p,ny_p,nm,u,v,p,&
          & u_past,v_past,p_past,u_next,v_next,p_next)
     write(*,*) "**************************************"
     write(*,*) "Finish reading restart file"
     write(*,*) "VM file name= "//trim(fname_in_rst)
     write(*,*) "*************************************"
  else
     call initialize_ocn(nx_p,ny_p,nm,u,v,p,u_past,v_past,p_past,&
          & u_next,v_next,p_next)
  end if
  !===============================================
  ! Prepare output file
  !===============================================
  ! History file
  read(unit=nmlf_io,nml=output_hist)
  write(*,*) "Creating output file (history)....."
  write(*,*) "File name= "//trim(fname_out_hist)
  call create_hist(trim(fname_out_hist),nx_p,ny_p,nm,x_p,y_p,x_u,y_u,x_v,y_v,&
       & lon_p,lat_p,lon_u,lat_u,lon_v,lat_v,&
       & start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss,&
       & out_hist_flag,out_hist_int,missing_value,istep_hist,ntime_hist)
  ! Average file
  read(unit=nmlf_io,nml=output_avg)
  write(*,*) "Creating output file (average)....."
  write(*,*) "File name= "//trim(fname_out_avg)
  call create_avg(trim(fname_out_avg),nx_p,ny_p,nm,x_p,y_p,x_u,y_u,x_v,y_v,&
       & lon_p,lat_p,lon_u,lat_u,lon_v,lat_v,&
       & start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss,&
       & out_avg_flag,out_avg_int,missing_value,istep_avg,ntime_avg)
  allocate(u_avg(1:nm,1:nx_p+1,0:ny_p+1)); u_avg=u
  allocate(v_avg(1:nm,0:nx_p+1,1:ny_p+1)); v_avg=v
  allocate(p_avg(1:nm,0:nx_p+1,0:ny_p+1)); p_avg=p
  allocate(tau_x_avg(0:nx_p+1,0:ny_p+1)) ; tau_x_avg=0.0_idx
  allocate(tau_y_avg(0:nx_p+1,0:ny_p+1)) ; tau_y_avg=0.0_idx

  ! ======================!
  !  Start point of loop  !
  ! ======================!
  ihist = 1 ; iavg = 1
  iavg_count=0
  ! Starting point of loop
  write(*,*) "Number of points= ",nx_p,"(x),",ny_p,"(y)"
  write(*,*) "Total vertical modes=",nm
  write(*,*) "Number of timestep=",ntime
!  ntime=1
  do itime = 1,ntime
     iavg_count = iavg_count + 1
     time_int=dt*itime
     if (Lcycle_uw .eq. "T") then
        time_int_uw=time_int-int(time_int/(Tcycle_uw*60.0*60.0*24.0))*Tcycle_uw*60.0*60.0*24.0
     else
        time_int_uw=time_int
     end if
     if (Lcycle_vw .eq. "T") then
        time_int_vw=time_int-int(time_int/(Tcycle_vw*60.0*60.0*24.0))*Tcycle_vw*60.0*60.0*24.0
     else
        time_int_vw=time_int
     end if
     if (Lcycle_bn .eq. "T") then
        time_int_bn=time_int-int(time_int/(Tcycle_bn*60.0*60.0*24.0))*Tcycle_bn*60.0*60.0*24.0
     else
        time_int_bn=time_int
     end if

     call time_wgt(time_uw,time_int_uw,ind1_uw,ind2_uw,wgt1_uw,wgt2_uw)
     call time_wgt(time_vw,time_int_vw,ind1_vw,ind2_vw,wgt1_vw,wgt2_vw)
     call time_wgt(time_bn,time_int_bn,ind1_bn,ind2_bn,wgt1_bn,wgt2_bn)
     !Set wind stress and projection coefficients
     do ix = 0,nx_p+1
        do iy = 0,ny_p+1
           tau_x(ix,iy)=set_data(ind1_uw,ind2_uw,wgt1_uw,wgt2_uw,wind_x(ix,iy,:))
           tau_y(ix,iy)=set_data(ind1_vw,ind2_vw,wgt1_vw,wgt2_vw,wind_y(ix,iy,:))
           do im = 1,nm
              bn(im,ix,iy)=set_data(ind1_bn,ind2_bn,wgt1_bn,wgt2_bn,bn_in(ix,iy,im,:))
           end do
        end do
     end do
     tau_x_avg=tau_x_avg+tau_x
     tau_y_avg=tau_y_avg+tau_y

     !$omp parallel private(iy,ix)
     !$omp do 
     do im = 1,nm
        call dyn_shallow_p(nx_p,ny_p,x_p,y_p,x_u,y_u,x_v,y_v,mask_p,damp_p,&
             & u(im,1:nx_p+1,0:ny_p+1),v(im,0:nx_p+1,1:ny_p+1),p(im,0:nx_p+1,0:ny_p+1),&
             & p_past(im,0:nx_p+1,0:ny_p+1),p_next(im,0:nx_p+1,0:ny_p+1),cn(im),bn(im,0:nx_p+1,0:ny_p+1),dt)
        call dyn_shallow_u(nx_p,ny_p,x_p,y_p,x_u,y_u,x_v,y_v,f,mask_u,mask_phi_u,damp_u,nu,tau_x,&
             & u(im,1:nx_p+1,0:ny_p+1),v(im,0:nx_p+1,1:ny_p+1),p(im,0:nx_p+1,0:ny_p+1),&
             & u_past(im,1:nx_p+1,0:ny_p+1),u_next(im,1:nx_p+1,0:ny_p+1),cn(im),bn(im,0:nx_p+1,0:ny_p+1),dt,A)
        call dyn_shallow_v(nx_p,ny_p,x_p,y_p,x_u,y_u,x_v,y_v,f,mask_v,mask_phi_v,damp_v,nu,tau_y,&
             & u(im,1:nx_p+1,0:ny_p+1),v(im,0:nx_p+1,1:ny_p+1),p(im,0:nx_p+1,0:ny_p+1),&
             & v_past(im,0:nx_p+1,1:ny_p+1),v_next(im,0:nx_p+1,1:ny_p+1),cn(im),bn(im,0:nx_p+1,0:ny_p+1),dt,A)
        ! Apply asselin fiter
        p(im,0:nx_p+1,0:ny_p+1) = p(im,0:nx_p+1,0:ny_p+1) + 0.5_idx * 0.2_idx * mask_p*(p_next(im,0:nx_p+1,0:ny_p+1) + p_past(im,0:nx_p+1,0:ny_p+1)-2.0_idx*p(im,0:nx_p+1,0:ny_p+1))
        u(im,1:nx_p+1,0:ny_p+1) = u(im,1:nx_p+1,0:ny_p+1) + 0.5_idx * 0.2_idx * mask_u*(u_next(im,1:nx_p+1,0:ny_p+1) + u_past(im,1:nx_p+1,0:ny_p+1)-2.0_idx*u(im,1:nx_p+1,0:ny_p+1))
        v(im,0:nx_p+1,1:ny_p+1) = v(im,0:nx_p+1,1:ny_p+1) + 0.5_idx * 0.2_idx * mask_v*(v_next(im,0:nx_p+1,1:ny_p+1) + v_past(im,0:nx_p+1,1:ny_p+1)-2.0_idx*v(im,0:nx_p+1,1:ny_p+1))
        !
        ! Apply boundary conditions
        call set_bc_p(nx_p,ny_p,p(im,0:nx_p+1,0:ny_p+1),p_past(im,:,:),p_next(im,:,:),wbc_p,ebc_p,nbc_p,sbc_p)
        call set_bc_u(nx_p,ny_p,u(im,:,:),u_past(im,:,:),u_next(im,:,:),wbc_u,ebc_u,nbc_u,sbc_u,slip)
        call set_bc_v(nx_p,ny_p,v(im,:,:),v_past(im,:,:),v_next(im,:,:),wbc_v,ebc_v,nbc_v,sbc_v,slip)
        u_avg(im,:,:)=u_avg(im,:,:)+u(im,:,:)
        v_avg(im,:,:)=v_avg(im,:,:)+v(im,:,:)
        p_avg(im,:,:)=p_avg(im,:,:)+p(im,:,:)
        ! Update
        u_past(im,:,:) = u(im,:,:) ; v_past(im,:,:) = v(im,:,:) ; p_past(im,:,:) = p(im,:,:)
        u(im,:,:) = u_next(im,:,:) ; v(im,:,:) = v_next(im,:,:) ; p(im,:,:) = p_next(im,:,:)
     end do
     !$omp end do
     !$omp end parallel
     
     !Output history file (snapshots)
     if (itime .eq. istep_hist(ihist)) then
        write(*,*) "Step (history) =",ihist," ",itime
        call write_hist(trim(fname_out_hist),nx_p,ny_p,nm,ihist,u,v,p,&
             & mask_u,mask_v,mask_p,missing_value)
        call write_hist_2d(trim(fname_out_hist),nx_p,ny_p,ihist,tau_x,tau_y,&
             & mask_p,missing_value)
        ihist=ihist+1
        ihist=min(ihist,ntime_hist)
     end if
     if (itime .eq. istep_avg(iavg)) then
        write(*,*) "Step (average) =",iavg," ",itime
        call write_avg(trim(fname_out_avg),nx_p,ny_p,nm,iavg,u_avg,v_avg,p_avg,&
             & mask_u,mask_v,mask_p,missing_value,iavg_count)
        call write_avg_2d(trim(fname_out_avg),nx_p,ny_p,iavg,tau_x_avg,tau_y_avg,&
             & mask_p,missing_value,iavg_count)
        iavg=iavg+1
        iavg=min(iavg,ntime_avg)
        iavg_count=0
     end if
  end do
  !$ en = omp_get_wtime()
  !$ print *, "Elapsed time :", en-st
  ! 
  ! Output restart file
  read(unit=nmlf_io,nml=output_rst)
  close(nmlf_io)
  if (out_rst_flag .eq. "T") then
     call create_rst(trim(fname_out_rst),nx_p,ny_p,nm,x_p,y_p,x_u,y_u,x_v,y_v,&
          & lon_p,lat_p,lon_u,lat_u,lon_v,lat_v,&
          & start_yymmdd,start_hhmmss,ntime*dt,&
          & u,v,p,&
          & u_past,v_past,p_past,missing_value)
  end if

  !===============================================
  ! Deallocation
  ! Coordinate arrays
  deallocate(x_p) ;  deallocate(y_p)
  deallocate(x_u) ;  deallocate(y_u)
  deallocate(x_v) ;  deallocate(y_v)
  deallocate(lon_p) ;  deallocate(lat_p)
  deallocate(lon_u) ;  deallocate(lat_u)
  deallocate(lon_v) ;  deallocate(lat_v)
  ! Coriolis
  deallocate(f)
  ! Mask arrays
  deallocate(mask_p)
  deallocate(mask_u) ;   deallocate(mask_v)
  deallocate(mask_phi_u) ;   deallocate(mask_phi_v)
  deallocate(damp_u) ; deallocate(damp_v); deallocate(damp_p)
  deallocate(nu)
  ! Vertical modes
  deallocate(cn) ; deallocate(bn)
  ! Time arrays
  deallocate(time_uw); deallocate(time_vw)
  deallocate(istep_hist) ; deallocate(istep_avg)
  ! External forcing
  deallocate(wind_x) ; deallocate(wind_y)
  deallocate(tau_x) ; deallocate(tau_y) 
  ! 3D-arrays
  deallocate(u); deallocate(v); deallocate(p)
  deallocate(u_past); deallocate(v_past); deallocate(p_past)
  deallocate(u_next); deallocate(v_next); deallocate(p_next)
end program lcsm_main_vs
