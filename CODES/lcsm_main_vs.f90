program lcsm_main_vs
  !*************************************************************************!
  ! Main program of linear ocean model (spatially varying stratification)   !
  ! Requirement: NetCDF library                                             !
  !*************************************************************************!
  !$ use omp_lib
  ! NetCDF
  use ncdf_read
  use ncdf_write
  ! Input and output
  use mod_io_master
  use mod_grid
  use mod_avg
  use mod_hist
  use mod_rst
  ! Time routines
  use calendar_sub
  use run_types
  use lcsm_array
  use ocn_dyn  
  implicit none
  type(ocn_set) :: oset
  integer :: ix,iy,im,itime
  integer :: nx_p,ny_p,nm
  real(idx) :: total_time
  integer :: ntime
  real(idx) :: time_int
  integer :: iavg_count
  real(idx) :: tmp
  character(maxlen) :: fname_in_grid
  ! History file
  integer :: out_hist_flag,out_hist_int
  character(maxlen) :: fname_out_hist
  ! Average file
  integer :: out_avg_flag,out_avg_int
  character(maxlen) :: fname_out_avg
  ! Input restart file
  character(1) :: in_rst_flag
  character(maxlen) :: fname_in_rst
  ! Output  restart file
  character(1) :: out_rst_flag
  character(maxlen) :: fname_out_rst
  ! Zonal wind stress
  type(TLL_dta) :: ocn_taux_dta
  integer :: nfile_ocn_taux
  character(len=maxlen),allocatable :: fnames_ocn_taux(:)
  character(len=maxlen) :: timename_ocn_taux,varname_ocn_taux
  character(1) :: Lcycle_ocn_taux
  real(idx) :: Tcycle_ocn_taux
 ! Meridional wind stress
  type(TLL_dta) :: ocn_tauy_dta
  integer :: nfile_ocn_tauy
  character(len=maxlen),allocatable :: fnames_ocn_tauy(:)
  character(len=maxlen) :: timename_ocn_tauy,varname_ocn_tauy
  character(1) :: Lcycle_ocn_tauy
  real(idx) :: Tcycle_ocn_tauy
  ! Stratification file
  character(maxlen) :: fname_in_cn
  type(TLLL_dta) :: ocn_obn_dta
  integer :: nfile_ocn_obn
  character(len=maxlen),allocatable :: fnames_ocn_obn(:)
  character(len=maxlen) :: timename_ocn_obn,varname_ocn_obn
  character(1) :: Lcycle_ocn_obn
  real(idx) :: Tcycle_ocn_obn
  ! Namelist
  namelist/date/dt,start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss
  namelist/grid/fname_in_grid
  namelist/output_hist/out_hist_flag,out_hist_int
  namelist/output_hist/fname_out_hist
  namelist/output_avg/out_avg_flag,out_avg_int
  namelist/output_avg/fname_out_avg
  namelist/init/in_rst_flag,fname_in_rst
  namelist/output_rst/out_rst_flag,fname_out_rst
  namelist/taux_param_ocn/nfile_ocn_taux,timename_ocn_taux,varname_ocn_taux,Lcycle_ocn_taux,Tcycle_ocn_taux
  namelist/taux_io_ocn/fnames_ocn_taux
  namelist/tauy_param_ocn/nfile_ocn_tauy,timename_ocn_tauy,varname_ocn_tauy,Lcycle_ocn_tauy,Tcycle_ocn_tauy
  namelist/tauy_io_ocn/fnames_ocn_tauy
  namelist/strf/fname_in_cn
  namelist/obn_param_ocn/nfile_ocn_obn,timename_ocn_obn,varname_ocn_obn,Lcycle_ocn_obn,Tcycle_ocn_obn
  namelist/obn_io_ocn/fnames_ocn_obn
  namelist/param_ocn/oset
  !$ double precision st, en
  !$ st = omp_get_wtime()
  !===========================================!
  ! Open namelist files                       !
  !===========================================!
  read(5,nml=date)
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
  read(5,nml=grid)
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
  call set_mask(nx_p,ny_p,mask_p,mask_u,mask_v,mask_phi_u,mask_phi_v,oset%slip_ind)
  ! Viscosity
  allocate(nu(0:nx_p+1,0:ny_p+1))
  nu = oset%nu_h
  !===============================================
  ! Read atmospheric data
  !===============================================
  ! Wind stess
  read(5,taux_param_ocn)
  allocate(fnames_ocn_taux(nfile_ocn_taux))
  read(5,taux_io_ocn)
  read(5,tauy_param_ocn)
  allocate(fnames_ocn_tauy(nfile_ocn_tauy))
  read(5,tauy_io_ocn)
 call read_data_TLL_p(nfile_ocn_taux,fnames_ocn_taux,timename_ocn_taux,varname_ocn_taux,&
       & nx_p,ny_p,ocn_taux_dta,start_yymmdd,start_hhmmss) ! Zonal wind
  ocn_taux_dta%Lcycle=Lcycle_ocn_taux;ocn_taux_dta%Tcycle=Tcycle_ocn_taux
  call read_data_TLL_p(nfile_ocn_tauy,fnames_ocn_tauy,timename_ocn_tauy,varname_ocn_tauy,&
       & nx_p,ny_p,ocn_tauy_dta,start_yymmdd,start_hhmmss) ! Meridional wind
  ocn_tauy_dta%Lcycle=Lcycle_ocn_tauy;ocn_tauy_dta%Tcycle=Tcycle_ocn_tauy
  allocate(tau_x(0:nx_p+1,0:ny_p+1)) ; allocate(tau_y(0:nx_p+1,0:ny_p+1))
  write(*,*) "**********************************"
  write(*,*) "Finish reading wind data"
  write(*,*) "**********************************"
  !===============================================
  ! Read vertical mode decomposition data (1-D)
  !===============================================
  read(5,nml=strf)
  call get_dimsize(fname_in_cn,"mode",nm); allocate(cn(1:nm))
  call get_variable(trim(fname_in_cn),"cn",(/1/),(/nm/),cn)
  write(*,*) "**************************************"
  write(*,*) "Finish reading cn data"
  write(*,*) "VM file name= "//trim(fname_in_cn)
  write(*,*) "*************************************"
  read(5,obn_param_ocn)
  allocate(fnames_ocn_obn(nfile_ocn_obn))
  read(5,obn_io_ocn)
  call read_data_TLLL_p(nfile_ocn_obn,fnames_ocn_obn,timename_ocn_obn,varname_ocn_obn,&
       & nx_p,ny_p,nm,ocn_obn_dta,start_yymmdd,start_hhmmss) ! obn
  ocn_obn_dta%Lcycle=Lcycle_ocn_obn;ocn_obn_dta%Tcycle=Tcycle_ocn_obn
  allocate(obn(1:nm,0:nx_p+1,0:ny_p+1))
  write(*,*) "**********************************"
  write(*,*) "Finish reading obn data"
  write(*,*) "**********************************"
  !===============================================
  ! Initialize oceanic array
  !===============================================
  read(5,nml=init)
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
  read(5,nml=output_hist)
  write(*,*) "Creating output file (history)....."
  write(*,*) "File name= "//trim(fname_out_hist)
  call create_hist(trim(fname_out_hist),nx_p,ny_p,nm,x_p,y_p,x_u,y_u,x_v,y_v,&
       & lon_p,lat_p,lon_u,lat_u,lon_v,lat_v,&
       & start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss,&
       & out_hist_flag,out_hist_int,missing_value,istep_hist,ntime_hist)
  ! Average file
  read(5,nml=output_avg)
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
  read(5,nml=output_rst)
  read(5,nml=param_ocn)
  ! ======================!
  !  Start point of loop  !
  ! ======================!
  ihist = 1 ; iavg = 1
  iavg_count=0
  ! Starting point of loop
  write(*,*) "Number of points= ",nx_p,"(x),",ny_p,"(y)"
  write(*,*) "Total vertical modes=",nm
  write(*,*) "Number of timestep=",ntime
  do itime = 1,ntime
     iavg_count = iavg_count + 1
     time_int=dt*itime*sec_to_day
     ! Zonal wind stress
     call get_data_TLL_p(time_int,start_yymmdd,start_hhmmss,nx_p,ny_p,ocn_taux_dta)
     tau_x=ocn_taux_dta%data_now%val
     ! Meridional wind stress
     call get_data_TLL_p(time_int,start_yymmdd,start_hhmmss,nx_p,ny_p,ocn_tauy_dta)
     tau_y=ocn_tauy_dta%data_now%val
     ! Bn
     call get_data_TLLL_p(time_int,start_yymmdd,start_hhmmss,nx_p,ny_p,nm,ocn_obn_dta)
     do im = 1,nm
          obn(im,0:nx_p,0:ny_p)=ocn_obn_dta%data_now%val(0:nx_p,0:ny_p,im)
     end do

     tau_x_avg=tau_x_avg+tau_x;tau_y_avg=tau_y_avg+tau_y

     !$omp parallel private(iy,ix)
     !$omp do 
     do im = 1,nm
        call dyn_shallow_p(nx_p,ny_p,x_p,y_p,x_u,y_u,x_v,y_v,mask_p,damp_p,&
             & u(im,1:nx_p+1,0:ny_p+1),v(im,0:nx_p+1,1:ny_p+1),p(im,0:nx_p+1,0:ny_p+1),&
             & p_past(im,0:nx_p+1,0:ny_p+1),p_next(im,0:nx_p+1,0:ny_p+1),cn(im),obn(im,0:nx_p+1,0:ny_p+1),dt,oset)
        call dyn_shallow_u(nx_p,ny_p,x_p,y_p,x_u,y_u,x_v,y_v,f,mask_u,mask_phi_u,damp_u,nu,tau_x,&
             & u(im,1:nx_p+1,0:ny_p+1),v(im,0:nx_p+1,1:ny_p+1),p(im,0:nx_p+1,0:ny_p+1),&
             & u_past(im,1:nx_p+1,0:ny_p+1),u_next(im,1:nx_p+1,0:ny_p+1),cn(im),obn(im,0:nx_p+1,0:ny_p+1),dt,oset)
        call dyn_shallow_v(nx_p,ny_p,x_p,y_p,x_u,y_u,x_v,y_v,f,mask_v,mask_phi_v,damp_v,nu,tau_y,&
             & u(im,1:nx_p+1,0:ny_p+1),v(im,0:nx_p+1,1:ny_p+1),p(im,0:nx_p+1,0:ny_p+1),&
             & v_past(im,0:nx_p+1,1:ny_p+1),v_next(im,0:nx_p+1,1:ny_p+1),cn(im),obn(im,0:nx_p+1,0:ny_p+1),dt,oset)
        ! Apply asselin fiter
        call asselin_filter_p(nx_p,ny_p,mask_p,p(im,0:nx_p+1,0:ny_p+1),p_past(im,0:nx_p+1,0:ny_p+1),p_next(im,0:nx_p+1,0:ny_p+1))
        call asselin_filter_u(nx_p,ny_p,mask_u,u(im,1:nx_p+1,0:ny_p+1),u_past(im,1:nx_p+1,0:ny_p+1),u_next(im,1:nx_p+1,0:ny_p+1))
        call asselin_filter_v(nx_p,ny_p,mask_v,v(im,0:nx_p+1,1:ny_p+1),v_past(im,0:nx_p+1,1:ny_p+1),v_next(im,0:nx_p+1,1:ny_p+1))
        !
        ! Apply boundary conditions
        call set_bc_p(nx_p,ny_p,p(im,0:nx_p+1,0:ny_p+1),p_past(im,0:nx_p+1,0:ny_p+1),p_next(im,0:nx_p+1,0:ny_p+1),oset%wbc_p,oset%ebc_p,oset%nbc_p,oset%sbc_p)
        call set_bc_u(nx_p,ny_p,u(im,1:nx_p+1,0:ny_p+1),u_past(im,1:nx_p+1,0:ny_p+1),u_next(im,1:nx_p+1,0:ny_p+1),oset%wbc_u,oset%ebc_u,oset%nbc_u,oset%sbc_u,oset%slip_ind)
        call set_bc_v(nx_p,ny_p,v(im,0:nx_p+1,1:ny_p+1),v_past(im,0:nx_p+1,1:ny_p+1),v_next(im,0:nx_p+1,1:ny_p+1),oset%wbc_v,oset%ebc_v,oset%nbc_v,oset%sbc_v,oset%slip_ind)
        u_avg(im,1:nx_p+1,0:ny_p+1)=u_avg(im,1:nx_p+1,0:ny_p+1)+&
             & u(im,1:nx_p+1,0:ny_p+1)
        v_avg(im,0:nx_p+1,1:ny_p+1)=v_avg(im,0:nx_p+1,1:ny_p+1)+&
             & v(im,0:nx_p+1,1:ny_p+1)
        p_avg(im,0:nx_p+1,0:ny_p+1)=p_avg(im,0:nx_p+1,0:ny_p+1)+p(im,0:nx_p+1,0:ny_p+1)
        ! Update
        u_past(im,1:nx_p+1,0:ny_p+1) = u(im,1:nx_p+1,0:ny_p+1)
        u(im,1:nx_p+1,0:ny_p+1) = u_next(im,1:nx_p+1,0:ny_p+1)
        v_past(im,0:nx_p+1,1:ny_p+1) = v(im,0:nx_p+1,1:ny_p+1)
        v(im,0:nx_p+1,1:ny_p+1) = v_next(im,0:nx_p+1,1:ny_p+1)
        p_past(im,0:nx_p+1,0:ny_p+1) = p(im,0:nx_p+1,0:ny_p+1)
        p(im,0:nx_p+1,0:ny_p+1) = p_next(im,0:nx_p+1,0:ny_p+1)
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
  deallocate(cn) ; deallocate(obn)
  ! Time arrays
  deallocate(istep_hist) ; deallocate(istep_avg)
  ! External forcing
  deallocate(tau_x) ; deallocate(tau_y) 
  ! 3D-arrays
  deallocate(u); deallocate(v); deallocate(p)
  deallocate(u_past); deallocate(v_past); deallocate(p_past)
  deallocate(u_next); deallocate(v_next); deallocate(p_next)
end program lcsm_main_vs
