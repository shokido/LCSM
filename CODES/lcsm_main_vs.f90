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
  use mod_diag
  use mod_hist
  use mod_rst
  ! Time routines
  use calendar_sub
  use run_types
!  use lcsm_array
  use mod_lcsm_solver
  implicit none
  type(ocn_dta) :: ogrd  
  type(ocn_set) :: oset
  integer :: ix,iy,im,itime,ifile
  integer :: nm
  real(idx) :: total_time
  integer :: ntime
  real(idx) :: time_int
  real(idx) :: tmp
  character(maxlen) :: fname_in_grid
  ! History file
  integer :: out_hist_flag=0,out_hist_int=0
  character(maxlen) :: fname_out_hist=""
  ! Average file
  integer :: out_avg_flag=0,out_avg_int=0
  character(maxlen) :: fname_out_avg=""
  ! Input restart file
  integer :: out_diag_flag=0,out_diag_int=0
  character(maxlen) :: fname_out_diag=""
  ! Input restart file
  character(1) :: in_rst_flag="F"
  character(maxlen) :: fname_in_rst=""
  ! Output  restart file
  character(1) :: out_rst_flag="T"
  character(maxlen) :: fname_out_rst=""
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
  ! Phase speed
  type(TLLL_dta) :: ocn_cn_dta
  integer :: nfile_ocn_cn
  character(len=maxlen),allocatable :: fnames_ocn_cn(:)
  character(len=maxlen) :: timename_ocn_cn,varname_ocn_cn
  character(1) :: Lcycle_ocn_cn
  real(idx) :: Tcycle_ocn_cn
  ! Wind projection coefficient
  type(TLLL_dta) :: ocn_obn_dta
  integer :: nfile_ocn_obn
  character(len=maxlen),allocatable :: fnames_ocn_obn(:)
  character(len=maxlen) :: timename_ocn_obn,varname_ocn_obn
  character(1) :: Lcycle_ocn_obn
  real(idx) :: Tcycle_ocn_obn
  ! Namelist
  namelist/date/dt,start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss
  namelist/grid/fname_in_grid
  namelist/output/out_hist_flag,out_hist_int
  namelist/output/fname_out_hist
  namelist/output/out_avg_flag,out_avg_int
  namelist/output/fname_out_avg
  namelist/output/out_diag_flag,out_diag_int
  namelist/output/fname_out_diag
  namelist/output/out_rst_flag,fname_out_rst
  namelist/init/in_rst_flag,fname_in_rst
  namelist/taux_param_ocn/nfile_ocn_taux,timename_ocn_taux,varname_ocn_taux,Lcycle_ocn_taux,Tcycle_ocn_taux
  namelist/taux_io_ocn/fnames_ocn_taux
  namelist/tauy_param_ocn/nfile_ocn_tauy,timename_ocn_tauy,varname_ocn_tauy,Lcycle_ocn_tauy,Tcycle_ocn_tauy
  namelist/tauy_io_ocn/fnames_ocn_tauy
  namelist/cn_param_ocn/nfile_ocn_cn,timename_ocn_cn,varname_ocn_cn,Lcycle_ocn_cn,Tcycle_ocn_cn
  namelist/cn_io_ocn/fnames_ocn_cn
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
  call read_ocn_grd(fname_in_grid,ogrd)
  write(*,*) "**********************************"
  write(*,*) "Finish reading oceanic grid data"
  write(*,*) "Grid file name= "//trim(fname_in_grid)
  write(*,*) "**********************************"
  !===============================================!
  ! Set mask array                                !
  !===============================================!
  call set_mask(ogrd,oset%slip_ind)
  call initialize_ocn_visc(ogrd,oset)
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
       & ogrd%nx_p,ogrd%ny_p,ocn_taux_dta,start_yymmdd,start_hhmmss) ! Zonal wind
  ocn_taux_dta%Lcycle=Lcycle_ocn_taux;ocn_taux_dta%Tcycle=Tcycle_ocn_taux
  call read_data_TLL_p(nfile_ocn_tauy,fnames_ocn_tauy,timename_ocn_tauy,varname_ocn_tauy,&
       & ogrd%nx_p,ogrd%ny_p,ocn_tauy_dta,start_yymmdd,start_hhmmss) ! Meridional wind
  ocn_tauy_dta%Lcycle=Lcycle_ocn_tauy;ocn_tauy_dta%Tcycle=Tcycle_ocn_tauy
  write(*,*) "**********************************"
  write(*,*) "Finish reading wind data"
  do ifile =1,nfile_ocn_taux
       write(*,*) "zonal wind stress file name= "//trim(fnames_ocn_taux(ifile))
  end do
  do ifile =1,nfile_ocn_tauy
       write(*,*) "meridional wind stress file name= "//trim(fnames_ocn_tauy(ifile))
  end do
  write(*,*) "**********************************"
  !===============================================
  ! Read vertical mode decomposition data (1-D)
  !===============================================
  read(5,cn_param_ocn)
  allocate(fnames_ocn_cn(nfile_ocn_cn))
  read(5,cn_io_ocn)
  call get_dimsize(fnames_ocn_cn(1),"mode",nm)
  ogrd%nm=nm
  allocate(ogrd%modes%val(1:nm))
  do im =1,nm
     ogrd%modes%val(im)=im*1.0_idx
  end do
  call read_data_TLLL_p(nfile_ocn_cn,fnames_ocn_cn,timename_ocn_cn,varname_ocn_cn,&
       & ogrd%nx_p,ogrd%ny_p,ogrd%nm,ocn_cn_dta,start_yymmdd,start_hhmmss) ! cn
  ocn_cn_dta%Lcycle=Lcycle_ocn_cn;ocn_cn_dta%Tcycle=Tcycle_ocn_cn
  write(*,*) "**************************************"
  write(*,*) "Finish reading cn data"
  do ifile =1,nfile_ocn_cn
       write(*,*) "Phase speed file name= "//trim(fnames_ocn_cn(ifile))
  end do
  write(*,*) "*************************************"
  read(5,obn_param_ocn)
  allocate(fnames_ocn_obn(nfile_ocn_obn))
  read(5,obn_io_ocn)
  call read_data_TLLL_p(nfile_ocn_obn,fnames_ocn_obn,timename_ocn_obn,varname_ocn_obn,&
       & ogrd%nx_p,ogrd%ny_p,ogrd%nm,ocn_obn_dta,start_yymmdd,start_hhmmss) ! obn
  ocn_obn_dta%Lcycle=Lcycle_ocn_obn;ocn_obn_dta%Tcycle=Tcycle_ocn_obn
  write(*,*) "**********************************"
  write(*,*) "Finish reading obn data"
  do ifile =1,nfile_ocn_obn
       write(*,*) "obn file name= "//trim(fnames_ocn_obn(ifile))
  end do
  write(*,*) "**********************************"
  !===============================================
  ! Initialize oceanic array
  !===============================================
  call allocate_ocn_arrays(ogrd)
  read(5,nml=init)
  ! Restart case
  if (in_rst_flag .eq. "T") then
     call read_rst_data(trim(fname_in_rst),ogrd)
     write(*,*) "**************************************"
     write(*,*) "Finish reading restart file"
     write(*,*) "VM file name= "//trim(fname_in_rst)
     write(*,*) "*************************************"
  else
     call clean_ocn_arrays(ogrd)
  end if
  !===============================================
  ! Prepare output file
  !===============================================
  ! History file
  read(5,nml=output)
  write(*,*) "Creating output file (history)....."
  write(*,*) "File name= "//trim(fname_out_hist)
  if (out_hist_flag .ne. 0) then
       call create_hist(trim(fname_out_hist),ogrd,oset,&
       & start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss,&
       & out_hist_flag,out_hist_int,missing_value,istep_hist,ntime_hist)
  end if  
  if (out_avg_flag .ne. 0) then
     ! Average file
     write(*,*) "Creating output file (average)....."
     write(*,*) "File name= "//trim(fname_out_avg)
     call initialize_ocn_avg(ogrd)  
     call create_avg(trim(fname_out_avg),ogrd,oset,&
          & start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss,&
          & out_avg_flag,out_avg_int,missing_value,istep_avg,ntime_avg)
  end if
  ! Diagnostic file
  write(*,*) "Creating output file (diagnostic))....."
  if (out_diag_flag .ne. 0) then
       write(*,*) "File name= "//trim(fname_out_diag)
       call initialize_ocn_diag(ogrd)  
       call create_diag(trim(fname_out_diag),ogrd,oset,&
            & start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss,&
            & out_diag_flag,out_diag_int,missing_value,istep_diag,ntime_diag)
  endif   
  read(5,nml=param_ocn)
  ! ======================!
  !  Start point of loop  !
  ! ======================!
  ihist = 1 ; iavg = 1;idiag=1
  iavg_count=0;idiag_count=0
  ! Starting point of loop
  write(*,*) "Number of points= ",ogrd%nx_p,"(x),",ogrd%ny_p,"(y)"
  write(*,*) "Total vertical modes=",nm
  write(*,*) "Number of timestep=",ntime
  do itime = 1,ntime
     iavg_count = iavg_count + 1
     idiag_count = idiag_count + 1
     time_int=dt*itime*sec_to_day
     ! Zonal wind stress
     call get_data_TLL_p(time_int,start_yymmdd,start_hhmmss,ogrd%nx_p,ogrd%ny_p,ocn_taux_dta)
     ogrd%tau_x%val=ocn_taux_dta%data_now%val
     ! Meridional wind stress
     call get_data_TLL_p(time_int,start_yymmdd,start_hhmmss,ogrd%nx_p,ogrd%ny_p,ocn_tauy_dta)
     ogrd%tau_y%val=ocn_tauy_dta%data_now%val
     ! Phase speed of gravity wave
     call get_data_TLLL_p(time_int,start_yymmdd,start_hhmmss,ogrd%nx_p,ogrd%ny_p,ogrd%nm,ocn_cn_dta)
     do im = 1,nm
          ogrd%cn%val(im,0:ogrd%nx_p,0:ogrd%ny_p)=ocn_cn_dta%data_now%val(0:ogrd%nx_p,0:ogrd%ny_p,im)
     end do
     ! Inverse of wind-trapping coefficient
     call get_data_TLLL_p(time_int,start_yymmdd,start_hhmmss,ogrd%nx_p,ogrd%ny_p,ogrd%nm,ocn_obn_dta)
     do im = 1,nm
          ogrd%obn%val(im,0:ogrd%nx_p,0:ogrd%ny_p)=ocn_obn_dta%data_now%val(0:ogrd%nx_p,0:ogrd%ny_p,im)
     end do
     !$omp parallel do default(shared) private(im) schedule(static)
     !$omp do 
     do im = 1,nm
          if (out_diag_flag .ne. 0) then
               call solve_sw_mode_asselin_diag(ogrd,oset,&
               & ogrd%u%val(im,1:ogrd%nx_p+1,0:ogrd%ny_p+1),&
               & ogrd%v%val(im,0:ogrd%nx_p+1,1:ogrd%ny_p+1),&
               & ogrd%p%val(im,0:ogrd%nx_p+1,0:ogrd%ny_p+1),&
               & ogrd%u_past%val(im,1:ogrd%nx_p+1,0:ogrd%ny_p+1),&
               & ogrd%v_past%val(im,0:ogrd%nx_p+1,1:ogrd%ny_p+1),&
               & ogrd%p_past%val(im,0:ogrd%nx_p+1,0:ogrd%ny_p+1),&
               & ogrd%u_next%val(im,1:ogrd%nx_p+1,0:ogrd%ny_p+1),&
               & ogrd%v_next%val(im,0:ogrd%nx_p+1,1:ogrd%ny_p+1),&
               & ogrd%p_next%val(im,0:ogrd%nx_p+1,0:ogrd%ny_p+1),&
               & ogrd%cn%val(im,0:ogrd%nx_p+1,0:ogrd%ny_p+1),ogrd%obn%val(im,0:ogrd%nx_p+1,0:ogrd%ny_p+1),&
               & ogrd%tau_x%val(0:ogrd%nx_p+1,0:ogrd%ny_p+1),ogrd%tau_y%val(0:ogrd%nx_p+1,0:ogrd%ny_p+1),&
               & ogrd%u_rate%val(im,1:ogrd%nx_p+1,0:ogrd%ny_p+1),&
               & ogrd%u_drag%val(im,1:ogrd%nx_p+1,0:ogrd%ny_p+1),&
               & ogrd%u_cori%val(im,1:ogrd%nx_p+1,0:ogrd%ny_p+1),&
               & ogrd%u_prgf%val(im,1:ogrd%nx_p+1,0:ogrd%ny_p+1),&
               & ogrd%u_wind%val(im,1:ogrd%nx_p+1,0:ogrd%ny_p+1),&
               & ogrd%u_hdif%val(im,1:ogrd%nx_p+1,0:ogrd%ny_p+1),&
               & ogrd%v_rate%val(im,0:ogrd%nx_p+1,1:ogrd%ny_p+1),&
               & ogrd%v_drag%val(im,0:ogrd%nx_p+1,1:ogrd%ny_p+1),&
               & ogrd%v_cori%val(im,0:ogrd%nx_p+1,1:ogrd%ny_p+1),&
               & ogrd%v_prgf%val(im,0:ogrd%nx_p+1,1:ogrd%ny_p+1),&
               & ogrd%v_wind%val(im,0:ogrd%nx_p+1,1:ogrd%ny_p+1),&
               & ogrd%v_hdif%val(im,0:ogrd%nx_p+1,1:ogrd%ny_p+1),&
               & ogrd%p_rate%val(im,0:ogrd%nx_p+1,0:ogrd%ny_p+1),&
               & ogrd%p_drag%val(im,0:ogrd%nx_p+1,0:ogrd%ny_p+1),&
               & ogrd%p_dudx%val(im,0:ogrd%nx_p+1,0:ogrd%ny_p+1),&
               & ogrd%p_dvdy%val(im,0:ogrd%nx_p+1,0:ogrd%ny_p+1),&
               & dt)
          else
               call solve_sw_mode_asselin(ogrd,oset,&
               & ogrd%u%val(im,1:ogrd%nx_p+1,0:ogrd%ny_p+1),&
               & ogrd%v%val(im,0:ogrd%nx_p+1,1:ogrd%ny_p+1),&
               & ogrd%p%val(im,0:ogrd%nx_p+1,0:ogrd%ny_p+1),&
               & ogrd%u_past%val(im,1:ogrd%nx_p+1,0:ogrd%ny_p+1),&
               & ogrd%v_past%val(im,0:ogrd%nx_p+1,1:ogrd%ny_p+1),&
               & ogrd%p_past%val(im,0:ogrd%nx_p+1,0:ogrd%ny_p+1),&
               & ogrd%u_next%val(im,1:ogrd%nx_p+1,0:ogrd%ny_p+1),&
               & ogrd%v_next%val(im,0:ogrd%nx_p+1,1:ogrd%ny_p+1),&
               & ogrd%p_next%val(im,0:ogrd%nx_p+1,0:ogrd%ny_p+1),&
               & ogrd%cn%val(im,0:ogrd%nx_p+1,0:ogrd%ny_p+1),ogrd%obn%val(im,0:ogrd%nx_p+1,0:ogrd%ny_p+1),&
               & ogrd%tau_x%val(0:ogrd%nx_p+1,0:ogrd%ny_p+1),ogrd%tau_y%val(0:ogrd%nx_p+1,0:ogrd%ny_p+1),dt)
          end if
        ! Update
          ogrd%u_past%val(im,1:ogrd%nx_p+1,0:ogrd%ny_p+1) =ogrd%u%val(im,1:ogrd%nx_p+1,0:ogrd%ny_p+1)
          ogrd%u%val(im,1:ogrd%nx_p+1,0:ogrd%ny_p+1) = ogrd%u_next%val(im,1:ogrd%nx_p+1,0:ogrd%ny_p+1)
          ogrd%v_past%val(im,0:ogrd%nx_p+1,1:ogrd%ny_p+1) = ogrd%v%val(im,0:ogrd%nx_p+1,1:ogrd%ny_p+1)
          ogrd%v%val(im,0:ogrd%nx_p+1,1:ogrd%ny_p+1) = ogrd%v_next%val(im,0:ogrd%nx_p+1,1:ogrd%ny_p+1)
          ogrd%p_past%val(im,0:ogrd%nx_p+1,0:ogrd%ny_p+1) = ogrd%p%val(im,0:ogrd%nx_p+1,0:ogrd%ny_p+1)
          ogrd%p%val(im,0:ogrd%nx_p+1,0:ogrd%ny_p+1) =ogrd%p_next%val(im,0:ogrd%nx_p+1,0:ogrd%ny_p+1)
     end do
     !$omp end do
     !$omp end parallel
     if (out_avg_flag .ne. 0) then
          call oper_avg_ocn(ogrd)
     end if
     if (out_diag_flag .ne. 0) then
          call oper_diag_ocn(ogrd)
     end if
     !Output history file (snapshots)
     if (itime .eq. istep_hist(ihist)) then
        write(*,*) "Step (history) =",ihist," ",itime
        call write_hist(trim(fname_out_hist),ogrd,ihist)
        ihist=ihist+1
        ihist=min(ihist,ntime_hist)
     end if
     if (out_avg_flag .ne. 0) then
          if (itime .eq. istep_avg(iavg)) then
               write(*,*) "Step (average) =",iavg," ",itime
               call write_avg(trim(fname_out_avg),ogrd,iavg,iavg_count)
               call clean_ocn_avg(ogrd)  
               iavg=iavg+1
               iavg=min(iavg,ntime_avg)
               iavg_count=0
          end if
     end if
     if (out_diag_flag .ne. 0) then
          if (itime .eq. istep_diag(idiag)) then
               write(*,*) "Step (diagnosis) =",idiag," ",itime
               call write_diag(trim(fname_out_diag),ogrd,idiag,idiag_count)
               call clean_ocn_diag(ogrd)  
               idiag=idiag+1
               idiag=min(idiag,ntime_diag)
               idiag_count=0
          end if
     end if
  end do
  !$ en = omp_get_wtime()
  !$ print *, "Elapsed time :", en-st
  ! 
  ! Output restart file
  if (out_rst_flag .eq. "T") then
     call create_rst(trim(fname_out_rst),ogrd,missing_value)
  end if
 call deallocate_ocn_arrays(ogrd)
 if (out_avg_flag .ne. 0) then
     call deallocate_ocn_avg(ogrd)
 end if    
 if (out_diag_flag .ne. 0) then
     call deallocate_ocn_diag(ogrd)
 end if    
end program lcsm_main_vs
