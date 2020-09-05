module input_files
  use run_param
  implicit none
  private
  public :: modify_time,get_var_units,get_dimsize
  public :: get_var_1D,get_var_2D,get_var_3D,get_var_4D
  public :: time_wgt,set_data
  public :: read_TLL_p,read_TLL_u,read_TLL_v
contains
  ! Check_R subroutine--------------------------------------------------------------
  subroutine check_net(status)
    use netcdf
    implicit none
    integer, intent ( in) :: status
    if(status /= nf90_noerr) then
       print *, trim(nf90_strerror(status))
       stop "Stopped"
    end if
  end subroutine check_net
  !------------------------------------------------------
  function get_dimsize(fname_grid,dimname) result(ndim)
    use netcdf
    implicit none
    character(len=*),intent(in) :: fname_grid,dimname
    integer :: ncid,dimid,ndim
    call check_net(nf90_open(trim(fname_grid),nf90_nowrite,ncid))
    call check_net(nf90_inq_dimid(ncid,dimname, dimid) )
    call check_net(nf90_inquire_dimension(ncid, dimid, len=ndim))
    call check_net(nf90_close(ncid))
  end function get_dimsize
  !-----------------------------------------------------
  subroutine get_var_1D(fname_grid,dimname,dims)
    use netcdf
    implicit none
    character(len=*),intent(in) :: fname_grid,dimname
    real(idx),intent(inout) :: dims(:)
    integer :: ncid,varid
    call check_net(nf90_open(trim(fname_grid),nf90_nowrite,ncid))
    call check_net(nf90_inq_varid(ncid,dimname,varid))
    call check_net( nf90_get_var(ncid,varid, dims))
    call check_net(nf90_close(ncid))
  end subroutine get_var_1D
  !-----------------------------------------------------
  subroutine get_var_2D(fname_grid,dimname,dims)
    use netcdf
    implicit none
    character(len=*),intent(in) :: fname_grid,dimname
    real(idx),intent(inout) :: dims(:,:)
    integer :: ncid,varid
    call check_net(nf90_open(trim(fname_grid),nf90_nowrite,ncid))
    call check_net(nf90_inq_varid(ncid,trim(dimname),varid))
    call check_net(nf90_get_var(ncid,varid, dims))
    call check_net(nf90_close(ncid))
  end subroutine get_var_2D
  subroutine get_var_3D(fname_grid,dimname,dims)
    use netcdf
    implicit none
    character(len=*),intent(in) :: fname_grid,dimname
    real(idx),intent(inout) :: dims(:,:,:)
    integer :: ncid,varid
    call check_net(nf90_open(trim(fname_grid),nf90_nowrite,ncid))
    call check_net(nf90_open(trim(fname_grid),nf90_nowrite,ncid))
    call check_net(nf90_inq_varid(ncid,trim(dimname),varid))
    call check_net(nf90_get_var(ncid,varid, dims))
    call check_net(nf90_close(ncid))
  end subroutine get_var_3D
  subroutine get_var_4D(fname_grid,dimname,dims)
    use netcdf
    implicit none
    character(len=*),intent(in) :: fname_grid,dimname
    real(idx),intent(inout) :: dims(:,:,:,:)
    integer :: ncid,varid
    call check_net(nf90_open(trim(fname_grid),nf90_nowrite,ncid))
    call check_net(nf90_open(trim(fname_grid),nf90_nowrite,ncid))
    call check_net(nf90_inq_varid(ncid,trim(dimname),varid))
    call check_net(nf90_get_var(ncid,varid, dims))
    call check_net(nf90_close(ncid))
  end subroutine get_var_4D
  subroutine get_var_units(fname_in,varname,units)
    use netcdf
    implicit none
    character(len=*),  intent(in) :: fname_in,varname
    character(len=*), intent(out) :: units
    integer :: ncid,varid
    call check_net(nf90_open(trim(fname_in),nf90_nowrite,ncid))
    call check_net(nf90_inq_varid(ncid,varname, varid))
    call check_net(nf90_get_att(ncid, varid, 'units', units) )
  end subroutine get_var_units
  !====================================
  function modify_time(ntime,time_in,time_units,start_yymmdd,start_hhmmss) result(time)
    use calendar_sub
    implicit none
    integer,intent(in) :: ntime
    real(idx),intent(in) :: time_in(ntime)
    character(len=*),intent(in) :: time_units
    integer,intent(in) :: start_yymmdd,start_hhmmss
    real(idx) :: time(ntime)
    character :: flag_char*8,yr_char*4,mn_char*2,dy_char*2,hr_char*2,min_char*2,sec_char
    integer :: ref_year,ref_month,ref_day,ref_hour,ref_min,ref_sec
    integer :: ind1,ind2
    integer :: ref_yymmdd,ref_hhmmss
    integer :: it
    integer :: flag
    real(idx) :: sec_start,sec_tmp
    integer :: tmp_yymmdd,tmp_hhmmss
    ind1=1
    ind2=index(time_units,"since")-2
    flag_char=time_units(ind1:ind2)
    ind1=index(time_units,"since")+6
    ind2=ind1+index(time_units(ind1:),"-")-1
    yr_char=time_units(ind1:ind2-1)
    ind1=ind2+1
    ind2=ind1+index(time_units(ind1:),"-")-1
    mn_char=time_units(ind1:ind2-1)
    ind1=ind2+1
    ind2=ind1+index(time_units(ind1:)," ")-1
    dy_char=time_units(ind1:ind2-1)
    ind1=ind2+1
    ind2=ind1+index(time_units(ind1:),":")-1
    hr_char=time_units(ind1:ind2-1)
    ind1=ind2+1
    ind2=ind1+index(time_units(ind1:),":")-1
    min_char=time_units(ind1:ind2-1)
    ind1=ind2+1
    ind2=len_trim(time_units)
    sec_char=time_units(ind1:ind2)

    read(yr_char,*) ref_year ; read(mn_char,*) ref_month ; read(dy_char,*) ref_day
    ref_yymmdd=ref_year*10000+ref_month*100+ref_day
    read(hr_char,*) ref_hour ; read(min_char,*) ref_min ; read(sec_char,*) ref_sec
    ref_hhmmss=ref_hour*10000+ref_min*100+ref_sec

    select case(trim(flag_char))
    case("seconds")
       flag=-10000
    case("minitues")
       flag=-100
    case("hours")
       flag=-1
    case("days")
       flag=1
    case("months")
       flag=100
    case("years")
       flag=10000
    end select
    do it = 1,ntime
       call calendar_cal_ymdhms_after(ref_yymmdd,ref_hhmmss,time_in(it),flag,tmp_yymmdd,tmp_hhmmss)
       call calendar_cal_length_ymdhms(start_yymmdd,start_hhmmss,tmp_yymmdd,tmp_hhmmss,-10000,time(it))
    end do
  end function modify_time
  subroutine time_wgt(time_array,time,i1,i2,w1,w2)
    real(idx),intent(in) :: time_array(:),time
    integer,intent(out) :: i1,i2    
    integer :: iasm,ntime
    real(idx) :: w1,w2
    ntime=sum(shape(time_array))
    if (ntime .ne. 1) then
       if (time  .gt. minval(time_array) .and. time .lt. maxval(time_array)) then
          i1 = sum(maxloc(time_array,mask=(time_array<=time)))
          i2 = i1 + 1
          w1 = (time_array(i2)-time) / (time_array(i2)-time_array(i1))
          w2 = (time-time_array(i1)) / (time_array(i2)-time_array(i1))
       else if (time .le. minval(time_array)) then
          i1 = 1
          i2 = 1
          w1 = 1.0_idx
          w2 = 0.0_idx
       else
          i1 = ntime
          i2 = ntime
          w1 = 1.0_idx
          w2 = 0.0_idx
       end if
    else
       i1 = 1; i2 =1
       w1 = 1.0_idx ; w2 = 0.0_idx
    end if
  end subroutine time_wgt
  function set_data(ind1,ind2,wgt1,wgt2,data_1d) result(data_ret)
    implicit none
    integer,intent(in) :: ind1,ind2
    real(idx),intent(in) :: wgt1,wgt2
    real(idx),intent(in) :: data_1d(:)
    real(idx) :: data_ret
    data_ret= wgt1*data_1d(ind1)+wgt2*data_1d(ind2)
  end function set_data
  subroutine read_TLL_p(nx,ny,fname,varname,data,time,start_yymmdd,start_hhmmss)
    implicit none
    integer,intent(in) :: nx,ny
    character(len=*),intent(in) :: fname,varname
    real(idx),allocatable,intent(inout) :: data(:,:,:)
    real(idx),allocatable,intent(inout) :: time(:)
    integer,intent(in) :: start_yymmdd,start_hhmmss
    integer :: nt
    real(idx),allocatable :: time_tmp(:)
    character(len=maxlen) :: time_units
    nt=get_dimsize(fname,"time")
    allocate(time_tmp(nt)) ; allocate(time(nt))
    call get_var_1D(fname,"time",time_tmp)
    call get_var_units(fname,"time",time_units)
    time=modify_time(nt,time_tmp,time_units,start_yymmdd,start_hhmmss)
    allocate(data(0:nx+1,0:ny+1,1:nt))
    call get_var_3D(trim(fname),varname,data)
  end subroutine read_TLL_p    
  subroutine read_TLL_u(nx,ny,fname,varname,data,time,start_yymmdd,start_hhmmss)
    implicit none
    integer,intent(in) :: nx,ny
    character(len=*),intent(in) :: fname,varname
    real(idx),allocatable,intent(inout) :: data(:,:,:)
    real(idx),allocatable,intent(inout) :: time(:)
    integer,intent(in) :: start_yymmdd,start_hhmmss
    integer :: nt
    real(idx),allocatable :: time_tmp(:)
    character(len=maxlen) :: time_units
    nt=get_dimsize(fname,"time")
    allocate(time_tmp(nt)) ; allocate(time(nt))
    call get_var_1D(fname,"time",time_tmp)
    call get_var_units(fname,"time",time_units)
    time=modify_time(nt,time_tmp,time_units,start_yymmdd,start_hhmmss)
    allocate(data(1:nx+1,0:ny+1,1:nt))
    call get_var_3D(trim(fname),varname,data)
  end subroutine read_TLL_u
  subroutine read_TLL_v(nx,ny,fname,varname,data,time,start_yymmdd,start_hhmmss)
    use netcdf
    implicit none
    integer,intent(in) :: nx,ny
    character(len=*),intent(in) :: fname,varname
    real(idx),allocatable,intent(inout) :: data(:,:,:)
    real(idx),allocatable,intent(inout) :: time(:)
    integer,intent(in) :: start_yymmdd,start_hhmmss
    integer :: nt
    real(idx),allocatable :: time_tmp(:)
    character(len=maxlen) :: time_units
    nt=get_dimsize(fname,"time")
    allocate(time_tmp(nt)) ; allocate(time(nt))
    call get_var_1D(fname,"time",time_tmp)
    call get_var_units(fname,"time",time_units)
    time=modify_time(nt,time_tmp,time_units,start_yymmdd,start_hhmmss)
    allocate(data(0:nx+1,1:ny+1,1:nt))
    call get_var_3D(trim(fname),varname,data)
  end subroutine read_TLL_v
end module input_files
  ! !====================================
  ! ! Read surface density data (rho/N^2 at the surface) 
  ! !====================================
  ! subroutine read_sdens_data(fname_sdens,sdens_G,time,start_yymmdd,start_hhmmss)
  !   use netcdf
  !   implicit none
  !   character(len=*),intent(in) :: fname_sdens    
  !   real(idx),allocatable,intent(inout) :: sdens_G(:,:,:)
  !   real(idx),allocatable,intent(inout) :: time(:)
  !   integer,intent(in) :: start_yymmdd,start_hhmmss
  !   integer :: nx,ny,nt
  !   integer :: ncid,varid,start(3),count(3)
  !   real(idx),allocatable :: time_tmp(:)
  !   character(len=400) :: time_units
  !   real(idx),parameter :: sec_to_day = 1/ (24.0*60.0*60.0)
  !   real(idx),parameter :: day_to_sec = 60.0*60.0*24.0
  !   nx=get_dimsize(fname_sdens,"lon")-2
  !   ny=get_dimsize(fname_sdens,"lat")-2
  !   nt=get_dimsize(fname_sdens,"time")
  !   allocate(time_tmp(nt)) ; allocate(time(nt))
  !   call get_var_1D(fname_sdens,"time",time_tmp)
  !   call get_var_units(fname_sdens,"time",time_units)
  !   time=modify_time(nt,time_tmp,time_units,start_yymmdd,start_hhmmss)
  !   !time = time * day_to_sec
  !   start = (/1,1,1/)
  !   count = (/nx+2,ny+2,nt/)
  !   allocate(sdens_G(0:nx+1,0:ny+1,1:nt))
  !   call check_net(nf90_open(trim(fname_sdens),nf90_nowrite,ncid))
  !   call check_net(nf90_inq_varid(ncid,"sdens",varid))
  !   call check_net(nf90_get_var(ncid, varid,sdens_G(0:nx+1,0:ny+1,1:nt), start = start,count = count))
  ! end subroutine read_sdens_data
  ! !====================================
  ! ! Read qn data (rho/N^2 at the surface) 
  ! !====================================
  ! subroutine read_qn_data(fname_qn,qn_G,time,start_yymmdd,start_hhmmss)
  !   use netcdf
  !   implicit none
  !   character(len=*),intent(in) :: fname_qn    
  !   real(idx),allocatable,intent(inout) :: qn_G(:,:,:,:)
  !   real(idx),allocatable,intent(inout) :: time(:)
  !   integer,intent(in) :: start_yymmdd,start_hhmmss
  !   integer :: nx,ny,nm,nt
  !   integer :: ncid,varid,start(4),count(4)
  !   real(idx),allocatable :: time_tmp(:)
  !   character(len=400) :: time_units
  !   real(idx),parameter :: sec_to_day = 1/ (24.0*60.0*60.0)
  !   real(idx),parameter :: day_to_sec = 60.0*60.0*24.0
  !   nx=get_dimsize(fname_qn,"lon")-2
  !   ny=get_dimsize(fname_qn,"lat")-2
  !   nm=get_dimsize(fname_qn,"mode")
  !   nt=get_dimsize(fname_qn,"time")
  !   allocate(time_tmp(nt)) ; allocate(time(nt))
  !   call get_var_1D(fname_qn,"time",time_tmp)
  !   call get_var_units(fname_qn,"time",time_units)
  !   time=modify_time(nt,time_tmp,time_units,start_yymmdd,start_hhmmss)
  !   !time = time * day_to_sec
  !   start = (/1,1,1,1/)
  !   count = (/nx+2,ny+2,nt,nm/)
  !   allocate(qn_G(0:nx+1,0:ny+1,nt,1:nm))
  !   call check_net(nf90_open(trim(fname_qn),nf90_nowrite,ncid))
  !   call check_net(nf90_inq_varid(ncid,"qn",varid))
  !   call check_net(nf90_get_var(ncid, varid,qn_G(0:nx+1,0:ny+1,1:nt,1:nm), start = start,count = count))
  ! end subroutine read_qn_data
  ! !====================================
