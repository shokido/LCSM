module input_files
  use run_param
  implicit none
  private
  public :: gen_cgrid
  public :: create_mask
  public :: read_ocn_grid
  public :: read_sdens_data
  public :: read_vm_data_1d,read_vm_data_2d
  public :: read_qn_data
  public :: read_rst_data
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
  !=============================================
  ! C-Grid generation
  !=============================================
  subroutine gen_cgrid(nx,ny,x_p,y_p,x_u,x_v,y_u,y_v)
    implicit none
    integer(idx),intent(in) :: nx,ny
    real(idx),intent(in) :: x_p(0:nx+1),y_p(0:ny+1)
    real(idx),intent(inout),allocatable :: x_u(:),x_v(:)
    real(idx),intent(inout),allocatable :: y_u(:),y_v(:)
    integer :: ix,iy
    allocate(x_u(1:nx+1)) ;  allocate(y_u(0:ny+1))
    allocate(x_v(0:nx+1)) ;  allocate(y_v(1:ny+1))
    ! X-direction
    x_v=x_p
    x_u(1:nx+1) = 0.5_4 * (x_p(0:nx)+x_p(1:nx+1))
    ! y
    y_u=y_p
    y_v(1:ny) = 0.5_4 * (y_p(0:ny)+y_p(1:ny+1))
  end subroutine gen_cgrid
  !=============================================
  ! Create mask
  !=============================================
  subroutine create_mask(nx,ny,mask_p,mask_u,mask_v,mask_phi_u,mask_phi_v,slip)
    implicit none
    integer,intent(in) :: nx,ny
    real(idx),intent(in) :: mask_p(0:nx+1,0:ny+1)
    real(idx),intent(inout) :: mask_u(1:nx+1,0:ny+1),mask_v(0:nx+1,1:ny+1)
    real(idx),intent(inout) :: mask_phi_u(1:nx+1,1:ny+1), mask_phi_v(1:nx+1,1:ny+1)
    real(idx),intent(in) :: slip
    integer :: ix,iy
    mask_u(:,:)=1.0
    mask_v(:,:)=1.0
    mask_phi_u(:,:)=1.0
    mask_phi_v(:,:)=1.0
    iy = 1
    do ix = 2,nx
       mask_u(ix,iy)=mask_p(ix-1,iy)*mask_p(ix,iy)
    end do

    do iy = 2,ny
       ix = 1
       mask_v(ix,iy)=mask_p(ix,iy-1)*mask_p(ix,iy)       
       do ix = 2,nx
          mask_u(ix,iy)=mask_p(ix-1,iy)*mask_p(ix,iy)
          mask_v(ix,iy)=mask_p(ix,iy-1)*mask_p(ix,iy)
          if (mask_p(ix-1,iy-1) == 0.0 .and. mask_p(ix,iy-1)==0.0) then
             mask_phi_u(ix,iy) = slip
          end if
          if (mask_p(ix-1,iy) == 0.0 .and. mask_p(ix,iy)==0.0) then
             mask_phi_u(ix,iy) = slip
          end if
          if (mask_p(ix-1,iy-1) == 0.0 .and. mask_p(ix-1,iy) == 0.0) then
             mask_phi_v(ix,iy) = slip
          end if
          if (mask_p(ix,iy-1) == 0.0 .and. mask_p(ix,iy) == 0.0) then
             mask_phi_v(ix,iy) = slip
          end if
       end do
    end do
  end subroutine create_mask
  subroutine read_ocn_grid(fname,nx,ny,x_p,y_p,x_u,y_u,x_v,y_v,&
       & lon_p,lat_p,lon_u,lat_u,lon_v,lat_v,f,mask_p,&
       & damp_p,damp_u,damp_v)
    implicit none
    character(len=*),intent(in) :: fname
    integer,intent(inout) :: nx,ny
    real(idx),allocatable,intent(inout) :: x_p(:),y_p(:),x_u(:),y_u(:),x_v(:),y_v(:)
    real(idx),allocatable,intent(inout) :: lon_p(:),lat_p(:),lon_u(:),lat_u(:),lon_v(:),lat_v(:)
    real(idx),allocatable,intent(inout) :: f(:)
    real(idx),allocatable,intent(inout) :: mask_p(:,:)
    real(idx),allocatable,intent(inout) :: damp_p(:,:),damp_u(:,:),damp_v(:,:)
    ! Get grid
    ! Grid generation------------------------------------------------
    ! p(0:nx+1,0:ny+1)
    nx=get_dimsize(fname,"x_p")-2
    ny=get_dimsize(fname,"y_p")-2
    allocate(x_p(0:nx+1)) ; allocate(y_p(0:ny+1))
    allocate(x_u(1:nx+1)) ; allocate(y_u(0:ny+1))
    allocate(x_v(0:nx+1)) ; allocate(y_v(1:ny+1))
    allocate(lon_p(0:nx+1)) ;  allocate(lat_p(0:ny+1))
    allocate(lon_u(1:nx+1)) ; allocate(lat_u(0:ny+1))
    allocate(lon_v(0:nx+1)) ; allocate(lat_v(1:ny+1))
    allocate(f(0:ny+1))
    allocate(mask_p(0:nx+1,0:ny+1))
    allocate(damp_p(0:nx+1,0:ny+1))
    allocate(damp_u(1:nx+1,0:ny+1))
    allocate(damp_v(0:nx+1,1:ny+1))
    call get_var_1D(fname,"x_p",x_p(0:nx+1))
    call get_var_1D(fname,"y_p",y_p(0:ny+1))
    call get_var_1D(fname,"x_u",x_u(1:nx+1))
    call get_var_1D(fname,"y_u",y_u(0:ny+1))
    call get_var_1D(fname,"x_v",x_v(0:nx+1))
    call get_var_1D(fname,"y_v",y_v(1:ny+1))
    call get_var_1D(fname,"lon_p",lon_p(0:nx+1))
    call get_var_1D(fname,"lat_p",lat_p(0:ny+1))
    call get_var_1D(fname,"lon_u",lon_u(1:nx+1))
    call get_var_1D(fname,"lat_u",lat_u(0:ny+1))
    call get_var_1D(fname,"lon_v",lon_v(0:nx+1))
    call get_var_1D(fname,"lat_v",lat_v(1:ny+1))
    call get_var_1D(fname,"f",f(0:ny+1))
    call get_var_2D(fname,"mask_p",mask_p(0:nx+1,0:ny+1))
    call get_var_2D(fname,"damp_p",damp_p(0:nx+1,0:ny+1))
    call get_var_2D(fname,"damp_u",damp_u(1:nx+1,0:ny+1))
    call get_var_2D(fname,"damp_v",damp_v(0:nx+1,1:ny+1))
  end subroutine read_ocn_grid
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
  !====================================
  ! Read surface density data (rho/N^2 at the surface) 
  !====================================
  subroutine read_sdens_data(fname_sdens,sdens_G,time,start_yymmdd,start_hhmmss)
    use netcdf
    implicit none
    character(len=*),intent(in) :: fname_sdens    
    real(idx),allocatable,intent(inout) :: sdens_G(:,:,:)
    real(idx),allocatable,intent(inout) :: time(:)
    integer,intent(in) :: start_yymmdd,start_hhmmss
    integer :: nx,ny,nt
    integer :: ncid,varid,start(3),count(3)
    real(idx),allocatable :: time_tmp(:)
    character(len=400) :: time_units
    real(idx),parameter :: sec_to_day = 1/ (24.0*60.0*60.0)
    real(idx),parameter :: day_to_sec = 60.0*60.0*24.0
    nx=get_dimsize(fname_sdens,"lon")-2
    ny=get_dimsize(fname_sdens,"lat")-2
    nt=get_dimsize(fname_sdens,"time")
    allocate(time_tmp(nt)) ; allocate(time(nt))
    call get_var_1D(fname_sdens,"time",time_tmp)
    call get_var_units(fname_sdens,"time",time_units)
    time=modify_time(nt,time_tmp,time_units,start_yymmdd,start_hhmmss)
    !time = time * day_to_sec
    start = (/1,1,1/)
    count = (/nx+2,ny+2,nt/)
    allocate(sdens_G(0:nx+1,0:ny+1,1:nt))
    call check_net(nf90_open(trim(fname_sdens),nf90_nowrite,ncid))
    call check_net(nf90_inq_varid(ncid,"sdens",varid))
    call check_net(nf90_get_var(ncid, varid,sdens_G(0:nx+1,0:ny+1,1:nt), start = start,count = count))
  end subroutine read_sdens_data
  !====================================
  ! Read qn data (rho/N^2 at the surface) 
  !====================================
  subroutine read_qn_data(fname_qn,qn_G,time,start_yymmdd,start_hhmmss)
    use netcdf
    implicit none
    character(len=*),intent(in) :: fname_qn    
    real(idx),allocatable,intent(inout) :: qn_G(:,:,:,:)
    real(idx),allocatable,intent(inout) :: time(:)
    integer,intent(in) :: start_yymmdd,start_hhmmss
    integer :: nx,ny,nm,nt
    integer :: ncid,varid,start(4),count(4)
    real(idx),allocatable :: time_tmp(:)
    character(len=400) :: time_units
    real(idx),parameter :: sec_to_day = 1/ (24.0*60.0*60.0)
    real(idx),parameter :: day_to_sec = 60.0*60.0*24.0
    nx=get_dimsize(fname_qn,"lon")-2
    ny=get_dimsize(fname_qn,"lat")-2
    nm=get_dimsize(fname_qn,"mode")
    nt=get_dimsize(fname_qn,"time")
    allocate(time_tmp(nt)) ; allocate(time(nt))
    call get_var_1D(fname_qn,"time",time_tmp)
    call get_var_units(fname_qn,"time",time_units)
    time=modify_time(nt,time_tmp,time_units,start_yymmdd,start_hhmmss)
    !time = time * day_to_sec
    start = (/1,1,1,1/)
    count = (/nx+2,ny+2,nt,nm/)
    allocate(qn_G(0:nx+1,0:ny+1,nt,1:nm))
    call check_net(nf90_open(trim(fname_qn),nf90_nowrite,ncid))
    call check_net(nf90_inq_varid(ncid,"qn",varid))
    call check_net(nf90_get_var(ncid, varid,qn_G(0:nx+1,0:ny+1,1:nt,1:nm), start = start,count = count))
  end subroutine read_qn_data
  !====================================
  !====================================
  subroutine read_vm_data_1d(fname_strf,nm,cn,bn,hn)
    implicit none
    character(len=*),intent(in) :: fname_strf
    integer,intent(inout) :: nm
    real(idx),allocatable,intent(inout) :: cn(:),bn(:),hn(:)
    nm=get_dimsize(fname_strf,"mode")
    allocate(cn(1:nm))
    call get_var_1D(fname_strf,"cn",cn(1:nm))
    allocate(bn(1:nm))
    call get_var_1D(fname_strf,"obn",bn(1:nm))
    allocate(hn(1:nm))
    call get_var_1D(fname_strf,"ohn",hn(1:nm))
  end subroutine read_vm_data_1d
  !===============================
  subroutine read_vm_data_2d(fname_strf,nm,bn,hn)
    use netcdf
    implicit none
    character(len=*),intent(in) :: fname_strf
    integer,intent(inout) :: nm
    real(idx),allocatable,intent(inout) :: bn(:,:,:),hn(:,:,:)
    real(idx),allocatable :: tmp_array(:,:,:)
    integer :: nx,ny
    integer :: ncid,varid,start(3),count(3)
    integer :: im
    nx=get_dimsize(fname_strf,"lon")-2
    ny=get_dimsize(fname_strf,"lat")-2
    nm=get_dimsize(fname_strf,"mode")
    start = (/1,1,1/)
    count = (/nx+2,ny+2,nm/)
    allocate(tmp_array(0:nx+1,0:ny+1,1:nm))
    call check_net(nf90_open(trim(fname_strf),nf90_nowrite,ncid))
    !
    !
    allocate(bn(1:nm,0:nx+1,0:ny+1))
    call check_net(nf90_inq_varid(ncid,"obn",varid))
    call check_net(nf90_get_var(ncid, varid,tmp_array(0:nx+1,0:ny+1,1:nm), start = start,count = count))
    do im = 1,nm
       bn(im,0:nx+1,0:ny+1)=tmp_array(0:nx+1,0:ny+1,im)
    end do
    !
    allocate(hn(1:nm,0:nx+1,0:ny+1))
    call check_net(nf90_inq_varid(ncid,"ohn",varid))
    call check_net(nf90_get_var(ncid, varid,tmp_array(0:nx+1,0:ny+1,1:nm), start = start,count = count))
    do im = 1,nm
       hn(im,0:nx+1,0:ny+1)=tmp_array(0:nx+1,0:ny+1,im)
    end do
  end subroutine read_vm_data_2d
  !===============================
  ! Read restart file
  !===============================
  subroutine read_rst_data(fname_rst,nx,ny,nm,u,v,p,u_past,v_past,p_past,&
       & u_next,v_next,p_next)
    use netcdf
    implicit none
    character(len=*),intent(in) :: fname_rst
    integer,intent(in) :: nx,ny,nm
    real(idx),intent(inout),allocatable :: u(:,:,:),v(:,:,:),p(:,:,:)
    real(idx),intent(inout),allocatable :: u_past(:,:,:),v_past(:,:,:),p_past(:,:,:)
    real(idx),intent(inout),allocatable :: u_next(:,:,:),v_next(:,:,:),p_next(:,:,:)
    real(idx),allocatable :: u_tmp(:,:,:),v_tmp(:,:,:),p_tmp(:,:,:)
    integer :: im,iy,ix
    integer :: ncid,varid,start(3),count(3)
    character :: ichar*2,vname*10
    allocate(u(1:nm,1:nx+1,0:ny+1)); allocate(v(1:nm,0:nx+1,1:nx+1))
    allocate(p(1:nm,0:nx+1,0:ny+1))
    allocate(u_past(1:nm,1:nx+1,0:ny+1)); allocate(v_past(1:nm,0:nx+1,1:nx+1))
    allocate(p_past(1:nm,0:nx+1,0:ny+1))
    allocate(u_next(1:nm,1:nx+1,0:ny+1)); allocate(v_next(1:nm,0:nx+1,1:nx+1))
    allocate(p_next(1:nm,0:nx+1,0:ny+1))
    allocate(u_tmp(1:nx+1,0:ny+1,1))
    allocate(v_tmp(0:nx+1,1:ny+1,1))
    allocate(p_tmp(0:nx+1,0:ny+1,1))
    call check_net(nf90_open(trim(fname_rst),nf90_nowrite,ncid))
    do im = 1,nm
       start = (/1,1,im/)
       count = (/nx+1,ny+2,1/)
       vname="u"
       call check_net(nf90_inq_varid(ncid,trim(vname),varid))
       call check_net(nf90_get_var(ncid, varid,u_tmp(1:nx+1,0:ny+1,1:1), start = start,count = count))
       do iy =0,ny+1
          do ix = 1,nx+1
             u(im,ix,iy)=u_tmp(ix,iy,1)
          end do
       end do
       vname="u_past"
       call check_net(nf90_inq_varid(ncid,trim(vname),varid))
       call check_net(nf90_get_var(ncid, varid,u_tmp(1:nx+1,0:ny+1,1:1), start = start,count = count))
       do iy =0,ny+1
          do ix = 1,nx+1
             u_past(im,ix,iy)=u_tmp(ix,iy,1)
          end do
       end do
       count = (/nx+2,ny+1,1/)
       vname="v"
       call check_net(nf90_inq_varid(ncid,trim(vname),varid))
       call check_net(nf90_get_var(ncid, varid,v_tmp(0:nx+1,1:ny+1,1:1), start = start,count = count))
       do iy =1,ny+1
          do ix = 0,nx+1
             v(im,ix,iy)=v_tmp(ix,iy,1)
          end do
       end do
       vname="v_past"
       call check_net(nf90_inq_varid(ncid,trim(vname),varid))
       call check_net(nf90_get_var(ncid, varid,v_tmp(0:nx+1,1:ny+1,1:1), start = start,count = count))
       do iy =1,ny+1
          do ix = 0,nx+1
             v_past(im,ix,iy)=v_tmp(ix,iy,1)
          end do
       end do
       count = (/nx+2,ny+2,1/)
       vname="p"
       call check_net(nf90_inq_varid(ncid,trim(vname),varid))
       call check_net(nf90_get_var(ncid, varid,p_tmp(0:nx+1,0:ny+1,1:1), start = start,count = count))
       do iy =0,ny+1
          do ix = 0,nx+1
             p(im,ix,iy)=p_tmp(ix,iy,1)
          end do
       end do
       vname="p_past"
       call check_net(nf90_inq_varid(ncid,trim(vname),varid))
       call check_net(nf90_get_var(ncid, varid,p_tmp(0:nx+1,0:ny+1,1:1), start = start,count = count))
       do iy =0,ny+1
          do ix = 0,nx+1
             p_past(im,ix,iy)=p_tmp(ix,iy,1)
          end do
       end do     
    end do
    call check_net(nf90_close(ncid))
    deallocate(p_tmp)
    deallocate(v_tmp)
    deallocate(u_tmp)
  end subroutine read_rst_data
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
    use netcdf
    implicit none
    integer,intent(in) :: nx,ny
    character(len=*),intent(in) :: fname,varname
    real(idx),allocatable,intent(inout) :: data(:,:,:)
    real(idx),allocatable,intent(inout) :: time(:)
    integer,intent(in) :: start_yymmdd,start_hhmmss
    integer :: nt
    integer :: ncid,varid,start(3),count(3)
    real(idx),allocatable :: time_tmp(:)
    character(len=maxlen) :: time_units
    nt=get_dimsize(fname,"time")
    allocate(time_tmp(nt)) ; allocate(time(nt))
    call get_var_1D(fname,"time",time_tmp)
    call get_var_units(fname,"time",time_units)
    time=modify_time(nt,time_tmp,time_units,start_yymmdd,start_hhmmss)
    start = (/1,1,1/)
    count = (/nx+2,ny+2,nt/)
    allocate(data(0:nx+1,0:ny+1,1:nt))
    call check_net(nf90_open(trim(fname),nf90_nowrite,ncid))
    call check_net(nf90_inq_varid(ncid,varname,varid))
    call check_net(nf90_get_var(ncid, varid,data(0:nx+1,0:ny+1,1:nt), start = start,count = count))
  end subroutine read_TLL_p
  subroutine read_TLL_u(nx,ny,fname,varname,data,time,start_yymmdd,start_hhmmss)
    use netcdf
    implicit none
    integer,intent(in) :: nx,ny
    character(len=*),intent(in) :: fname,varname
    real(idx),allocatable,intent(inout) :: data(:,:,:)
    real(idx),allocatable,intent(inout) :: time(:)
    integer,intent(in) :: start_yymmdd,start_hhmmss
    integer :: nt
    integer :: ncid,varid,start(3),count(3)
    real(idx),allocatable :: time_tmp(:)
    character(len=maxlen) :: time_units
    nt=get_dimsize(fname,"time")
    allocate(time_tmp(nt)) ; allocate(time(nt))
    call get_var_1D(fname,"time",time_tmp)
    call get_var_units(fname,"time",time_units)
    time=modify_time(nt,time_tmp,time_units,start_yymmdd,start_hhmmss)
    start = (/1,1,1/)
    count = (/nx+1,ny+2,nt/)
    allocate(data(1:nx+1,0:ny+1,1:nt))
    call check_net(nf90_open(trim(fname),nf90_nowrite,ncid))
    call check_net(nf90_inq_varid(ncid,varname,varid))
    call check_net(nf90_get_var(ncid, varid,data(1:nx+1,0:ny+1,1:nt), start = start,count = count))
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
    integer :: ncid,varid,start(3),count(3)
    real(idx),allocatable :: time_tmp(:)
    character(len=maxlen) :: time_units
    nt=get_dimsize(fname,"time")
    allocate(time_tmp(nt)) ; allocate(time(nt))
    call get_var_1D(fname,"time",time_tmp)
    call get_var_units(fname,"time",time_units)
    time=modify_time(nt,time_tmp,time_units,start_yymmdd,start_hhmmss)
    start = (/1,1,1/)
    count = (/nx+2,ny+1,nt/)
    allocate(data(0:nx+1,1:ny+1,1:nt))
    call check_net(nf90_open(trim(fname),nf90_nowrite,ncid))
    call check_net(nf90_inq_varid(ncid,varname,varid))
    call check_net(nf90_get_var(ncid, varid,data(0:nx+1,1:ny+1,1:nt), start = start,count = count))
  end subroutine read_TLL_v
end module input_files
