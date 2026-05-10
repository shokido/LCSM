module ncdf_read
  ! Compile method
  !  ifort -c ncdf_read.f90 -I/usr/local/netcdf4/include -L/usr/local/netcdf4/lib/ -lnetcdf -lnetcdff
  !  ar rcv /Users/kido/fortran/library_ifort/libncdf_read.a ncdf_read.o
  !  ranlib /Users/kido/fortran/library_ifort/libncdf_read.a
  implicit none
  private
  !================================================================
  ! subroutine get_dimension(ncid, name, dim, dims)
  interface get_dimension
     module procedure get_dimension_i,get_dimension_f,get_dimension_d
  end interface get_dimension
  interface select_array_1D
     module procedure select_array_1D_i,select_array_1D_f,select_array_1D_d
  end interface select_array_1D
  interface get_attribute
     module procedure get_attribute_c,get_attribute_f,get_attribute_d
  end interface get_attribute
  interface get_variable
     module procedure    get_variable_1D_wr_i,get_variable_1D_wr_f,get_variable_1D_wr_d,&
          & get_variable_2D_wr_i,get_variable_2D_wr_f,get_variable_2D_wr_d,&
          & get_variable_3D_wr_i,get_variable_3D_wr_f,get_variable_3D_wr_d,&
          & get_variable_4D_wr_i,get_variable_4D_wr_f,get_variable_4D_wr_d,&
          & get_variable_5D_wr_i,get_variable_5D_wr_f,get_variable_5D_wr_d
  end interface get_variable
  interface get_variable_TLL
     module procedure get_variable_TLL_f,get_variable_TLL_d
  end interface get_variable_TLL
  interface get_variable_TLLL
     module procedure get_variable_TLLL_f,get_variable_TLLL_d
  end interface get_variable_TLLL
  public :: check_r,get_dimsize,get_dimension,select_array_1D
  public :: get_attribute
  public :: get_variable
  public :: get_variable_TLL, get_variable_TLLL
contains
  !==============================!  
  ! Check_R subroutine           !
  !==============================!  
  subroutine check_r(status)
    use netcdf
    implicit none
    integer, intent ( in) :: status
   if(status /= nf90_noerr) then 
       print *, trim(nf90_strerror(status))
       stop "Stopped"
    end if
  end subroutine check_r
  !=============================================!  
  ! Subroutine for get dimension size           !
  ! get_dimsize(ncid,name,ndim)                 !
  !=============================================!  
  subroutine get_dimsize(fname,name, ndim)
    use netcdf
    implicit none    
    character(len=*),  intent(in) :: fname
    character(len=*),  intent(in) :: name
    integer,           intent(inout) :: ndim
    integer :: ncid,dimid
    call check_r(nf90_open(trim(fname),nf90_nowrite,ncid))    
    call check_r(nf90_inq_dimid(ncid, name, dimid) )
    call check_r(nf90_inquire_dimension(ncid, dimid, len=ndim) )
    call check_r(nf90_close(ncid))    
  end subroutine get_dimsize
  !=============================================!  
  ! Subroutine for get dimension size           !
  ! get_dimsize(ncid,name,ndim)                 !
  !=============================================!  
  subroutine get_ndim(ncid, name, ndim)
    use netcdf
    implicit none
    integer,           intent(in) :: ncid
    character(len=*),  intent(in) :: name
    integer,           intent(inout) :: ndim
    integer :: dimid
    call check_r(nf90_inq_dimid(ncid, name, dimid) )
    call check_r(nf90_inquire_dimension(ncid, dimid, len=ndim) )
  end subroutine get_ndim
  !=============================================!  
  ! Subroutine for get dimension variable       !
  ! get_ndim(ncid,name,ndim,dims)            !
  !=============================================!  
  subroutine get_dimension_i(ncid, name, ndim, dims)
    use netcdf
    implicit none
    integer,           intent(in) :: ncid
    character(len=*),  intent(in) :: name
    integer,           intent(inout) :: ndim
    integer, allocatable, intent(inout) :: dims(:)
    integer :: err
    integer :: varid, dimid
    call check_r(nf90_inq_dimid(ncid, name, dimid) )
    call check_r(nf90_inquire_dimension(ncid, dimid, len=ndim) )
    allocate(dims(ndim), stat=err)
    if (err /= 0) print *, name, ": Allocation request denied in get_dimension_i"
    call check_r( nf90_inq_varid(ncid, name, varid) )
    call check_r( nf90_get_var(ncid, varid, dims) )
  end subroutine get_dimension_i
   subroutine get_dimension_f(ncid, name, ndim, dims)
    use netcdf
    implicit none
    integer,parameter :: idx=4
    integer,           intent(in) :: ncid
    character(len=*),  intent(in) :: name
    integer,           intent(inout) :: ndim
    real(idx), allocatable, intent(inout) :: dims(:)
    integer :: err
    integer :: varid, dimid
    call check_r(nf90_inq_dimid(ncid, name, dimid) )
    call check_r(nf90_inquire_dimension(ncid, dimid, len=ndim) )
    allocate(dims(ndim), stat=err)
    if (err /= 0) print *, name, ": Allocation request denied in get_dimension_f"
    call check_r( nf90_inq_varid(ncid, name, varid))
    call check_r( nf90_get_var(ncid, varid, dims))
  end subroutine get_dimension_f
   subroutine get_dimension_d(ncid, name, ndim, dims)
    use netcdf
    implicit none
    integer,parameter :: idx=8
    integer,           intent(in) :: ncid
    character(len=*),  intent(in) :: name
    integer,           intent(inout) :: ndim
    real(idx), allocatable, intent(inout) :: dims(:)
    integer :: err
    integer :: varid, dimid
    call check_r(nf90_inq_dimid(ncid, name, dimid) )
    call check_r(nf90_inquire_dimension(ncid, dimid, len=ndim) )
    allocate(dims(ndim), stat=err)
    if (err /= 0) print *, name, ": Allocation request denied in get_dimension_d"
    call check_r( nf90_inq_varid(ncid, name, varid))
    call check_r( nf90_get_var(ncid, varid, dims))
  end subroutine get_dimension_d
  !=============================================!  
  ! Subroutine for get attribute                !
  ! get_attribute(fname,varname,varatt,atts)    !
  !=============================================!  
  subroutine get_attribute_c(fname,varname,varatt,atts)
    use netcdf
    implicit none
    integer,parameter :: maxlen=400
    character(len=*),  intent(in) :: fname,varname,varatt
    character(len=*),  intent(inout) :: atts
    integer :: ncid
    integer :: err
    integer :: varid,dim1id
    integer :: start(1), count(1)
    ! Open files
    call check_r( nf90_open(trim(fname),nf90_nowrite,ncid))
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    if ( nf90_get_att(ncid, varid,varatt,atts) == 0 ) then
       call check_r(nf90_get_att(ncid, varid,varatt,atts))
    else
       atts="None"
    end if
    ! ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_attribute_c
  subroutine get_attribute_f(fname,varname,varatt,atts)
    use netcdf
    implicit none
    integer,parameter :: idx=4,maxlen=400
    character(len=*),  intent(in) :: fname,varname,varatt
    real(idx),  intent(inout) :: atts
    integer :: ncid
    integer :: err
    integer :: varid,dim1id
    integer :: start(1), count(1)
    ! Open files
    call check_r( nf90_open(trim(fname),nf90_nowrite,ncid))
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    if ( nf90_get_att(ncid, varid,varatt,atts) == 0 ) then
       call check_r(nf90_get_att(ncid, varid,varatt,atts))
    else
       atts=0.0_idx
    end if
    ! ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_attribute_f
  subroutine get_attribute_i(fname,varname,varatt,atts)
    use netcdf
    implicit none
    integer,parameter :: idx=4,maxlen=400
    character(len=*),  intent(in) :: fname,varname,varatt
    integer,  intent(inout) :: atts
    integer :: ncid
    integer :: err
    integer :: varid,dim1id
    integer :: start(1), count(1)
    ! Open files
    call check_r( nf90_open(trim(fname),nf90_nowrite,ncid))
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    if ( nf90_get_att(ncid, varid,varatt,atts) == 0 ) then
       call check_r(nf90_get_att(ncid, varid,varatt,atts))
    else
       atts=0.0_idx
    end if
    ! ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_attribute_i
  subroutine get_attribute_d(fname,varname,varatt,atts)
    use netcdf
    implicit none
    integer,parameter :: idx=8,maxlen=400
    character(len=*),  intent(in) :: fname,varname,varatt
    real(idx),  intent(inout) :: atts
    integer :: ncid
    integer :: err
    integer :: varid,dim1id
    integer :: start(1), count(1)
    ! Open files
    call check_r( nf90_open(trim(fname),nf90_nowrite,ncid))
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    if ( nf90_get_att(ncid, varid,varatt,atts) == 0 ) then
       call check_r(nf90_get_att(ncid, varid,varatt,atts))
    else
       atts=0.0_idx
    end if
    ! ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_attribute_d

  !------------------------------------------------------------------------
  subroutine select_array_1D_i(ori_N,ori_data,data_min,data_max,new_N,new_data,min_i,max_i)
    implicit none
    integer,intent(in) :: ori_N
    integer,intent(in) :: ori_data(ori_N)
    integer,intent(in) :: data_min,data_max
    integer,intent(inout) :: new_N
    integer,intent(inout) :: min_i,max_i
    integer,intent(inout),allocatable :: new_data(:)
    logical :: m(ori_N)
    integer,allocatable :: indices(:)
    integer :: ix,ixt
    if (maxval(ori_data) < data_min) then
       write(*,*) "Input minval is too large!"
       new_N=1
       allocate(new_data(new_N))
       new_data(1)=maxval(ori_data)
       min_i = minval(minloc(ori_data,mask=(ori_data==maxval(ori_data))))
       max_i = min_i
    else if (minval(ori_data) > data_max) then
       write(*,*) "Input maxval is too small!"
       new_N=1
       allocate(new_data(new_N))
       new_data(1)=minval(ori_data)
       min_i = minval(minloc(ori_data,mask=(ori_data==minval(ori_data))))
       max_i = min_i
    else
       ! Condition
       m=(ori_data .ge. data_min .and. ori_data .le. data_max)       
       new_N=count(m)
       allocate(new_data(new_N)) ; allocate(indices(new_N))
       ixt=1
       do ix=1,ori_N
          if (m(ix) .eqv. .TRUE.) then
             indices(ixt)=ix
             new_data(ixt)=ori_data(ix)
             ixt=ixt+1
          end if
       end do
       min_i=minval(indices); max_i=maxval(indices)
       deallocate(indices)
    end if
  end subroutine select_array_1D_i

  subroutine select_array_1D_f(ori_N,ori_data,data_min,data_max,new_N,new_data,min_i,max_i)
    implicit none
    integer,parameter :: idx=4
    integer,intent(in) :: ori_N
    real(idx),intent(in) :: ori_data(ori_N)
    real(idx),intent(in) :: data_min,data_max
    integer,intent(inout) :: new_N
    integer,intent(inout) :: min_i,max_i
    real(idx),intent(inout),allocatable :: new_data(:)
    logical :: m(ori_N)
    integer,allocatable :: indices(:)
    integer :: ix,ixt

    if (maxval(ori_data) < data_min) then
       write(*,*) "Input minval is too large!"
       new_N=1
       allocate(new_data(new_N))
       new_data(1)=maxval(ori_data)
       min_i = minval(minloc(ori_data,mask=(ori_data==maxval(ori_data))))
       max_i = min_i
    else if (minval(ori_data) > data_max) then
       write(*,*) "Input maxval is too small!"
       new_N=1
       allocate(new_data(new_N))
       new_data(1)=minval(ori_data)
       min_i = minval(minloc(ori_data,mask=(ori_data==minval(ori_data))))
       max_i = min_i
    else
       ! Condition
       m=(ori_data .ge. data_min .and. ori_data .le. data_max)       
       new_N=count(m)
       allocate(new_data(new_N)) ; allocate(indices(new_N))
       ixt=1
       do ix=1,ori_N
          if (m(ix) .eqv. .TRUE.) then
             indices(ixt)=ix
             new_data(ixt)=ori_data(ix)
             ixt=ixt+1
          end if
       end do
       min_i=minval(indices); max_i=maxval(indices)
       deallocate(indices)
    end if
  end subroutine select_array_1D_f
  subroutine select_array_1D_d(ori_N,ori_data,data_min,data_max,new_N,new_data,min_i,max_i)
    implicit none
    integer,parameter :: idx=8
    integer,intent(in) :: ori_N
    real(idx),intent(in) :: ori_data(ori_N)
    real(idx),intent(in) :: data_min,data_max
    integer,intent(inout) :: new_N
    integer,intent(inout) :: min_i,max_i
    real(idx),intent(inout),allocatable :: new_data(:)
    logical :: m(ori_N)
    integer,allocatable :: indices(:)
    integer :: ix,ixt

    if (maxval(ori_data) < data_min) then
       write(*,*) "Input minval is too large!"
       new_N=1
       allocate(new_data(new_N))
       new_data(1)=maxval(ori_data)
       min_i = minval(minloc(ori_data,mask=(ori_data==maxval(ori_data))))
       max_i = min_i
    else if (minval(ori_data) > data_max) then
       write(*,*) "Input maxval is too small!"
       new_N=1
       allocate(new_data(new_N))
       new_data(1)=minval(ori_data)
       min_i = minval(minloc(ori_data,mask=(ori_data==minval(ori_data))))
       max_i = min_i
    else
       ! Condition
       m=(ori_data .ge. data_min .and. ori_data .le. data_max)       
       new_N=count(m)
       allocate(new_data(new_N)) ; allocate(indices(new_N))
       ixt=1
       do ix=1,ori_N
          if (m(ix) .eqv. .TRUE.) then
             indices(ixt)=ix
             new_data(ixt)=ori_data(ix)
             ixt=ixt+1
          end if
       end do
       min_i=minval(indices); max_i=maxval(indices)
       deallocate(indices)
    end if
  end subroutine select_array_1D_d
  !=============================================!  
  ! Subroutine for get array                    !
  ! get_attribute(fname,varname,varatt,atts)    !
  !=============================================!  
  ! 1-D                                         !
  !=============================================!  
  subroutine get_variable_1D_wr_i(fname,varname,istr,iend,vars)
    use netcdf
    implicit none
    integer,parameter :: maxlen=400,ndim=1
    character(len=*),  intent(in) :: fname,varname
    integer,intent(in) :: istr(ndim),iend(ndim)
    integer, allocatable, intent(inout) :: vars(:)
    integer :: ncid,varid,err,i
    integer :: scale_factor,add_offset
    integer :: count_array(ndim)
    ! Open files
    call check_r(nf90_open(trim(fname),nf90_nowrite,ncid))    
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    do i=1,ndim
       count_array(i)=iend(i)-istr(i)+1
    end do

    ! Allocate data
    allocate(vars(count_array(1)), stat=err)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = istr, count = count_array))
    if (nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if (nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_variable_1D_wr_i
  subroutine get_variable_1D_wr_f(fname,varname,istr,iend,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=4,maxlen=400,ndim=1
    character(len=*),  intent(in) :: fname,varname
    integer,intent(in) :: istr(ndim),iend(ndim)
    real(idx), allocatable, intent(inout) :: vars(:)
    integer :: ncid,varid,err,i
    real(idx) :: scale_factor,add_offset
    integer :: count_array(ndim)
    ! Open files
    call check_r(nf90_open(trim(fname),nf90_nowrite,ncid))    
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    do i=1,ndim
       count_array(i)=iend(i)-istr(i)+1
    end do

    ! Allocate data
    allocate(vars(count_array(1)), stat=err)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = istr, count = count_array))
    if (nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if (nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_variable_1D_wr_f
  subroutine get_variable_1D_wr_d(fname,varname,istr,iend,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=8,maxlen=400,ndim=1
    character(len=*),  intent(in) :: fname,varname
    integer,intent(in) :: istr(ndim),iend(ndim)
    real(idx), allocatable, intent(inout) :: vars(:)
    integer :: ncid,varid,err,i
    real(idx) :: scale_factor,add_offset
    integer :: count_array(ndim)
    ! Open files
    call check_r(nf90_open(trim(fname),nf90_nowrite,ncid))    
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    do i=1,ndim
       count_array(i)=iend(i)-istr(i)+1
    end do

    ! Allocate data
    allocate(vars(count_array(1)), stat=err)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = istr, count = count_array))
    if (nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if (nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_variable_1D_wr_d

  !=============================================!  
  ! 2-D                                         !
  !=============================================!  
  subroutine get_variable_2d_wr_i(fname,varname,istr,iend,vars)
    use netcdf
    implicit none
    integer,parameter :: maxlen=400,ndim=2
    character(len=*),  intent(in) :: fname,varname
    integer,intent(in) :: istr(ndim),iend(ndim)
    integer, allocatable, intent(inout) :: vars(:,:)
    integer :: ncid,varid,err,i
    integer :: scale_factor,add_offset
    integer :: count_array(ndim)
    ! Open files
    call check_r(nf90_open(trim(fname),nf90_nowrite,ncid))    
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    do i=1,ndim
       count_array(i)=iend(i)-istr(i)+1
    end do

    ! Allocate data
    allocate(vars(count_array(1),count_array(2)), stat=err)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = istr, count = count_array))
    if (nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if (nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_variable_2d_wr_i
  subroutine get_variable_2d_wr_f(fname,varname,istr,iend,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=4,maxlen=400,ndim=2
    character(len=*),  intent(in) :: fname,varname
    integer,intent(in) :: istr(ndim),iend(ndim)
    real(idx), allocatable, intent(inout) :: vars(:,:)
    integer :: ncid,varid,err,i
    real(idx) :: scale_factor,add_offset
    integer :: count_array(ndim)
    ! Open files
    call check_r(nf90_open(trim(fname),nf90_nowrite,ncid))    
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    do i=1,ndim
       count_array(i)=iend(i)-istr(i)+1
    end do

    ! Allocate data
    allocate(vars(count_array(1),count_array(2)), stat=err)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = istr, count = count_array))
    if (nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if (nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_variable_2d_wr_f
  subroutine get_variable_2d_wr_d(fname,varname,istr,iend,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=8,maxlen=400,ndim=2
    character(len=*),  intent(in) :: fname,varname
    integer,intent(in) :: istr(ndim),iend(ndim)
    real(idx), allocatable, intent(inout) :: vars(:,:)
    integer :: ncid,varid,err,i
    real(idx) :: scale_factor,add_offset
    integer :: count_array(ndim)
    ! Open files
    call check_r(nf90_open(trim(fname),nf90_nowrite,ncid))    
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    do i=1,ndim
       count_array(i)=iend(i)-istr(i)+1
    end do

    ! Allocate data
    allocate(vars(count_array(1),count_array(2)), stat=err)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = istr, count = count_array))
    if (nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if (nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_variable_2d_wr_d
  !=============================================!  
  ! 3-D                                         !
  !=============================================!  
  subroutine get_variable_3D_wr_i(fname,varname,istr,iend,vars)
    use netcdf
    implicit none
    integer,parameter :: maxlen=400,ndim=3
    character(len=*),  intent(in) :: fname,varname
    integer,intent(in) :: istr(ndim),iend(ndim)
    integer, allocatable, intent(inout) :: vars(:,:,:)
    integer :: ncid,varid,err,i
    integer :: scale_factor,add_offset
    integer :: count_array(ndim)
    ! Open files
    call check_r(nf90_open(trim(fname),nf90_nowrite,ncid))    
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    do i=1,ndim
       count_array(i)=iend(i)-istr(i)+1
    end do

    ! Allocate data
    allocate(vars(count_array(1),count_array(2),count_array(3)), stat=err)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = istr, count = count_array))
    if (nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if (nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_variable_3D_wr_i
  subroutine get_variable_3D_wr_f(fname,varname,istr,iend,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=4,maxlen=400,ndim=3
    character(len=*),  intent(in) :: fname,varname
    integer,intent(in) :: istr(ndim),iend(ndim)
    real(idx), allocatable, intent(inout) :: vars(:,:,:)
    integer :: ncid,varid,err,i
    real(idx) :: scale_factor,add_offset
    integer :: count_array(ndim)
    ! Open files
    call check_r(nf90_open(trim(fname),nf90_nowrite,ncid))    
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    do i=1,ndim
       count_array(i)=iend(i)-istr(i)+1
    end do

    ! Allocate data
    allocate(vars(count_array(1),count_array(2),count_array(3)), stat=err)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = istr, count = count_array))
    if (nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if (nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_variable_3D_wr_f
  subroutine get_variable_3D_wr_d(fname,varname,istr,iend,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=8,maxlen=400,ndim=3
    character(len=*),  intent(in) :: fname,varname
    integer,intent(in) :: istr(ndim),iend(ndim)
    real(idx), allocatable, intent(inout) :: vars(:,:,:)
    integer :: ncid,varid,err,i
    real(idx) :: scale_factor,add_offset
    integer :: count_array(ndim)
    ! Open files
    call check_r(nf90_open(trim(fname),nf90_nowrite,ncid))    
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    do i=1,ndim
       count_array(i)=iend(i)-istr(i)+1
    end do

    ! Allocate data
    allocate(vars(count_array(1),count_array(2),count_array(3)), stat=err)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = istr, count = count_array))
    if (nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if (nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_variable_3D_wr_d
  !==================================================!
  ! 4-D                                               !
  !==================================================!
  subroutine get_variable_4D_wr_i(fname,varname,istr,iend,vars)
    use netcdf
    implicit none
    integer,parameter :: maxlen=400,ndim=4
    character(len=*),  intent(in) :: fname,varname
    integer,intent(in) :: istr(ndim),iend(ndim)
    integer, allocatable, intent(inout) :: vars(:,:,:,:)
    integer :: ncid,varid,err,i
    integer :: scale_factor,add_offset
    integer :: count_array(ndim)
    ! Open files
    call check_r(nf90_open(trim(fname),nf90_nowrite,ncid))    
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    do i=1,ndim
       count_array(i)=iend(i)-istr(i)+1
    end do

    ! Allocate data
    allocate(vars(count_array(1),count_array(2),count_array(3),count_array(4)), stat=err)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = istr, count = count_array))
    if (nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if (nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_variable_4D_wr_i
  subroutine get_variable_4D_wr_f(fname,varname,istr,iend,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=4,maxlen=400,ndim=4
    character(len=*),  intent(in) :: fname,varname
    integer,intent(in) :: istr(ndim),iend(ndim)
    real(idx), allocatable, intent(inout) :: vars(:,:,:,:)
    integer :: ncid,varid,err,i
    real(idx) :: scale_factor,add_offset
    integer :: count_array(ndim)
    ! Open files
    call check_r(nf90_open(trim(fname),nf90_nowrite,ncid))    
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    do i=1,ndim
       count_array(i)=iend(i)-istr(i)+1
    end do

    ! Allocate data
    allocate(vars(count_array(1),count_array(2),count_array(3),count_array(4)), stat=err)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = istr, count = count_array))
    if (nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if (nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_variable_4D_wr_f
  subroutine get_variable_4D_wr_d(fname,varname,istr,iend,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=8,maxlen=400,ndim=4
    character(len=*),  intent(in) :: fname,varname
    integer,intent(in) :: istr(ndim),iend(ndim)
    real(idx), allocatable, intent(inout) :: vars(:,:,:,:)
    integer :: ncid,varid,err,i
    real(idx) :: scale_factor,add_offset
    integer :: count_array(ndim)
    ! Open files
    call check_r(nf90_open(trim(fname),nf90_nowrite,ncid))    
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    do i=1,ndim
       count_array(i)=iend(i)-istr(i)+1
    end do

    ! Allocate data
    allocate(vars(count_array(1),count_array(2),count_array(3),count_array(4)), stat=err)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = istr, count = count_array))
    if (nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if (nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_variable_4D_wr_d
  !==================================================!
  ! 5-D                                              !
  !==================================================!
  subroutine get_variable_5D_wr_i(fname,varname,istr,iend,vars)
    use netcdf
    implicit none
    integer,parameter :: maxlen=400,ndim=5
    character(len=*),  intent(in) :: fname,varname
    integer,intent(in) :: istr(ndim),iend(ndim)
    integer, allocatable, intent(inout) :: vars(:,:,:,:,:)
    integer :: ncid,varid,err,i
    integer :: scale_factor,add_offset
    integer :: count_array(ndim)
    ! Open files
    call check_r(nf90_open(trim(fname),nf90_nowrite,ncid))    
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    do i=1,ndim
       count_array(i)=iend(i)-istr(i)+1
    end do

    ! Allocate data
    allocate(vars(count_array(1),count_array(2),count_array(3),count_array(4),count_array(5)), stat=err)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = istr, count = count_array))
    if (nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if (nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_variable_5D_wr_i
  subroutine get_variable_5D_wr_f(fname,varname,istr,iend,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=4,maxlen=400,ndim=5
    character(len=*),  intent(in) :: fname,varname
    integer,intent(in) :: istr(ndim),iend(ndim)
    real(idx), allocatable, intent(inout) :: vars(:,:,:,:,:)
    integer :: ncid,varid,err,i
    real(idx) :: scale_factor,add_offset
    integer :: count_array(ndim)
    ! Open files
    call check_r(nf90_open(trim(fname),nf90_nowrite,ncid))    
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    do i=1,ndim
       count_array(i)=iend(i)-istr(i)+1
    end do

    ! Allocate data
    allocate(vars(count_array(1),count_array(2),count_array(3),count_array(4),count_array(5)), stat=err)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = istr, count = count_array))
    if (nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if (nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_variable_5D_wr_f
  subroutine get_variable_5D_wr_d(fname,varname,istr,iend,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=8,maxlen=400,ndim=5
    character(len=*),  intent(in) :: fname,varname
    integer,intent(in) :: istr(ndim),iend(ndim)
    real(idx), allocatable, intent(inout) :: vars(:,:,:,:,:)
    integer :: ncid,varid,err,i
    real(idx) :: scale_factor,add_offset
    integer :: count_array(ndim)
    ! Open files
    call check_r(nf90_open(trim(fname),nf90_nowrite,ncid))    
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    do i=1,ndim
       count_array(i)=iend(i)-istr(i)+1
    end do

    ! Allocate data
    allocate(vars(count_array(1),count_array(2),count_array(3),count_array(4),count_array(5)), stat=err)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = istr, count = count_array))
    if (nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if (nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_variable_5D_wr_d

  subroutine get_variable_TLL_f(fname,varname,lon,lat,time,var,lonname,latname,timename)
    implicit none
    integer,parameter :: idx=4
    character(len=*),  intent(in) :: fname,varname
    character(len=*),optional :: lonname,latname,timename
    real(idx),allocatable,intent(inout)  :: lon(:),lat(:),time(:),var(:,:,:)
    integer :: nlon,nlat,ntime
    if(.not. present(lonname) ) then
       lonname="lon"
    end if
    if(.not. present(latname) ) then
       latname="lat"
    end if
    if(.not. present(timename) ) then
       timename="time"
    end if
    call get_dimsize(fname,lonname,nlon)
    call get_dimsize(fname,latname,nlat)
    call get_dimsize(fname,timename,ntime)
    allocate(lon(nlon))
    allocate(lat(nlat))
    allocate(time(ntime))
    allocate(var(nlon,nlat,ntime))
    call get_variable(fname,varname,(/1,1,1/),(/nlon,nlat,ntime/),var)
  end subroutine get_variable_TLL_f
  subroutine get_variable_TLL_d(fname,varname,lon,lat,time,var,lonname,latname,timename)
    implicit none
    integer,parameter :: idx=8
    character(len=*),  intent(in) :: fname,varname
    character(len=*),optional :: lonname,latname,timename
    real(idx),allocatable,intent(inout) :: lon(:),lat(:),time(:),var(:,:,:)
    integer :: nlon,nlat,ntime
    if(.not. present(lonname) ) then
       lonname="lon"
    end if
    if(.not. present(latname) ) then
       latname="lat"
    end if
    if(.not. present(timename) ) then
       timename="time"
    end if
    call get_dimsize(fname,lonname,nlon)
    call get_dimsize(fname,latname,nlat)
    call get_dimsize(fname,timename,ntime)
    allocate(lon(nlon))
    allocate(lat(nlat))
    allocate(time(ntime))
    allocate(var(nlon,nlat,ntime))
    call get_variable(fname,varname,(/1,1,1/),(/nlon,nlat,ntime/),var)
  end subroutine get_variable_TLL_d
  subroutine get_variable_TLLL_f(fname,varname,lon,lat,lev,time,var,lonname,latname,levname,timename)
    implicit none
    integer,parameter :: idx=4
    character(len=*),  intent(in) :: fname,varname
    character(len=*),optional :: lonname,latname,levname,timename
    real(idx),allocatable,intent(inout) :: lon(:),lat(:),lev(:),time(:),var(:,:,:,:)
    integer :: nlon,nlat,nlev,ntime
    if(.not. present(lonname) ) then
       lonname="lon"
    end if
    if(.not. present(latname) ) then
       latname="lat"
    end if
    if(.not. present(levname) ) then
       levname="lev"
    end if
    if(.not. present(timename) ) then
       timename="time"
    end if
    call get_dimsize(fname,lonname,nlon)
    call get_dimsize(fname,latname,nlat)
    call get_dimsize(fname,levname,nlev)
    call get_dimsize(fname,timename,ntime)
    allocate(lon(nlon))
    allocate(lat(nlat))
    allocate(lev(nlev))
    allocate(time(ntime))
    allocate(var(nlon,nlat,nlev,ntime))
    call get_variable(fname,varname,(/1,1,1,1/),(/nlon,nlat,nlev,ntime/),var)
  end subroutine get_variable_TLLL_f
  subroutine get_variable_TLLL_d(fname,varname,lon,lat,lev,time,var,lonname,latname,levname,timename)
    implicit none
    integer,parameter :: idx=8
    character(len=*),  intent(in) :: fname,varname
    character(len=*),optional :: lonname,latname,levname,timename
    real(idx),allocatable,intent(inout) :: lon(:),lat(:),lev(:),time(:),var(:,:,:,:)
    integer :: nlon,nlat,nlev,ntime
    if(.not. present(lonname) ) then
       lonname="lon"
    end if
    if(.not. present(latname) ) then
       latname="lat"
    end if
    if(.not. present(levname) ) then
       levname="lev"
    end if
    if(.not. present(timename) ) then
       timename="time"
    end if
    call get_dimsize(fname,lonname,nlon)
    call get_dimsize(fname,latname,nlat)
    call get_dimsize(fname,levname,nlev)
    call get_dimsize(fname,timename,ntime)
    allocate(lon(nlon))
    allocate(lat(nlat))
    allocate(lev(nlev))
    allocate(time(ntime))
    allocate(var(nlon,nlat,nlev,ntime))
    call get_variable(fname,varname,(/1,1,1,1/),(/nlon,nlat,nlev,ntime/),var)
  end subroutine get_variable_TLLL_d
end module ncdf_read
