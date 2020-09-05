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
     module procedure get_dimension,dget_dimension
  end interface get_dimension
  !---------------------------------------------------------------
  ! subroutine get_dimension_units(ncid, name, dim, dims,units)
  interface get_dimension_units
     module procedure get_dimension_units,dget_dimension_units
  end interface get_dimension_units
  !---------------------------------------------------------------
  interface get_variable
     module procedure get_variable_1D,dget_variable_1D,get_variable_1D_nc,dget_variable_1D_nc, &
          & get_variable_2D,dget_variable_2D,get_variable_2D_nc,dget_variable_2D_nc, &
          & get_variable_3D,dget_variable_3D,get_variable_3D_nc,dget_variable_3D_nc,&
          & get_variable_4D,dget_variable_4D,get_variable_4D_nc,dget_variable_4D_nc,&
          & get_variable_1D_wr,dget_variable_1D_wr,&
          & get_variable_2D_wr,dget_variable_2D_wr,&
          & get_variable_3D_wr,dget_variable_3D_wr,&
          & get_variable_4D_wr,dget_variable_4D_wr,&
          & get_variable_5D_wr,dget_variable_5D_wr
  end interface get_variable
  ! subroutine select_array_1D(ori_N,ori_data,data_min,data_max,new_N,new_data,min_i,max_i)
  interface select_array_1D
     module procedure select_array_1D,dselect_array_1D,iselect_array_1D
  end interface select_array_1D
  interface get_attribute
     module procedure get_attribute_c,get_attribute_f,get_attribute_d
  end interface get_attribute

  public :: check_r,get_ndim,get_dimension
  public :: get_variable
  public :: get_attribute
  public :: select_array_1D
contains
  ! Check_R subroutine--------------------------------------------------------------
  subroutine check_r(status)
    use netcdf
    implicit none
    integer, intent ( in) :: status
    if(status /= nf90_noerr) then 
       print *, trim(nf90_strerror(status))
       stop "Stopped"
    end if
  end subroutine check_r
  ! getting dimension
  !===========================================================================================
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
  subroutine get_dimension(ncid, name, dim, dims)
    use netcdf
    implicit none
    integer,parameter :: idx=4    
    integer,           intent(in) :: ncid
    character(len=*),  intent(in) :: name
    integer,           intent(inout) :: dim
    real(idx), allocatable, intent(inout) :: dims(:)
    integer :: err
    integer :: varid, dimid
    call check_r(nf90_inq_dimid(ncid, name, dimid) )
    call check_r(nf90_inquire_dimension(ncid, dimid, len=dim) )
    allocate(dims(dim), stat=err)
    if (err /= 0) print *, name, ": Allocation request denied"
    call check_r( nf90_inq_varid(ncid, name, varid) )
    call check_r( nf90_get_var(ncid, varid, dims) )
  end subroutine get_dimension
  !----------------------------------------------------
  subroutine dget_dimension(ncid, name, dim, dims)
    use netcdf
    implicit none
    integer,parameter :: idx=8
    integer,           intent(in) :: ncid
    character(len=*),  intent(in) :: name
    integer,           intent(inout) :: dim
    real(idx), allocatable, intent(inout) :: dims(:)
    integer :: err
    integer :: varid, dimid
    call check_r(nf90_inq_dimid(ncid, name, dimid) )
    call check_r(nf90_inquire_dimension(ncid, dimid, len=dim) )
    allocate(dims(dim), stat=err)
    if (err /= 0) print *, name, ": Allocation request denied"
    call check_r( nf90_inq_varid(ncid, name, varid) )
    call check_r( nf90_get_var(ncid, varid, dims) )
  end subroutine dget_dimension
  !---------------------------------------------------------
  subroutine get_dimension_units(ncid, name, dim, dims,units)
    use netcdf
    implicit none
    integer,parameter :: idx=4    
    integer,           intent(in) :: ncid
    character(len=*),  intent(in) :: name
    integer,           intent(inout) :: dim
    real(idx), allocatable, intent(inout) :: dims(:)
    character(len=*), intent(out) :: units 
    integer :: err
    integer :: varid, dimid
    call check_r(nf90_inq_dimid(ncid, name, dimid))
    call check_r(nf90_inquire_dimension(ncid, dimid, len=dim) )
    allocate(dims(dim), stat=err)
    if (err /= 0) print *, name, ": Allocation request denied"
    call check_r( nf90_inq_varid(ncid, name, varid) )
    call check_r( nf90_get_var(ncid, varid, dims) )
    call check_r( nf90_get_att(ncid, varid, 'units', units) )
  end subroutine get_dimension_units
  !---------------------------------------------------------
  subroutine dget_dimension_units(ncid, name, dim, dims,units)
    use netcdf
    implicit none
    integer,parameter :: idx=8  
    integer,           intent(in) :: ncid
    character(len=*),  intent(in) :: name
    integer,           intent(inout) :: dim
    real(idx), allocatable, intent(inout) :: dims(:)
    character(len=*), intent(out) :: units 
    integer :: err
    integer :: varid, dimid
    call check_r(nf90_inq_dimid(ncid, name, dimid))
    call check_r(nf90_inquire_dimension(ncid, dimid, len=dim) )
    allocate(dims(dim), stat=err)
    if (err /= 0) print *, name, ": Allocation request denied"
    call check_r( nf90_inq_varid(ncid, name, varid) )
    call check_r( nf90_get_var(ncid, varid, dims) )
    call check_r( nf90_get_att(ncid, varid, 'units', units) )
  end subroutine dget_dimension_units
  !------------------------------------------------------------------------
  subroutine iselect_array_1D(ori_N,ori_data,data_min,data_max,new_N,new_data,min_i,max_i)
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
  end subroutine iselect_array_1D

  subroutine select_array_1D(ori_N,ori_data,data_min,data_max,new_N,new_data,min_i,max_i)
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
  end subroutine select_array_1D

  subroutine dselect_array_1D(ori_N,ori_data,data_min,data_max,new_N,new_data,min_i,max_i)
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
  end subroutine dselect_array_1D
  !---------------------------------------------
  ! Subroutine for get variables
  !---------------------------------------------
  subroutine get_variable_1D(fname,dim1name,varname,dim1unit,varunit,&
       & ndim1,dim1,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=4,maxlen=400
    character(len=*),  intent(in) :: fname
    character(len=*),  intent(in) :: dim1name,varname
    character(len=*),  intent(inout) :: dim1unit,varunit
    integer,intent(inout) :: ndim1
    real(idx), allocatable, intent(inout) :: dim1(:),vars(:)
    integer :: ncid
    integer :: err
    real(idx) :: scale_factor,add_offset
    integer :: varid,dim1id
    integer :: start(1), count(1)
    ! Open files
    call check_r( nf90_open(trim(fname),nf90_nowrite,ncid))
    if (allocated(dim1) .eqv. .true.) then
       deallocate(dim1)
    end if
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    ! Obtain dim1
    call get_dimension(ncid,dim1name,ndim1,dim1)
    ! Allocate data
    allocate(vars(ndim1), stat=err)
    start = (/1/)
    count = (/ndim1/)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = start, &
         count = count) )

    ! Get attributes
    call check_r( nf90_inq_dimid(ncid, dim1name, dim1id) )
    if ( nf90_get_att(ncid, dim1id, 'units',dim1unit) == 0 ) then
       call check_r( nf90_get_att(ncid,dim1id, 'units',dim1unit))
    else
       dim1unit="None"
    end if
    if ( nf90_get_att(ncid, varid, 'units',varunit) == 0 ) then
       call check_r( nf90_get_att(ncid,varid, 'units',varunit))
    else
       varunit="None"
    end if
    if ( nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if ( nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    ! ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_variable_1D
  subroutine dget_variable_1D(fname,dim1name,varname,dim1unit,varunit,&
       & ndim1,dim1,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=8,maxlen=400
    character(len=*),  intent(in) :: fname
    character(len=*),  intent(in) :: dim1name,varname
    character(len=*),  intent(inout) :: dim1unit,varunit
    integer,intent(inout) :: ndim1
    real(idx), allocatable, intent(inout) :: dim1(:),vars(:)
    integer :: ncid
    integer :: err
    real(idx) :: scale_factor,add_offset
    integer :: varid,dim1id
    integer :: start(1), count(1)
    ! Open files
    call check_r( nf90_open(trim(fname),nf90_nowrite,ncid))
    if (allocated(dim1) .eqv. .true.) then
       deallocate(dim1)
    end if
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    ! Obtain dim1
    call get_dimension(ncid,dim1name,ndim1,dim1)
    ! Allocate data
    allocate(vars(ndim1), stat=err)
    start = (/1/)
    count = (/ndim1/)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = start, &
         count = count) )

    ! Get attributes
    call check_r( nf90_inq_dimid(ncid, dim1name, dim1id) )
    if ( nf90_get_att(ncid, dim1id, 'units',dim1unit) == 0 ) then
       call check_r( nf90_get_att(ncid,dim1id, 'units',dim1unit))
    else
       dim1unit="None"
    end if
    if ( nf90_get_att(ncid, varid, 'units',varunit) == 0 ) then
       call check_r( nf90_get_att(ncid,varid, 'units',varunit))
    else
       varunit="None"
    end if
    if ( nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if ( nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    ! ! close file
    call check_r(nf90_close(ncid))
  end subroutine dget_variable_1D
  ! Variable only
  subroutine get_variable_1D_nc(fname,dim1name,varname,ndim1,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=4,maxlen=400
    character(len=*),  intent(in) :: fname
    character(len=*),  intent(in) :: dim1name,varname
    integer,intent(inout) :: ndim1
    real(idx), allocatable, intent(inout) :: vars(:)
    integer :: ncid
    integer :: err
    real(idx) :: scale_factor,add_offset
    integer :: varid,dim1id
    integer :: start(1), count(1)
    ! Open files
    call check_r( nf90_open(trim(fname),nf90_nowrite,ncid))
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    ! Obtain ndim
    call get_ndim(ncid,dim1name,ndim1)
    ! Allocate data
    allocate(vars(ndim1), stat=err)
    start = (/1/)
    count = (/ndim1/)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = start, &
         count = count) )

    if ( nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if ( nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    ! ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_variable_1D_nc
  subroutine dget_variable_1D_nc(fname,dim1name,varname,ndim1,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=8,maxlen=400
    character(len=*),  intent(in) :: fname
    character(len=*),  intent(in) :: dim1name,varname
    integer,intent(inout) :: ndim1
    real(idx), allocatable, intent(inout) :: vars(:)
    integer :: ncid
    integer :: err
    real(idx) :: scale_factor,add_offset
    integer :: varid,dim1id
    integer :: start(1), count(1)
    ! Open files
    call check_r( nf90_open(trim(fname),nf90_nowrite,ncid))
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    ! Obtain ndim
    call get_ndim(ncid,dim1name,ndim1)
    ! Allocate data
    allocate(vars(ndim1), stat=err)
    start = (/1/)
    count = (/ndim1/)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = start, &
         count = count) )

    if ( nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if ( nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    ! ! close file
    call check_r(nf90_close(ncid))
  end subroutine dget_variable_1D_nc
  subroutine get_variable_1D_wr(fname,varname,istr_1,iend_1,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=4,maxlen=400,ndim=1
    character(len=*),  intent(in) :: fname,varname
    integer,intent(in) :: istr_1,iend_1
    real(idx), allocatable, intent(inout) :: vars(:)
    integer :: ndim1
    integer :: ncid,varid
    integer :: err
    real(idx) :: scale_factor,add_offset
    integer :: start(ndim), count(ndim)
    ! Open files
    call check_r(nf90_open(trim(fname),nf90_nowrite,ncid))
    
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    ndim1 = iend_1-istr_1+1

    ! Allocate data
    allocate(vars(ndim1), stat=err)
    start = (/istr_1/)
    count = (/ndim1/)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = start, &
         count = count) )
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
    ! ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_variable_1D_wr
  subroutine dget_variable_1D_wr(fname,varname,istr_1,iend_1,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=8,maxlen=400,ndim=1
    character(len=*),  intent(in) :: fname,varname
    integer,intent(in) :: istr_1,iend_1
    real(idx), allocatable, intent(inout) :: vars(:)
    integer :: ndim1
    integer :: ncid,varid
    integer :: err
    real(idx) :: scale_factor,add_offset
    integer :: start(ndim), count(ndim)
    ! Open files
    call check_r(nf90_open(trim(fname),nf90_nowrite,ncid))
    
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    ndim1 = iend_1-istr_1+1

    ! Allocate data
    allocate(vars(ndim1), stat=err)
    start = (/istr_1/)
    count = (/ndim1/)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = start, &
         count = count) )
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
    ! ! close file
    call check_r(nf90_close(ncid))
  end subroutine dget_variable_1D_wr

  ! 2D
  subroutine get_variable_2D(fname,dim1name,dim2name,varname,dim1unit,dim2unit,varunit,&
       & ndim1,ndim2,dim1,dim2,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=4,maxlen=400
    character(len=*),  intent(in) :: fname
    character(len=*),  intent(in) :: dim1name,dim2name,varname
    character(len=*),  intent(inout) :: dim1unit,dim2unit,varunit
    integer,intent(inout) :: ndim1,ndim2
    real(idx), allocatable, intent(inout) :: dim1(:),dim2(:),vars(:,:)
    integer,allocatable :: temp_cals(:),cals(:)
    integer :: ncid
    integer :: err
    real(idx) :: scale_factor,add_offset
    integer :: varid,dim1id,dim2id
    integer :: start(2), count(2)
    ! Open files
    call check_r( nf90_open(trim(fname),nf90_nowrite,ncid))
    if (allocated(dim1) .eqv. .true.) then
       deallocate(dim1)
    end if
    if (allocated(dim2) .eqv. .true.) then
       deallocate(dim2)
    end if
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    ! Obtain dim1
    call get_dimension(ncid,dim1name,ndim1,dim1)
    ! Obtain dim2
    call get_dimension(ncid,dim2name,ndim2,dim2)
    ! Allocate data
    allocate(vars(ndim1,ndim2), stat=err)
    start = (/1,1/)
    count = (/ndim1,ndim2/)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = start, &
         count = count) )

    ! Get attributes
    call check_r( nf90_inq_dimid(ncid, dim1name, dim1id) )
    if ( nf90_get_att(ncid, dim1id, 'units',dim1unit) == 0 ) then
       call check_r( nf90_get_att(ncid,dim1id, 'units',dim1unit))
    else
       dim1unit="None"
    end if
    call check_r( nf90_inq_dimid(ncid, dim2name, dim2id) )
    if ( nf90_get_att(ncid, dim2id, 'units',dim2unit) == 0 ) then
       call check_r( nf90_get_att(ncid,dim2id, 'units',dim2unit))
    else
       dim2unit="None"
    end if
    if ( nf90_get_att(ncid, varid, 'units',varunit) == 0 ) then
       call check_r( nf90_get_att(ncid,varid, 'units',varunit))
    else
       varunit="None"
    end if
    if ( nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if ( nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    ! ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_variable_2D
    subroutine dget_variable_2D(fname,dim1name,dim2name,varname,dim1unit,dim2unit,varunit,&
       & ndim1,ndim2,dim1,dim2,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=8,maxlen=400
    character(len=*),  intent(in) :: fname
    character(len=*),  intent(in) :: dim1name,dim2name,varname
    character(len=*),  intent(inout) :: dim1unit,dim2unit,varunit
    integer,intent(inout) :: ndim1,ndim2
    real(idx), allocatable, intent(inout) :: dim1(:),dim2(:),vars(:,:)
    integer,allocatable :: temp_cals(:),cals(:)
    integer :: ncid
    integer :: err
    real(idx) :: scale_factor,add_offset
    integer :: varid,dim1id,dim2id
    integer :: start(2), count(2)
    ! Open files
    call check_r( nf90_open(trim(fname),nf90_nowrite,ncid))
    if (allocated(dim1) .eqv. .true.) then
       deallocate(dim1)
    end if
    if (allocated(dim2) .eqv. .true.) then
       deallocate(dim2)
    end if
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    ! Obtain dim1
    call dget_dimension(ncid,dim1name,ndim1,dim1)
    ! Obtain dim2
    call dget_dimension(ncid,dim2name,ndim2,dim2)
    ! Allocate data
    allocate(vars(ndim1,ndim2), stat=err)
    start = (/1,1/)
    count = (/ndim1,ndim2/)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = start, &
         count = count) )

    ! Get attributes
    call check_r( nf90_inq_dimid(ncid, dim1name, dim1id) )
    if ( nf90_get_att(ncid, dim1id, 'units',dim1unit) == 0 ) then
       call check_r( nf90_get_att(ncid,dim1id, 'units',dim1unit))
    else
       dim1unit="None"
    end if
    call check_r( nf90_inq_dimid(ncid, dim2name, dim2id) )
    if ( nf90_get_att(ncid, dim2id, 'units',dim2unit) == 0 ) then
       call check_r( nf90_get_att(ncid,dim2id, 'units',dim2unit))
    else
       dim2unit="None"
    end if
    if ( nf90_get_att(ncid, varid, 'units',varunit) == 0 ) then
       call check_r( nf90_get_att(ncid,varid, 'units',varunit))
    else
       varunit="None"
    end if
    if ( nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if ( nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    ! ! close file
    call check_r(nf90_close(ncid))
  end subroutine dget_variable_2D
  subroutine get_variable_2D_nc(fname,dim1name,dim2name,varname,&
       & ndim1,ndim2,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=4,maxlen=400
    character(len=*),  intent(in) :: fname
    character(len=*),  intent(in) :: dim1name,dim2name,varname
    integer,intent(inout) :: ndim1,ndim2
    real(idx), allocatable, intent(inout) :: vars(:,:)
    integer,allocatable :: temp_cals(:),cals(:)
    integer :: ncid
    integer :: err
    real(idx) :: scale_factor,add_offset
    integer :: varid,dim1id,dim2id
    integer :: start(2), count(2)
    ! Open files
    call check_r( nf90_open(trim(fname),nf90_nowrite,ncid))
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    ! Obtain dim1
    call get_ndim(ncid,dim1name,ndim1)
    ! Obtain dim2
    call get_ndim(ncid,dim2name,ndim2)
    ! Allocate data
    allocate(vars(ndim1,ndim2), stat=err)
    start = (/1,1/)
    count = (/ndim1,ndim2/)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = start, &
         count = count) )

    if ( nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if ( nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    ! ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_variable_2D_nc
  ! Non 
  subroutine dget_variable_2D_nc(fname,dim1name,dim2name,varname,&
       & ndim1,ndim2,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=8,maxlen=400
    character(len=*),  intent(in) :: fname
    character(len=*),  intent(in) :: dim1name,dim2name,varname
    integer,intent(inout) :: ndim1,ndim2
    real(idx), allocatable, intent(inout) :: vars(:,:)
    integer,allocatable :: temp_cals(:),cals(:)
    integer :: ncid
    integer :: err
    real(idx) :: scale_factor,add_offset
    integer :: varid,dim1id,dim2id
    integer :: start(2), count(2)
    ! Open files
    call check_r( nf90_open(trim(fname),nf90_nowrite,ncid))
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    ! Obtain dim1
    call get_ndim(ncid,dim1name,ndim1)
    ! Obtain dim2
    call get_ndim(ncid,dim2name,ndim2)
    ! Allocate data
    allocate(vars(ndim1,ndim2), stat=err)
    start = (/1,1/)
    count = (/ndim1,ndim2/)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = start, &
         count = count) )

    if ( nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if ( nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    ! ! close file
    call check_r(nf90_close(ncid))
  end subroutine dget_variable_2D_nc
  subroutine get_variable_2D_wr(fname,varname,istr_1,iend_1,istr_2,iend_2,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=4,maxlen=400,ndim=2
    character(len=*),  intent(in) :: fname,varname
    integer,intent(in) :: istr_1,iend_1
    integer,intent(in) :: istr_2,iend_2
    real(idx), allocatable, intent(inout) :: vars(:,:)
    integer :: ndim1,ndim2
    integer :: ncid,varid
    integer :: err
    real(idx) :: scale_factor,add_offset
    integer :: start(ndim), count(ndim)
    ! Open files
    call check_r(nf90_open(trim(fname),nf90_nowrite,ncid))
    
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    ndim1 = iend_1-istr_1+1
    ndim2 = iend_2-istr_2+1

    ! Allocate data
    allocate(vars(ndim1,ndim2), stat=err)
    start = (/istr_1,istr_2/)
    count = (/ndim1,ndim2/)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = start, &
         count = count) )
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
    ! ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_variable_2D_wr
    subroutine dget_variable_2D_wr(fname,varname,istr_1,iend_1,istr_2,iend_2,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=8,maxlen=400,ndim=2
    character(len=*),  intent(in) :: fname,varname
    integer,intent(in) :: istr_1,iend_1
    integer,intent(in) :: istr_2,iend_2
    real(idx), allocatable, intent(inout) :: vars(:,:)
    integer :: ndim1,ndim2
    integer :: ncid,varid
    integer :: err
    real(idx) :: scale_factor,add_offset
    integer :: start(ndim), count(ndim)
    ! Open files
    call check_r(nf90_open(trim(fname),nf90_nowrite,ncid))
    
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    ndim1 = iend_1-istr_1+1
    ndim2 = iend_2-istr_2+1

    ! Allocate data
    allocate(vars(ndim1,ndim2), stat=err)
    start = (/istr_1,istr_2/)
    count = (/ndim1,ndim2/)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = start, &
         count = count) )
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
    ! ! close file
    call check_r(nf90_close(ncid))
  end subroutine dget_variable_2D_wr

  ! 3D
  subroutine get_variable_3D(fname,dim1name,dim2name,dim3name,varname,dim1unit,dim2unit,dim3unit,varunit,&
       & ndim1,ndim2,ndim3,dim1,dim2,dim3,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=4,maxlen=400
    character(len=*),  intent(in) :: fname
    character(len=*),  intent(in) :: dim1name,dim2name,dim3name,varname
    character(len=*),  intent(inout) :: dim1unit,dim2unit,dim3unit,varunit
    integer,intent(inout) :: ndim1,ndim2,ndim3
    real(idx), allocatable, intent(inout) :: dim1(:),dim2(:),dim3(:),vars(:,:,:)
    integer :: ncid
    integer :: err
    real(idx) :: scale_factor,add_offset
    integer :: varid,dim1id,dim2id,dim3id
    integer :: start(3), count(3),shapes(3)
    ! Open files
    call check_r( nf90_open(trim(fname),nf90_nowrite,ncid))
    if (allocated(dim1) .eqv. .true.) then
       deallocate(dim1)
    end if
    if (allocated(dim2) .eqv. .true.) then
       deallocate(dim2)
    end if
    if (allocated(dim3) .eqv. .true.) then
       deallocate(dim3)
    end if
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    ! Obtain dim1
    call get_dimension(ncid,dim1name,ndim1,dim1)
    ! Obtain dim2
    call get_dimension(ncid,dim2name,ndim2,dim2)
    ! Obtain dim3
    call get_dimension(ncid,dim3name,ndim3,dim3)
    ! Allocate data
    allocate(vars(ndim1,ndim2,ndim3), stat=err)
    start = (/1,1,1/)
    count = (/ndim1,ndim2,ndim3/)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = start, &
         count = count) )

    ! Get attributes
    call check_r( nf90_inq_dimid(ncid, dim1name, dim1id) )
    if ( nf90_get_att(ncid, dim1id, 'units',dim1unit) == 0 ) then
       call check_r( nf90_get_att(ncid,dim1id, 'units',dim1unit))
    else
       dim1unit="None"
    end if
    call check_r( nf90_inq_dimid(ncid, dim2name, dim2id) )
    if ( nf90_get_att(ncid, dim2id, 'units',dim2unit) == 0 ) then
       call check_r( nf90_get_att(ncid,dim2id, 'units',dim2unit))
    else
       dim2unit="None"
    end if
    call check_r( nf90_inq_dimid(ncid, dim3name, dim3id) )
    if ( nf90_get_att(ncid, dim3id, 'units',dim3unit) == 0 ) then
       call check_r( nf90_get_att(ncid,dim3id, 'units',dim3unit))
    else
       dim3unit="None"
    end if
    if ( nf90_get_att(ncid, varid, 'units',varunit) == 0 ) then
       call check_r( nf90_get_att(ncid,varid, 'units',varunit))
    else
       varunit="None"
    end if
    if ( nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if ( nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    shapes=shape(vars)
    ndim1=shapes(1) ; ndim2=shapes(2)
    ndim1=shapes(3)
    ! ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_variable_3D
  subroutine dget_variable_3D(fname,dim1name,dim2name,dim3name,varname,dim1unit,dim2unit,dim3unit,varunit,&
       & ndim1,ndim2,ndim3,dim1,dim2,dim3,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=8,maxlen=400
    character(len=*),  intent(in) :: fname
    character(len=*),  intent(in) :: dim1name,dim2name,dim3name,varname
    character(len=*),  intent(inout) :: dim1unit,dim2unit,dim3unit,varunit
    integer,intent(inout) :: ndim1,ndim2,ndim3
    real(idx), allocatable, intent(inout) :: dim1(:),dim2(:),dim3(:),vars(:,:,:)
    integer :: ncid
    integer :: err
    real(idx) :: scale_factor,add_offset
    integer :: varid,dim1id,dim2id,dim3id
    integer :: start(3), count(3),shapes(3)
    ! Open files
    call check_r( nf90_open(trim(fname),nf90_nowrite,ncid))
    if (allocated(dim1) .eqv. .true.) then
       deallocate(dim1)
    end if
    if (allocated(dim2) .eqv. .true.) then
       deallocate(dim2)
    end if
    if (allocated(dim3) .eqv. .true.) then
       deallocate(dim3)
    end if
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    ! Obtain dim1
    call dget_dimension(ncid,dim1name,ndim1,dim1)
    ! Obtain dim2
    call dget_dimension(ncid,dim2name,ndim2,dim2)
    ! Obtain dim3
    call dget_dimension(ncid,dim3name,ndim3,dim3)
    ! Allocate data
    allocate(vars(ndim1,ndim2,ndim3), stat=err)
    start = (/1,1,1/)
    count = (/ndim1,ndim2,ndim3/)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = start, &
         count = count) )

    ! Get attributes
    call check_r( nf90_inq_dimid(ncid, dim1name, dim1id) )
    if ( nf90_get_att(ncid, dim1id, 'units',dim1unit) == 0 ) then
       call check_r( nf90_get_att(ncid,dim1id, 'units',dim1unit))
    else
       dim1unit="None"
    end if
    call check_r( nf90_inq_dimid(ncid, dim2name, dim2id) )
    if ( nf90_get_att(ncid, dim2id, 'units',dim2unit) == 0 ) then
       call check_r( nf90_get_att(ncid,dim2id, 'units',dim2unit))
    else
       dim2unit="None"
    end if
    call check_r( nf90_inq_dimid(ncid, dim3name, dim3id) )
    if ( nf90_get_att(ncid, dim3id, 'units',dim3unit) == 0 ) then
       call check_r( nf90_get_att(ncid,dim3id, 'units',dim3unit))
    else
       dim3unit="None"
    end if
    if ( nf90_get_att(ncid, varid, 'units',varunit) == 0 ) then
       call check_r( nf90_get_att(ncid,varid, 'units',varunit))
    else
       varunit="None"
    end if
    if ( nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if ( nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    shapes=shape(vars)
    ndim1=shapes(1) ; ndim2=shapes(2)
    ndim1=shapes(3)
    ! ! close file
    call check_r(nf90_close(ncid))
  end subroutine dget_variable_3D
  ! Variable only
    subroutine get_variable_3D_nc(fname,dim1name,dim2name,dim3name,varname,&
       & ndim1,ndim2,ndim3,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=4,maxlen=400
    character(len=*),  intent(in) :: fname
    character(len=*),  intent(in) :: dim1name,dim2name,dim3name,varname
    integer,intent(inout) :: ndim1,ndim2,ndim3
    real(idx), allocatable, intent(inout) :: vars(:,:,:)
    integer :: ncid
    integer :: err
    real(idx) :: scale_factor,add_offset
    integer :: varid,dim1id,dim2id,dim3id
    integer :: start(3), count(3),shapes(3)
    ! Open files
    call check_r( nf90_open(trim(fname),nf90_nowrite,ncid))
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    ! Obtain dim1
    call get_ndim(ncid,dim1name,ndim1)
    ! Obtain dim2
    call get_ndim(ncid,dim2name,ndim2)
    ! Obtain dim3
    call get_ndim(ncid,dim3name,ndim3)
    ! Allocate data
    allocate(vars(ndim1,ndim2,ndim3), stat=err)
    start = (/1,1,1/)
    count = (/ndim1,ndim2,ndim3/)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = start, &
         count = count) )

    if ( nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if ( nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    shapes=shape(vars)
    ndim1=shapes(1) ; ndim2=shapes(2)
    ndim3=shapes(3)
    ! ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_variable_3D_nc
  subroutine dget_variable_3D_nc(fname,dim1name,dim2name,dim3name,varname,&
       & ndim1,ndim2,ndim3,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=8,maxlen=400
    character(len=*),  intent(in) :: fname
    character(len=*),  intent(in) :: dim1name,dim2name,dim3name,varname
    integer,intent(inout) :: ndim1,ndim2,ndim3
    real(idx), allocatable, intent(inout) :: vars(:,:,:)
    integer :: ncid
    integer :: err
    real(idx) :: scale_factor,add_offset
    integer :: varid,dim1id,dim2id,dim3id
    integer :: start(3), count(3),shapes(3)
    ! Open files
    call check_r( nf90_open(trim(fname),nf90_nowrite,ncid))
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    ! Obtain dim1
    call get_ndim(ncid,dim1name,ndim1)
    ! Obtain dim2
    call get_ndim(ncid,dim2name,ndim2)
    ! Obtain dim3
    call get_ndim(ncid,dim3name,ndim3)
    ! Allocate data
    allocate(vars(ndim1,ndim2,ndim3), stat=err)
    start = (/1,1,1/)
    count = (/ndim1,ndim2,ndim3/)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = start, &
         count = count) )

    if ( nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if ( nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    shapes=shape(vars)
    ndim1=shapes(1) ; ndim2=shapes(2)
    ndim3=shapes(3)
    ! ! close file
    call check_r(nf90_close(ncid))
  end subroutine dget_variable_3D_nc
  subroutine get_variable_3D_wr(fname,varname,istr_1,iend_1,istr_2,iend_2,istr_3,iend_3,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=4,maxlen=400,ndim=3
    character(len=*),  intent(in) :: fname,varname
    integer,intent(in) :: istr_1,iend_1
    integer,intent(in) :: istr_2,iend_2
    integer,intent(in) :: istr_3,iend_3
    real(idx), allocatable, intent(inout) :: vars(:,:,:)
    integer :: ndim1,ndim2,ndim3
    integer :: ncid,varid
    integer :: err
    real(idx) :: scale_factor,add_offset
    integer :: start(ndim), count(ndim)
    ! Open files
    call check_r(nf90_open(trim(fname),nf90_nowrite,ncid))    
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    ndim1 = iend_1-istr_1+1
    ndim2 = iend_2-istr_2+1
    ndim3 = iend_3-istr_3+1
    ! Allocate data
    allocate(vars(ndim1,ndim2,ndim3), stat=err)
    start = (/istr_1,istr_2,istr_3/)
    count = (/ndim1,ndim2,ndim3/)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = start, &
         count = count) )
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
    ! ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_variable_3D_wr
  subroutine dget_variable_3D_wr(fname,varname,istr_1,iend_1,istr_2,iend_2,istr_3,iend_3,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=8,maxlen=400,ndim=3
    character(len=*),  intent(in) :: fname,varname
    integer,intent(in) :: istr_1,iend_1
    integer,intent(in) :: istr_2,iend_2
    integer,intent(in) :: istr_3,iend_3
    real(idx), allocatable, intent(inout) :: vars(:,:,:)
    integer :: ndim1,ndim2,ndim3
    integer :: ncid,varid
    integer :: err
    real(idx) :: scale_factor,add_offset
    integer :: start(ndim), count(ndim)
    ! Open files
    call check_r(nf90_open(trim(fname),nf90_nowrite,ncid))    
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    ndim1 = iend_1-istr_1+1
    ndim2 = iend_2-istr_2+1
    ndim3 = iend_3-istr_3+1
    ! Allocate data
    allocate(vars(ndim1,ndim2,ndim3), stat=err)
    start = (/istr_1,istr_2,istr_3/)
    count = (/ndim1,ndim2,ndim3/)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = start, &
         count = count) )
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
    ! ! close file
    call check_r(nf90_close(ncid))
  end subroutine dget_variable_3D_wr
  !==================================================
  ! 4D
  !==================================================
  subroutine get_variable_4D(fname,dim1name,dim2name,dim3name,dim4name,varname,dim1unit,dim2unit,dim3unit,dim4unit,varunit,&
       & ndim1,ndim2,ndim3,ndim4,dim1,dim2,dim3,dim4,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=4,maxlen=400
    character(len=*),  intent(in) :: fname
    character(len=*),  intent(in) :: dim1name,dim2name,dim3name,dim4name,varname
    character(len=*),  intent(inout) :: dim1unit,dim2unit,dim3unit,dim4unit,varunit
    integer,intent(inout) :: ndim1,ndim2,ndim3,ndim4
    real(idx), allocatable, intent(inout) :: dim1(:),dim2(:),dim3(:),dim4(:),vars(:,:,:,:)
    integer :: ncid
    integer :: err
    real(idx) :: scale_factor,add_offset
    integer :: varid,dim1id,dim2id,dim3id,dim4id
    integer :: start(4), count(4),shapes(4)
    ! Open files
    call check_r( nf90_open(trim(fname),nf90_nowrite,ncid))
    if (allocated(dim1) .eqv. .true.) then
       deallocate(dim1)
    end if
    if (allocated(dim2) .eqv. .true.) then
       deallocate(dim2)
    end if
    if (allocated(dim3) .eqv. .true.) then
       deallocate(dim3)
    end if
    if (allocated(dim4) .eqv. .true.) then
       deallocate(dim4)
    end if
    
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    ! Obtain dim1
    call get_dimension(ncid,dim1name,ndim1,dim1)
    ! Obtain dim2
    call get_dimension(ncid,dim2name,ndim2,dim2)
    ! Obtain dim3
    call get_dimension(ncid,dim3name,ndim3,dim3)
    ! Obtain dim4
    call get_dimension(ncid,dim4name,ndim4,dim4)
 
    ! Allocate data
    allocate(vars(ndim1,ndim2,ndim3,ndim4), stat=err)
    start = (/1,1,1,1/)
    count = (/ndim1,ndim2,ndim3,ndim4/)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = start, &
         count = count) )

    ! Get attributes
    call check_r( nf90_inq_dimid(ncid, dim1name, dim1id) )
    if ( nf90_get_att(ncid, dim1id, 'units',dim1unit) == 0 ) then
       call check_r( nf90_get_att(ncid,dim1id, 'units',dim1unit))
    else
       dim1unit="None"
    end if
    call check_r( nf90_inq_dimid(ncid, dim2name, dim2id) )
    if ( nf90_get_att(ncid, dim2id, 'units',dim2unit) == 0 ) then
       call check_r( nf90_get_att(ncid,dim2id, 'units',dim2unit))
    else
       dim2unit="None"
    end if
    call check_r( nf90_inq_dimid(ncid, dim3name, dim3id) )
    if ( nf90_get_att(ncid, dim3id, 'units',dim3unit) == 0 ) then
       call check_r( nf90_get_att(ncid,dim3id, 'units',dim3unit))
    else
       dim3unit="None"
    end if
    call check_r( nf90_inq_dimid(ncid, dim4name, dim4id) )
    if ( nf90_get_att(ncid, dim4id, 'units',dim4unit) == 0 ) then
       call check_r( nf90_get_att(ncid,dim4id, 'units',dim4unit))
    else
       dim4unit="None"
    end if

    if ( nf90_get_att(ncid, varid, 'units',varunit) == 0 ) then
       call check_r( nf90_get_att(ncid,varid, 'units',varunit))
    else
       varunit="None"
    end if
    if ( nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if ( nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    shapes=shape(vars)
    ndim1=shapes(1) ; ndim2=shapes(2)
    ndim3=shapes(3) ; ndim4=shapes(4)
    ! ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_variable_4D
  subroutine dget_variable_4D(fname,dim1name,dim2name,dim3name,dim4name,varname,dim1unit,dim2unit,dim3unit,dim4unit,varunit,&
       & ndim1,ndim2,ndim3,ndim4,dim1,dim2,dim3,dim4,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=8,maxlen=400
    character(len=*),  intent(in) :: fname
    character(len=*),  intent(in) :: dim1name,dim2name,dim3name,dim4name,varname
    character(len=*),  intent(inout) :: dim1unit,dim2unit,dim3unit,dim4unit,varunit
    integer,intent(inout) :: ndim1,ndim2,ndim3,ndim4
    real(idx), allocatable, intent(inout) :: dim1(:),dim2(:),dim3(:),dim4(:),vars(:,:,:,:)
    integer :: ncid
    integer :: err
    real(idx) :: scale_factor,add_offset
    integer :: varid,dim1id,dim2id,dim3id,dim4id
    integer :: start(4), count(4),shapes(4)
    ! Open files
    call check_r( nf90_open(trim(fname),nf90_nowrite,ncid))
    if (allocated(dim1) .eqv. .true.) then
       deallocate(dim1)
    end if
    if (allocated(dim2) .eqv. .true.) then
       deallocate(dim2)
    end if
    if (allocated(dim3) .eqv. .true.) then
       deallocate(dim3)
    end if
    if (allocated(dim4) .eqv. .true.) then
       deallocate(dim4)
    end if
    
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    ! Obtain dim1
    call dget_dimension(ncid,dim1name,ndim1,dim1)
    ! Obtain dim2
    call dget_dimension(ncid,dim2name,ndim2,dim2)
    ! Obtain dim3
    call dget_dimension(ncid,dim3name,ndim3,dim3)
    ! Obtain dim4
    call dget_dimension(ncid,dim4name,ndim4,dim4)
 
    ! Allocate data
    allocate(vars(ndim1,ndim2,ndim3,ndim4), stat=err)
    start = (/1,1,1,1/)
    count = (/ndim1,ndim2,ndim3,ndim4/)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = start, &
         count = count) )

    ! Get attributes
    call check_r( nf90_inq_dimid(ncid, dim1name, dim1id) )
    if ( nf90_get_att(ncid, dim1id, 'units',dim1unit) == 0 ) then
       call check_r( nf90_get_att(ncid,dim1id, 'units',dim1unit))
    else
       dim1unit="None"
    end if
    call check_r( nf90_inq_dimid(ncid, dim2name, dim2id) )
    if ( nf90_get_att(ncid, dim2id, 'units',dim2unit) == 0 ) then
       call check_r( nf90_get_att(ncid,dim2id, 'units',dim2unit))
    else
       dim2unit="None"
    end if
    call check_r( nf90_inq_dimid(ncid, dim3name, dim3id) )
    if ( nf90_get_att(ncid, dim3id, 'units',dim3unit) == 0 ) then
       call check_r( nf90_get_att(ncid,dim3id, 'units',dim3unit))
    else
       dim3unit="None"
    end if
    call check_r( nf90_inq_dimid(ncid, dim4name, dim4id) )
    if ( nf90_get_att(ncid, dim4id, 'units',dim4unit) == 0 ) then
       call check_r( nf90_get_att(ncid,dim4id, 'units',dim4unit))
    else
       dim4unit="None"
    end if

    if ( nf90_get_att(ncid, varid, 'units',varunit) == 0 ) then
       call check_r( nf90_get_att(ncid,varid, 'units',varunit))
    else
       varunit="None"
    end if
    if ( nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if ( nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    shapes=shape(vars)
    ndim1=shapes(1) ; ndim2=shapes(2)
    ndim3=shapes(3) ; ndim4=shapes(4)
    ! ! close file
    call check_r(nf90_close(ncid))
  end subroutine dget_variable_4D
  ! Variable only
    subroutine get_variable_4D_nc(fname,dim1name,dim2name,dim3name,dim4name,varname,&
       & ndim1,ndim2,ndim3,ndim4,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=4,maxlen=400
    character(len=*),  intent(in) :: fname
    character(len=*),  intent(in) :: dim1name,dim2name,dim3name,dim4name,varname
    integer,intent(inout) :: ndim1,ndim2,ndim3,ndim4
    real(idx), allocatable, intent(inout) :: vars(:,:,:,:)
    integer :: ncid
    integer :: err
    real(idx) :: scale_factor,add_offset
    integer :: varid,dim1id,dim2id,dim3id,dim4id
    integer :: start(4), count(4),shapes(4)
    ! Open files
    call check_r( nf90_open(trim(fname),nf90_nowrite,ncid))
    
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    ! Obtain dim1
    call get_ndim(ncid,dim1name,ndim1)
    ! Obtain dim2
    call get_ndim(ncid,dim2name,ndim2)
    ! Obtain dim3
    call get_ndim(ncid,dim3name,ndim3)
    ! Obtain dim4
    call get_ndim(ncid,dim4name,ndim4)
 
    ! Allocate data
    allocate(vars(ndim1,ndim2,ndim3,ndim4), stat=err)
    start = (/1,1,1,1/)
    count = (/ndim1,ndim2,ndim3,ndim4/)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = start, &
         count = count) )

    if ( nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if ( nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    shapes=shape(vars)
    ndim1=shapes(1) ; ndim2=shapes(2)
    ndim3=shapes(3) ; ndim4=shapes(4)
    ! ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_variable_4D_nc
  subroutine dget_variable_4D_nc(fname,dim1name,dim2name,dim3name,dim4name,varname,&
       & ndim1,ndim2,ndim3,ndim4,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=8,maxlen=400
    character(len=*),  intent(in) :: fname
    character(len=*),  intent(in) :: dim1name,dim2name,dim3name,dim4name,varname
    integer,intent(inout) :: ndim1,ndim2,ndim3,ndim4
    real(idx), allocatable, intent(inout) :: vars(:,:,:,:)
    integer :: ncid
    integer :: err
    real(idx) :: scale_factor,add_offset
    integer :: varid,dim1id,dim2id,dim3id,dim4id
    integer :: start(4), count(4),shapes(4)
    ! Open files
    call check_r( nf90_open(trim(fname),nf90_nowrite,ncid))
    
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    ! Obtain dim1
    call get_ndim(ncid,dim1name,ndim1)
    ! Obtain dim2
    call get_ndim(ncid,dim2name,ndim2)
    ! Obtain dim3
    call get_ndim(ncid,dim3name,ndim3)
    ! Obtain dim4
    call get_ndim(ncid,dim4name,ndim4)
 
    ! Allocate data
    allocate(vars(ndim1,ndim2,ndim3,ndim4), stat=err)
    start = (/1,1,1,1/)
    count = (/ndim1,ndim2,ndim3,ndim4/)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = start, &
         count = count) )

    if ( nf90_get_att(ncid, varid, 'scale_factor',scale_factor) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'scale_factor',scale_factor))
    else
       scale_factor=1.0
    end if
    if ( nf90_get_att(ncid, varid, 'add_offset',add_offset) == 0 ) then
       call check_r( nf90_get_att(ncid, varid, 'add_offset',add_offset))
    else
       add_offset = 0.0
    end if
    vars = vars * scale_factor + add_offset
    shapes=shape(vars)
    ndim1=shapes(1) ; ndim2=shapes(2)
    ndim3=shapes(3) ; ndim4=shapes(4)
    ! ! close file
    call check_r(nf90_close(ncid))
  end subroutine dget_variable_4D_nc
  subroutine get_variable_4D_wr(fname,varname,istr_1,iend_1,istr_2,iend_2,istr_3,iend_3,istr_4,iend_4,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=4,maxlen=400
    character(len=*),  intent(in) :: fname,varname
    integer,intent(in) :: istr_1,iend_1
    integer,intent(in) :: istr_2,iend_2
    integer,intent(in) :: istr_3,iend_3
    integer,intent(in) :: istr_4,iend_4
    real(idx), allocatable, intent(inout) :: vars(:,:,:,:)
    integer :: ndim1,ndim2,ndim3,ndim4
    integer :: ncid
    integer :: err
    real(idx) :: scale_factor,add_offset
    integer :: varid,dim1id,dim2id,dim3id,dim4id
    integer :: start(4), count(4),shapes(4)
    ! Open files
    call check_r(nf90_open(trim(fname),nf90_nowrite,ncid))
    
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    ndim1 = iend_1-istr_1+1
    ndim2 = iend_2-istr_2+1
    ndim3 = iend_3-istr_3+1
    ndim4 = iend_4-istr_4+1

    ! Allocate data
    allocate(vars(ndim1,ndim2,ndim3,ndim4), stat=err)
    start = (/istr_1,istr_2,istr_3,istr_4/)
    count = (/ndim1,ndim2,ndim3,ndim4/)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = start, &
         count = count) )
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
    ! ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_variable_4D_wr
  subroutine dget_variable_4D_wr(fname,varname,istr_1,iend_1,istr_2,iend_2,istr_3,iend_3,istr_4,iend_4,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=8,maxlen=400,ndim=4
    character(len=*),  intent(in) :: fname,varname
    integer,intent(in) :: istr_1,iend_1
    integer,intent(in) :: istr_2,iend_2
    integer,intent(in) :: istr_3,iend_3
    integer,intent(in) :: istr_4,iend_4
    real(idx), allocatable, intent(inout) :: vars(:,:,:,:)
    integer :: ndim1,ndim2,ndim3,ndim4
    integer :: ncid
    integer :: err
    real(idx) :: scale_factor,add_offset
    integer :: varid
    integer :: start(ndim), count(ndim)
    ! Open files
    call check_r(nf90_open(trim(fname),nf90_nowrite,ncid))
    
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    ndim1 = iend_1-istr_1+1
    ndim2 = iend_2-istr_2+1
    ndim3 = iend_3-istr_3+1
    ndim4 = iend_4-istr_4+1

    ! Allocate data
    allocate(vars(ndim1,ndim2,ndim3,ndim4), stat=err)
    start = (/istr_1,istr_2,istr_3,istr_4/)
    count = (/ndim1,ndim2,ndim3,ndim4/)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = start, &
         count = count) )
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
    ! ! close file
    call check_r(nf90_close(ncid))
  end subroutine dget_variable_4D_wr

  subroutine get_variable_5D_wr(fname,varname,istr_1,iend_1,istr_2,iend_2,istr_3,iend_3,istr_4,iend_4,istr_5,iend_5,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=4,maxlen=400,ndim=5
    character(len=*),  intent(in) :: fname,varname
    integer,intent(in) :: istr_1,iend_1
    integer,intent(in) :: istr_2,iend_2
    integer,intent(in) :: istr_3,iend_3
    integer,intent(in) :: istr_4,iend_4
    integer,intent(in) :: istr_5,iend_5
    real(idx), allocatable, intent(inout) :: vars(:,:,:,:,:)
    integer :: ndim1,ndim2,ndim3,ndim4,ndim5
    integer :: ncid
    integer :: err
    real(idx) :: scale_factor,add_offset
    integer :: varid
    integer :: start(ndim), count(ndim)
    ! Open files
    call check_r(nf90_open(trim(fname),nf90_nowrite,ncid))
    
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    ndim1 = iend_1-istr_1+1
    ndim2 = iend_2-istr_2+1
    ndim3 = iend_3-istr_3+1
    ndim4 = iend_4-istr_4+1
    ndim5 = iend_5-istr_5+1

    ! Allocate data
    allocate(vars(ndim1,ndim2,ndim3,ndim4,ndim5), stat=err)
    start = (/istr_1,istr_2,istr_3,istr_4,istr_5/)
    count = (/ndim1,ndim2,ndim3,ndim4,ndim5/)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = start, &
         count = count) )
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
    ! ! close file
    call check_r(nf90_close(ncid))
  end subroutine get_variable_5D_wr
  
  subroutine dget_variable_5D_wr(fname,varname,istr_1,iend_1,istr_2,iend_2,istr_3,iend_3,istr_4,iend_4,istr_5,iend_5,vars)
    use netcdf
    implicit none
    integer,parameter :: idx=8,maxlen=400,ndim=5
    character(len=*),  intent(in) :: fname,varname
    integer,intent(in) :: istr_1,iend_1
    integer,intent(in) :: istr_2,iend_2
    integer,intent(in) :: istr_3,iend_3
    integer,intent(in) :: istr_4,iend_4
    integer,intent(in) :: istr_5,iend_5
    real(idx), allocatable, intent(inout) :: vars(:,:,:,:,:)
    integer :: ndim1,ndim2,ndim3,ndim4,ndim5
    integer :: ncid
    integer :: err
    real(idx) :: scale_factor,add_offset
    integer :: varid
    integer :: start(ndim), count(ndim)
    ! Open files
    call check_r(nf90_open(trim(fname),nf90_nowrite,ncid))
    
    if (allocated(vars) .eqv. .true.) then
       deallocate(vars)
    end if
    ndim1 = iend_1-istr_1+1
    ndim2 = iend_2-istr_2+1
    ndim3 = iend_3-istr_3+1
    ndim4 = iend_4-istr_4+1
    ndim5 = iend_5-istr_5+1

    ! Allocate data
    allocate(vars(ndim1,ndim2,ndim3,ndim4,ndim5), stat=err)
    start = (/istr_1,istr_2,istr_3,istr_4,istr_5/)
    count = (/ndim1,ndim2,ndim3,ndim4,ndim5/)
    call check_r( nf90_inq_varid(ncid, varname, varid) )
    call check_r( nf90_get_var(ncid, varid, vars, start = start, &
         count = count) )
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
    ! ! close file
    call check_r(nf90_close(ncid))
  end subroutine dget_variable_5D_wr

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
       atts="None"
    end if
  end subroutine get_attribute_f
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
       atts="None"
    end if
  end subroutine get_attribute_d
end module ncdf_read
