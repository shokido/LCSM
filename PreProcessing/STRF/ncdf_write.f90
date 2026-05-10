module NCDF_WRITE
  ! Compile method
  !  ifort -c ncdf_write.f90 -I/usr/local/netcdf4/include -L/usr/local/netcdf4/lib/ -lnetcdf -lnetcdff
  !  ar rcv /Users/kido/fortran/library_ifort/libncdf_write.a ncdf_write.o
  !  ranlib /Users/kido/fortran/library_ifort/libncdf_write.a
  implicit none
  private
  !------------------------------------------------------------
  ! ! Preparation
  interface WRITENET_PRE
     module procedure WRITENET_1D_PRE_I,WRITENET_1D_PRE_F,WRITENET_1D_PRE_D,&
          & WRITENET_2D_PRE_I,WRITENET_2D_PRE_F,WRITENET_2D_PRE_D, &
          & WRITENET_3D_PRE_I,WRITENET_3D_PRE_F,WRITENET_3D_PRE_D, &
          & WRITENET_4D_PRE_I,WRITENET_4D_PRE_F,WRITENET_4D_PRE_D, &
          & WRITENET_5D_PRE_I,WRITENET_5D_PRE_F,WRITENET_5D_PRE_D
  end interface WRITENET_PRE
  ! !------------------------------------------------------------
  interface WRITENET_DV
     module procedure WRITENET_1D_DV_I,WRITENET_1D_DV_F,WRITENET_1D_DV_D,&
          & WRITENET_2D_DV_I,WRITENET_2D_DV_F,WRITENET_2D_DV_D, &
          & WRITENET_3D_DV_I,WRITENET_3D_DV_F,WRITENET_3D_DV_D, &
          & WRITENET_4D_DV_I,WRITENET_4D_DV_F,WRITENET_4D_DV_D, &
          & WRITENET_5D_DV_I,WRITENET_5D_DV_F,WRITENET_5D_DV_D 
  end interface WRITENET_DV

  interface add_var_att
     module procedure add_var_att_c,add_var_att_f,add_var_att_d,add_var_att_i
  end interface add_var_att
  interface WRITENET_wv
     module procedure WRITENET_wv_1d_i,WRITENET_wv_1d_f,WRITENET_wv_1d_d,&
          & WRITENET_wv_2d_i,WRITENET_wv_2d_f,WRITENET_wv_2d_d,&
          & WRITENET_wv_3d_i,WRITENET_wv_3d_f,WRITENET_wv_3d_d,&
          & WRITENET_wv_4d_i,WRITENET_wv_4d_f,WRITENET_wv_4d_d,&
          & WRITENET_wv_5d_i,WRITENET_wv_5d_f,WRITENET_wv_5d_d
  end interface WRITENET_wv

  ! !------------------------------------------------------------
  ! public :: CHECK_W
  ! public :: WRITENET_PRE,WRITENET_DV,WRITENET_WV
  ! public :: CREATE_TIME_ATT
  ! public :: WRITENET_DD,ADD_VAR_ATT
  ! public :: WRITENET_PREDV
  
  public :: add_var_att
  public :: writenet_def_dim
  public :: writenet_def_var
  public :: writenet_pre,writenet_dv,writenet_wv
contains
  !=======================================
  ! Subroutine for check netcdf operation
  !=======================================
  subroutine CHECK_W(status)
    use NETCDF
    implicit none
    integer, intent ( in) :: status
    if(status /= NF90_NOERR) then 
       print *, trim(NF90_STRERROR(status))
       stop "Stopped"
    end if
  end subroutine CHECK_W
 subroutine add_var_att_c(file_name,varname,att_name,att)
    use netcdf
    implicit none
    ! Input data
    character(len=*),intent(in) :: file_name,varname,att_name,att
    integer :: ncid,vars_varid
    ! open file
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    ! get variable id
    call CHECK_W(NF90_INQ_VARID(ncid,varname,vars_varid))
    ! Add attributes
    call CHECK_W(NF90_REDEF(ncid))
    call CHECK_W(NF90_PUT_ATT(ncid, vars_varid,att_name,att))
    call CHECK_W(NF90_ENDDEF(ncid))
    ! close file
    call check_w(NF90_CLOSE(ncid))
  end subroutine add_var_att_c
  subroutine add_var_att_i(file_name,varname,att_name,att)
    use netcdf
    implicit none
    ! Input data
    character(len=*),intent(in) :: file_name,varname,att_name
    integer :: att
    integer :: ncid,vars_varid
    ! open file
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    ! get variable id
    call CHECK_W(NF90_INQ_VARID(ncid,varname,vars_varid))
    ! Add attributes
    call CHECK_W(NF90_REDEF(ncid))
    call CHECK_W(NF90_PUT_ATT(ncid, vars_varid,att_name,att))
    call CHECK_W(NF90_ENDDEF(ncid))
    ! close file
    call check_w(NF90_CLOSE(ncid))
  end subroutine add_var_att_i
    subroutine add_var_att_f(file_name,varname,att_name,att)
    use netcdf
    implicit none
    ! Input data
    character(len=*),intent(in) :: file_name,varname,att_name
    real(4) :: att
    integer :: ncid,vars_varid
    ! open file
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    ! get variable id
    call CHECK_W(NF90_INQ_VARID(ncid,varname,vars_varid))
    ! Add attributes
    call CHECK_W(NF90_REDEF(ncid))
    call CHECK_W(NF90_PUT_ATT(ncid, vars_varid,att_name,att))
    call CHECK_W(NF90_ENDDEF(ncid))
    ! close file
    call check_w(NF90_CLOSE(ncid))
  end subroutine add_var_att_f
  subroutine add_var_att_d(file_name,varname,att_name,att)
    use netcdf
    implicit none
    ! Input data
    character(len=*),intent(in) :: file_name,varname,att_name
    real(8) :: att
    integer :: ncid,vars_varid
    ! open file
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    ! get variable id
    call CHECK_W(NF90_INQ_VARID(ncid,varname,vars_varid))
    ! Add attributes
    call CHECK_W(NF90_REDEF(ncid))
    call CHECK_W(NF90_PUT_ATT(ncid, vars_varid,att_name,att))
    call CHECK_W(NF90_ENDDEF(ncid))
    ! close file
    call check_w(NF90_CLOSE(ncid))
  end subroutine add_var_att_d
  subroutine WRITENET_def_dim(file_name,total_dim,ndims,dim_name,dtype)
    ! call writenet_def_dim(fname,1,(/10/),(/"lon"/),"float")
    use NETCDF
    implicit none
    integer,parameter :: maxlen=400
    character(len=*),intent(in) :: file_name
    integer,intent(in) :: total_dim,ndims(total_dim)
    character(len=*),intent(in) :: dim_name(total_dim)
    character(len=*),intent(in) :: dtype(total_dim)
    !-Variables-----------------------------------
    integer :: ncid
    integer :: bnds_dimid
    integer :: dim1_dimid,dim1_varid
    integer :: idim
    ! Create file
    call CHECK_W(NF90_CREATE(trim(file_name),nf90_netcdf4, ncid) )
    do idim =1,total_dim
       ! Define the dimensions
       call CHECK_W(NF90_DEF_DIM(ncid, trim(dim_name(idim)), ndims(idim),dim1_dimid))
       if (trim(dtype(idim)) .eq. "float") then
          call CHECK_W(NF90_DEF_VAR(ncid, trim(dim_name(idim)), NF90_FLOAT,dim1_dimid,dim1_varid))
       else if (trim(dtype(idim)) .eq. "integer") then
          call CHECK_W(NF90_DEF_VAR(ncid, trim(dim_name(idim)), NF90_INT,dim1_dimid,dim1_varid))
       else
          call CHECK_W(NF90_DEF_VAR(ncid, trim(dim_name(idim)), NF90_DOUBLE,dim1_dimid,dim1_varid))
       end if       
    end do
    call CHECK_W(NF90_ENDDEF(ncid))
    ! close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine WRITENET_DEF_DIM


  subroutine WRITENET_def_var(file_name,total_dim,nvar,dim_names,var_names,dtype)
    ! call writenet_def_dim(fname,1,2,(/"lev"/),(/"T","S"/),(/"float","double"/))
    use NETCDF
    implicit none
    character(len=*),intent(in) :: file_name
    integer,intent(in) :: total_dim,nvar
    character(len=*),intent(in) :: dim_names(total_dim)
    character(len=*),intent(in) :: var_names(total_dim)
    character(len=*),intent(in) :: dtype(nvar)
    !-Variables-----------------------------------
    integer :: ncid
    integer :: vars_varid
    integer,allocatable :: dimids(:)
    integer :: idim,ivar
    allocate(dimids(total_dim))
   ! Get ncid & dimid
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    do idim=1,total_dim
       call CHECK_W(NF90_INQ_DIMID(ncid,dim_names(idim),dimids(idim)))
    end do       
    ! Define variable
    call CHECK_W(NF90_REDEF(ncid))
    do ivar=1,nvar
       if (trim(dtype(ivar)) .eq. "float") then
          call CHECK_W(NF90_DEF_VAR(ncid, trim(var_names(ivar)),NF90_FLOAT,dimids, vars_varid))
       else if (trim(dtype(ivar)) .eq. "double") then
          call CHECK_W(NF90_DEF_VAR(ncid, trim(var_names(ivar)),NF90_DOUBLE, dimids, vars_varid))
       else if (trim(dtype(ivar)) .eq. "integer") then
          call CHECK_W(NF90_DEF_VAR(ncid, trim(var_names(ivar)),NF90_INT, dimids, vars_varid))
       end if
    end do
    call CHECK_W(NF90_ENDDEF(ncid))
    ! close file
    call CHECK_W(NF90_CLOSE(ncid) )
    deallocate(dimids)
  end subroutine WRITENET_DEF_VAR
  
  subroutine WRITENET_wv_1d_i(file_name,varname,start_i,end_i,vars)
    use NETCDF
    implicit none
    integer,parameter :: total_dim=1
    character(len=*),intent(in) :: file_name,varname
    integer,intent(in) :: start_i(total_dim),end_i(total_dim)
    integer,intent(in) :: vars(:)
    integer :: start(total_dim),count(total_dim)
    integer :: ncid,vars_varid,idim
    !-----------------------------
    ! Get file id and variable id
    !-----------------------------
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_VARID(ncid,varname,vars_varid))
    do idim=1,total_dim
       start(idim) = start_i(idim)
       count(idim)  = end_i(idim)-start_i(idim)+1
    end do
    ! Put variable
    call CHECK_W(NF90_PUT_VAR(ncid, vars_varid, vars,start=start,count=count) )
    ! Close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine WRITENET_wv_1d_i
   subroutine WRITENET_wv_1d_f(file_name,varname,start_i,end_i,vars)
    use NETCDF
    implicit none
    integer,parameter :: total_dim=1
    character(len=*),intent(in) :: file_name,varname
    integer,intent(in) :: start_i(total_dim),end_i(total_dim)
    real(4),intent(in) :: vars(:)
    integer :: start(total_dim),count(total_dim)
    integer :: ncid,vars_varid,idim
    !-----------------------------
    ! Get file id and variable id
    !-----------------------------
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_VARID(ncid,varname,vars_varid))
    do idim=1,total_dim
       start(idim) = start_i(idim)
       count(idim)  = end_i(idim)-start_i(idim)+1
    end do
    ! Put variable
    call CHECK_W(NF90_PUT_VAR(ncid, vars_varid, vars,start=start,count=count) )
    ! Close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine WRITENET_wv_1d_f
  subroutine WRITENET_wv_1d_d(file_name,varname,start_i,end_i,vars)
    use NETCDF
    implicit none
    integer,parameter :: total_dim=1
    character(len=*),intent(in) :: file_name,varname
    integer,intent(in) :: start_i(total_dim),end_i(total_dim)
    real(8),intent(in) :: vars(:)
    integer :: start(total_dim),count(total_dim)
    integer :: ncid,vars_varid,idim
    !-----------------------------
    ! Get file id and variable id
    !-----------------------------
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_VARID(ncid,varname,vars_varid))
    do idim=1,total_dim
       start(idim) = start_i(idim)
       count(idim)  = end_i(idim)-start_i(idim)+1
    end do
    ! Put variable
    call CHECK_W(NF90_PUT_VAR(ncid, vars_varid, vars,start=start,count=count) )
    ! Close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine WRITENET_wv_1d_d


  ! 2D routine
  subroutine WRITENET_wv_2d_i(file_name,varname,start_i,end_i,vars)
    use NETCDF
    implicit none
    integer,parameter :: total_dim=2
    character(len=*),intent(in) :: file_name,varname
    integer,intent(in) :: start_i(total_dim),end_i(total_dim)
    integer,intent(in) :: vars(:,:)
    integer :: start(total_dim),count(total_dim)
    integer :: ncid,vars_varid,idim
    !-----------------------------
    ! Get file id and variable id
    !-----------------------------
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_VARID(ncid,varname,vars_varid))
    do idim=1,total_dim
       start(idim) = start_i(idim)
       count(idim)  = end_i(idim)-start_i(idim)+1
    end do
    ! Put variable
    call CHECK_W(NF90_PUT_VAR(ncid, vars_varid, vars,start=start,count=count) )
    ! Close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine WRITENET_wv_2d_i
   subroutine WRITENET_wv_2d_f(file_name,varname,start_i,end_i,vars)
    use NETCDF
    implicit none
    integer,parameter :: total_dim=2
    character(len=*),intent(in) :: file_name,varname
    integer,intent(in) :: start_i(total_dim),end_i(total_dim)
    real(4),intent(in) :: vars(:,:)
    integer :: start(total_dim),count(total_dim)
    integer :: ncid,vars_varid,idim
    !-----------------------------
    ! Get file id and variable id
    !-----------------------------
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_VARID(ncid,varname,vars_varid))
    do idim=1,total_dim
       start(idim) = start_i(idim)
       count(idim)  = end_i(idim)-start_i(idim)+1
    end do
    ! Put variable
    call CHECK_W(NF90_PUT_VAR(ncid, vars_varid, vars,start=start,count=count) )
    ! Close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine WRITENET_wv_2d_f
  subroutine WRITENET_wv_2d_d(file_name,varname,start_i,end_i,vars)
    use NETCDF
    implicit none
    integer,parameter :: total_dim=1
    character(len=*),intent(in) :: file_name,varname
    integer,intent(in) :: start_i(total_dim),end_i(total_dim)
    real(8),intent(in) :: vars(:,:)
    integer :: start(total_dim),count(total_dim)
    integer :: ncid,vars_varid,idim
    !-----------------------------
    ! Get file id and variable id
    !-----------------------------
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_VARID(ncid,varname,vars_varid))
    do idim=1,total_dim
       start(idim) = start_i(idim)
       count(idim)  = end_i(idim)-start_i(idim)+1
    end do
    ! Put variable
    call CHECK_W(NF90_PUT_VAR(ncid, vars_varid, vars,start=start,count=count) )
    ! Close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine WRITENET_wv_2d_d

  ! 3D routine
  subroutine WRITENET_wv_3d_i(file_name,varname,start_i,end_i,vars)
    use NETCDF
    implicit none
    integer,parameter :: total_dim=3
    character(len=*),intent(in) :: file_name,varname
    integer,intent(in) :: start_i(total_dim),end_i(total_dim)
    integer,intent(in) :: vars(:,:,:)
    integer :: start(total_dim),count(total_dim)
    integer :: ncid,vars_varid,idim
    !-----------------------------
    ! Get file id and variable id
    !-----------------------------
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_VARID(ncid,varname,vars_varid))
    do idim=1,total_dim
       start(idim) = start_i(idim)
       count(idim)  = end_i(idim)-start_i(idim)+1
    end do
    ! Put variable
    call CHECK_W(NF90_PUT_VAR(ncid, vars_varid, vars,start=start,count=count) )
    ! Close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine WRITENET_wv_3d_i
   subroutine WRITENET_wv_3d_f(file_name,varname,start_i,end_i,vars)
    use NETCDF
    implicit none
    integer,parameter :: total_dim=3
    character(len=*),intent(in) :: file_name,varname
    integer,intent(in) :: start_i(total_dim),end_i(total_dim)
    real(4),intent(in) :: vars(:,:,:)
    integer :: start(total_dim),count(total_dim)
    integer :: ncid,vars_varid,idim
    !-----------------------------
    ! Get file id and variable id
    !-----------------------------
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_VARID(ncid,varname,vars_varid))
    do idim=1,total_dim
       start(idim) = start_i(idim)
       count(idim)  = end_i(idim)-start_i(idim)+1
    end do
    ! Put variable
    call CHECK_W(NF90_PUT_VAR(ncid, vars_varid, vars,start=start,count=count) )
    ! Close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine WRITENET_wv_3d_f
  subroutine WRITENET_wv_3d_d(file_name,varname,start_i,end_i,vars)
    use NETCDF
    implicit none
    integer,parameter :: total_dim=3
    character(len=*),intent(in) :: file_name,varname
    integer,intent(in) :: start_i(total_dim),end_i(total_dim)
    real(8),intent(in) :: vars(:,:,:)
    integer :: start(total_dim),count(total_dim)
    integer :: ncid,vars_varid,idim
    !-----------------------------
    ! Get file id and variable id
    !-----------------------------
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_VARID(ncid,varname,vars_varid))
    do idim=1,total_dim
       start(idim) = start_i(idim)
       count(idim)  = end_i(idim)-start_i(idim)+1
    end do
    ! Put variable
    call CHECK_W(NF90_PUT_VAR(ncid, vars_varid, vars,start=start,count=count) )
    ! Close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine WRITENET_wv_3d_d

    ! 4D routine
  subroutine WRITENET_wv_4d_i(file_name,varname,start_i,end_i,vars)
    use NETCDF
    implicit none
    integer,parameter :: total_dim=4
    character(len=*),intent(in) :: file_name,varname
    integer,intent(in) :: start_i(total_dim),end_i(total_dim)
    integer,intent(in) :: vars(:,:,:,:)
    integer :: start(total_dim),count(total_dim)
    integer :: ncid,vars_varid,idim
    !-----------------------------
    ! Get file id and variable id
    !-----------------------------
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_VARID(ncid,varname,vars_varid))
    do idim=1,total_dim
       start(idim) = start_i(idim)
       count(idim)  = end_i(idim)-start_i(idim)+1
    end do
    ! Put variable
    call CHECK_W(NF90_PUT_VAR(ncid, vars_varid, vars,start=start,count=count) )
    ! Close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine WRITENET_wv_4d_i
   subroutine WRITENET_wv_4d_f(file_name,varname,start_i,end_i,vars)
    use NETCDF
    implicit none
    integer,parameter :: total_dim=4
    character(len=*),intent(in) :: file_name,varname
    integer,intent(in) :: start_i(total_dim),end_i(total_dim)
    real(4),intent(in) :: vars(:,:,:,:)
    integer :: start(total_dim),count(total_dim)
    integer :: ncid,vars_varid,idim
    !-----------------------------
    ! Get file id and variable id
    !-----------------------------
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_VARID(ncid,varname,vars_varid))
    do idim=1,total_dim
       start(idim) = start_i(idim)
       count(idim)  = end_i(idim)-start_i(idim)+1
    end do
    ! Put variable
    call CHECK_W(NF90_PUT_VAR(ncid, vars_varid, vars,start=start,count=count) )
    ! Close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine WRITENET_wv_4d_f
  subroutine WRITENET_wv_4d_d(file_name,varname,start_i,end_i,vars)
    use NETCDF
    implicit none
    integer,parameter :: total_dim=4
    character(len=*),intent(in) :: file_name,varname
    integer,intent(in) :: start_i(total_dim),end_i(total_dim)
    real(8),intent(in) :: vars(:,:,:,:)
    integer :: start(total_dim),count(total_dim)
    integer :: ncid,vars_varid,idim
    !-----------------------------
    ! Get file id and variable id
    !-----------------------------
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_VARID(ncid,varname,vars_varid))
    do idim=1,total_dim
       start(idim) = start_i(idim)
       count(idim)  = end_i(idim)-start_i(idim)+1
    end do
    ! Put variable
    call CHECK_W(NF90_PUT_VAR(ncid, vars_varid, vars,start=start,count=count) )
    ! Close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine WRITENET_wv_4d_d

  ! 5D routine
  subroutine WRITENET_wv_5d_i(file_name,varname,start_i,end_i,vars)
    use NETCDF
    implicit none
    integer,parameter :: total_dim=5
    character(len=*),intent(in) :: file_name,varname
    integer,intent(in) :: start_i(total_dim),end_i(total_dim)
    integer,intent(in) :: vars(:,:,:,:,:)
    integer :: start(total_dim),count(total_dim)
    integer :: ncid,vars_varid,idim
    !-----------------------------
    ! Get file id and variable id
    !-----------------------------
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_VARID(ncid,varname,vars_varid))
    do idim=1,total_dim
       start(idim) = start_i(idim)
       count(idim)  = end_i(idim)-start_i(idim)+1
    end do
    ! Put variable
    call CHECK_W(NF90_PUT_VAR(ncid, vars_varid, vars,start=start,count=count) )
    ! Close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine WRITENET_wv_5d_i
   subroutine WRITENET_wv_5d_f(file_name,varname,start_i,end_i,vars)
    use NETCDF
    implicit none
    integer,parameter :: total_dim=5
    character(len=*),intent(in) :: file_name,varname
    integer,intent(in) :: start_i(total_dim),end_i(total_dim)
    real(4),intent(in) :: vars(:,:,:,:,:)
    integer :: start(total_dim),count(total_dim)
    integer :: ncid,vars_varid,idim
    !-----------------------------
    ! Get file id and variable id
    !-----------------------------
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_VARID(ncid,varname,vars_varid))
    do idim=1,total_dim
       start(idim) = start_i(idim)
       count(idim)  = end_i(idim)-start_i(idim)+1
    end do
    ! Put variable
    call CHECK_W(NF90_PUT_VAR(ncid, vars_varid, vars,start=start,count=count) )
    ! Close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine WRITENET_wv_5d_f
  subroutine WRITENET_wv_5d_d(file_name,varname,start_i,end_i,vars)
    use NETCDF
    implicit none
    integer,parameter :: total_dim=5
    character(len=*),intent(in) :: file_name,varname
    integer,intent(in) :: start_i(total_dim),end_i(total_dim)
    real(8),intent(in) :: vars(:,:,:,:,:)
    integer :: start(total_dim),count(total_dim)
    integer :: ncid,vars_varid,idim
    !-----------------------------
    ! Get file id and variable id
    !-----------------------------
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_VARID(ncid,varname,vars_varid))
    do idim=1,total_dim
       start(idim) = start_i(idim)
       count(idim)  = end_i(idim)-start_i(idim)+1
    end do
    ! Put variable
    call CHECK_W(NF90_PUT_VAR(ncid, vars_varid, vars,start=start,count=count) )
    ! Close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine WRITENET_wv_5d_d
  ! !==================================
  ! ! Subroutine for define dimensions
  ! !==================================
  subroutine WRITENET_1D_PRE_I(file_name,ndims1,dim1_name,dim1_units,dim1)
    implicit none
    integer,parameter :: maxlen=400
    character(len=*),intent(in) :: file_name
    integer,intent(in) :: ndims1
    character(len=*),intent(in) :: dim1_name,dim1_units
    integer :: dim1(ndims1)
    character(len=maxlen) :: dimname(1),typename(1)
    dimname(1)=trim(dim1_name)
    typename(1)="integer"
    call WRITENET_def_dim(file_name,1,(/ndims1/),dimname,typename)
    call add_var_att(file_name,dimname(1),"units",trim(dim1_units))
    call WRITENET_wv(file_name,dimname(1),(/1/),(/ndims1/),dim1)
  end subroutine WRITENET_1D_PRE_I
  subroutine WRITENET_1D_PRE_F(file_name,ndims1,dim1_name,dim1_units,dim1)
    implicit none
    integer,parameter :: idx = 4,maxlen=400
    character(len=*),intent(in) :: file_name
    integer,intent(in) :: ndims1
    character(len=*),intent(in) :: dim1_name,dim1_units
    real(idx),intent(in) :: dim1(ndims1)
    character(len=maxlen) :: dimname(1),typename(1)
    dimname(1)=trim(dim1_name)
    typename(1)="float"
    call WRITENET_def_dim(file_name,1,(/ndims1/),dimname,typename)
    call add_var_att(file_name,dimname(1),"units",trim(dim1_units))
    call WRITENET_wv(file_name,dimname(1),(/1/),(/ndims1/),dim1)
  end subroutine WRITENET_1D_PRE_F
  subroutine WRITENET_1D_PRE_D(file_name,ndims1,dim1_name,dim1_units,dim1)
    implicit none
    integer,parameter :: idx = 8,maxlen=400
    character(len=*),intent(in) :: file_name
    integer,intent(in) :: ndims1
    character(len=*),intent(in) :: dim1_name,dim1_units
    real(idx),intent(in) :: dim1(ndims1)
    character(len=maxlen) :: dimname(1),typename(1)
    dimname(1)=trim(dim1_name)
    typename(1)="double"
    call WRITENET_def_dim(file_name,1,(/ndims1/),dimname,typename)
    call add_var_att(file_name,dimname(1),"units",trim(dim1_units))
    call WRITENET_wv(file_name,dimname(1),(/1/),(/ndims1/),dim1)
  end subroutine WRITENET_1D_PRE_D
  subroutine WRITENET_2D_PRE_I(file_name,ndims1,ndims2,dim1_name,dim2_name,dim1_units,dim2_units&
       & ,dim1,dim2)
    implicit none
    ! Input data
    integer,parameter :: maxlen=400
    character(len=*),intent(in) :: file_name
    !-Dimensions-----------------------------------------------
    integer,intent(in) :: ndims1,ndims2
    character(len=*),intent(in) :: dim1_name,dim1_units
    character(len=*),intent(in) :: dim2_name,dim2_units
    integer,intent(in) :: dim1(ndims1),dim2(ndims2)
    character(len=maxlen) :: dimname(2),typename(2)
    dimname(1)=trim(dim1_name);typename(1)="integer"
    dimname(2)=trim(dim2_name);typename(2)="integer"
    call WRITENET_def_dim(file_name,2,(/ndims1,ndims2/),dimname,typename)
    call add_var_att(file_name,dimname(1),"units",trim(dim1_units))
    call add_var_att(file_name,dimname(2),"units",trim(dim2_units))
    call WRITENET_wv(file_name,dimname(1),(/1/),(/ndims1/),dim1)
    call WRITENET_wv(file_name,dimname(2),(/1/),(/ndims2/),dim2)
  end subroutine WRITENET_2D_PRE_I
  subroutine WRITENET_2D_PRE_F(file_name,ndims1,ndims2,dim1_name,dim2_name,dim1_units,dim2_units&
       & ,dim1,dim2)
    implicit none
    ! Input data
    integer,parameter :: maxlen=400
    integer,parameter :: idx = 4
    character(len=*),intent(in) :: file_name
    !-Dimensions-----------------------------------------------
    integer,intent(in) :: ndims1,ndims2
    character(len=*),intent(in) :: dim1_name,dim1_units
    character(len=*),intent(in) :: dim2_name,dim2_units
    real(idx),intent(in) :: dim1(ndims1),dim2(ndims2)
    character(len=maxlen) :: dimname(2),typename(2)
    dimname(1)=trim(dim1_name);typename(1)="float"
    dimname(2)=trim(dim2_name);typename(2)="float"
    call WRITENET_def_dim(file_name,2,(/ndims1,ndims2/),dimname,typename)
    call add_var_att(file_name,dimname(1),"units",trim(dim1_units))
    call add_var_att(file_name,dimname(2),"units",trim(dim2_units))
    call WRITENET_wv(file_name,dimname(1),(/1/),(/ndims1/),dim1)
    call WRITENET_wv(file_name,dimname(2),(/1/),(/ndims2/),dim2)
  end subroutine WRITENET_2D_PRE_F
  subroutine WRITENET_2D_PRE_D(file_name,ndims1,ndims2,dim1_name,dim2_name,dim1_units,dim2_units&
       & ,dim1,dim2)
    implicit none
    ! Input data
    integer,parameter :: maxlen=400
    integer,parameter :: idx = 8
    character(len=*),intent(in) :: file_name
    !-Dimensions-----------------------------------------------
    integer,intent(in) :: ndims1,ndims2
    character(len=*),intent(in) :: dim1_name,dim1_units
    character(len=*),intent(in) :: dim2_name,dim2_units
    real(idx),intent(in) :: dim1(ndims1),dim2(ndims2)
    character(len=maxlen) :: dimname(2),typename(2)
    dimname(1)=trim(dim1_name);typename(1)="double"
    dimname(2)=trim(dim2_name);typename(2)="double"
    call WRITENET_def_dim(file_name,2,(/ndims1,ndims2/),dimname,typename)
    call add_var_att(file_name,dimname(1),"units",trim(dim1_units))
    call add_var_att(file_name,dimname(2),"units",trim(dim2_units))
    call WRITENET_wv(file_name,dimname(1),(/1/),(/ndims1/),dim1)
    call WRITENET_wv(file_name,dimname(2),(/1/),(/ndims2/),dim2)
  end subroutine WRITENET_2D_PRE_D
  subroutine WRITENET_3D_PRE_I(file_name,ndims1,ndims2,ndims3,&
       & dim1_name,dim2_name,dim3_name,dim1_units,dim2_units,dim3_units,&
       & dim1,dim2,dim3)
    implicit none
    ! Input data
    integer,parameter :: maxlen=400
    character(len=*),intent(in) :: file_name
    !-Dimensions-----------------------------------------------
    integer,intent(in) :: ndims1,ndims2,ndims3
    character(len=*),intent(in) :: dim1_name,dim1_units
    character(len=*),intent(in) :: dim2_name,dim2_units
    character(len=*),intent(in) :: dim3_name,dim3_units
    integer,intent(in) :: dim1(ndims1),dim2(ndims2),dim3(ndims3)
    character(len=maxlen) :: dimname(3),typename(3)
    dimname(1)=trim(dim1_name);typename(1)="integer"
    dimname(2)=trim(dim2_name);typename(2)="integer"
    dimname(3)=trim(dim3_name);typename(3)="integer"
    call WRITENET_def_dim(file_name,3,(/ndims1,ndims2,ndims3/),dimname,typename)
    call add_var_att(file_name,dimname(1),"units",trim(dim1_units))
    call add_var_att(file_name,dimname(2),"units",trim(dim2_units))
    call add_var_att(file_name,dimname(3),"units",trim(dim3_units))
    call WRITENET_wv(file_name,dimname(1),(/1/),(/ndims1/),dim1)
    call WRITENET_wv(file_name,dimname(2),(/1/),(/ndims2/),dim2)
    call WRITENET_wv(file_name,dimname(3),(/1/),(/ndims3/),dim3)
  end subroutine WRITENET_3D_PRE_I
  subroutine WRITENET_3D_PRE_F(file_name,ndims1,ndims2,ndims3,&
       & dim1_name,dim2_name,dim3_name,dim1_units,dim2_units,dim3_units,&
       & dim1,dim2,dim3)
    implicit none
    ! Input data
    integer,parameter :: maxlen=400,idx=4
    character(len=*),intent(in) :: file_name
    !-Dimensions-----------------------------------------------
    integer,intent(in) :: ndims1,ndims2,ndims3
    character(len=*),intent(in) :: dim1_name,dim1_units
    character(len=*),intent(in) :: dim2_name,dim2_units
    character(len=*),intent(in) :: dim3_name,dim3_units
    real(idx),intent(in) :: dim1(ndims1),dim2(ndims2),dim3(ndims3)
    character(len=maxlen) :: dimname(3),typename(3)
    dimname(1)=trim(dim1_name);typename(1)="float"
    dimname(2)=trim(dim2_name);typename(2)="float"
    dimname(3)=trim(dim3_name);typename(3)="float"
    call WRITENET_def_dim(file_name,3,(/ndims1,ndims2,ndims3/),dimname,typename)
    call add_var_att(file_name,dimname(1),"units",trim(dim1_units))
    call add_var_att(file_name,dimname(2),"units",trim(dim2_units))
    call add_var_att(file_name,dimname(3),"units",trim(dim3_units))
    call WRITENET_wv(file_name,dimname(1),(/1/),(/ndims1/),dim1)
    call WRITENET_wv(file_name,dimname(2),(/1/),(/ndims2/),dim2)
    call WRITENET_wv(file_name,dimname(3),(/1/),(/ndims3/),dim3)
  end subroutine WRITENET_3D_PRE_F
  subroutine WRITENET_3D_PRE_D(file_name,ndims1,ndims2,ndims3,&
       & dim1_name,dim2_name,dim3_name,dim1_units,dim2_units,dim3_units,&
       & dim1,dim2,dim3)
    implicit none
    ! Input data
    integer,parameter :: maxlen=400,idx=8
    character(len=*),intent(in) :: file_name
    !-Dimensions-----------------------------------------------
    integer,intent(in) :: ndims1,ndims2,ndims3
    character(len=*),intent(in) :: dim1_name,dim1_units
    character(len=*),intent(in) :: dim2_name,dim2_units
    character(len=*),intent(in) :: dim3_name,dim3_units
    real(idx),intent(in) :: dim1(ndims1),dim2(ndims2),dim3(ndims3)
    character(len=maxlen) :: dimname(3),typename(3)
    dimname(1)=trim(dim1_name);typename(1)="double"
    dimname(2)=trim(dim2_name);typename(2)="double"
    dimname(3)=trim(dim3_name);typename(3)="double"
    call WRITENET_def_dim(file_name,3,(/ndims1,ndims2,ndims3/),dimname,typename)
    call add_var_att(file_name,dimname(1),"units",trim(dim1_units))
    call add_var_att(file_name,dimname(2),"units",trim(dim2_units))
    call add_var_att(file_name,dimname(3),"units",trim(dim3_units))
    call WRITENET_wv(file_name,dimname(1),(/1/),(/ndims1/),dim1)
    call WRITENET_wv(file_name,dimname(2),(/1/),(/ndims2/),dim2)
    call WRITENET_wv(file_name,dimname(3),(/1/),(/ndims3/),dim3)
  end subroutine WRITENET_3D_PRE_D

    subroutine WRITENET_4D_PRE_I(file_name,ndims1,ndims2,ndims3,ndims4,&
       & dim1_name,dim2_name,dim3_name,dim4_name,dim1_units,dim2_units,dim3_units,dim4_units,&
       & dim1,dim2,dim3,dim4)
    implicit none
    ! Input data
    integer,parameter :: maxlen=400
    character(len=*),intent(in) :: file_name
    !-Dimensions-----------------------------------------------
    integer,intent(in) :: ndims1,ndims2,ndims3,ndims4
    character(len=*),intent(in) :: dim1_name,dim1_units
    character(len=*),intent(in) :: dim2_name,dim2_units
    character(len=*),intent(in) :: dim3_name,dim3_units
    character(len=*),intent(in) :: dim4_name,dim4_units
    integer,intent(in) :: dim1(ndims1),dim2(ndims2),dim3(ndims3),dim4(ndims4)
    character(len=maxlen) :: dimname(4),typename(4)
    dimname(1)=trim(dim1_name);typename(1)="integer"
    dimname(2)=trim(dim2_name);typename(2)="integer"
    dimname(3)=trim(dim3_name);typename(3)="integer"
    dimname(4)=trim(dim4_name);typename(4)="integer"
    call WRITENET_def_dim(file_name,4,(/ndims1,ndims2,ndims3,ndims4/),dimname,typename)
    call add_var_att(file_name,dimname(1),"units",trim(dim1_units))
    call add_var_att(file_name,dimname(2),"units",trim(dim2_units))
    call add_var_att(file_name,dimname(3),"units",trim(dim3_units))
    call add_var_att(file_name,dimname(4),"units",trim(dim4_units))
    call WRITENET_wv(file_name,dimname(1),(/1/),(/ndims1/),dim1)
    call WRITENET_wv(file_name,dimname(2),(/1/),(/ndims2/),dim2)
    call WRITENET_wv(file_name,dimname(3),(/1/),(/ndims3/),dim3)
    call WRITENET_wv(file_name,dimname(4),(/1/),(/ndims4/),dim4)
  end subroutine WRITENET_4D_PRE_I
  subroutine WRITENET_4D_PRE_F(file_name,ndims1,ndims2,ndims3,ndims4,&
       & dim1_name,dim2_name,dim3_name,dim4_name,dim1_units,dim2_units,dim3_units,dim4_units,&
       & dim1,dim2,dim3,dim4)
    implicit none
    ! Input data
    integer,parameter :: maxlen=400,idx=4
    character(len=*),intent(in) :: file_name
    !-Dimensions-----------------------------------------------
    integer,intent(in) :: ndims1,ndims2,ndims3,ndims4
    character(len=*),intent(in) :: dim1_name,dim1_units
    character(len=*),intent(in) :: dim2_name,dim2_units
    character(len=*),intent(in) :: dim3_name,dim3_units
    character(len=*),intent(in) :: dim4_name,dim4_units
    real(idx),intent(in) :: dim1(ndims1),dim2(ndims2),dim3(ndims3),dim4(ndims4)
    character(len=maxlen) :: dimname(4),typename(4)
    dimname(1)=trim(dim1_name);typename(1)="float"
    dimname(2)=trim(dim2_name);typename(2)="float"
    dimname(3)=trim(dim3_name);typename(3)="float"
    dimname(4)=trim(dim4_name);typename(4)="float"
    call WRITENET_def_dim(file_name,4,(/ndims1,ndims2,ndims3,ndims4/),dimname,typename)
    call add_var_att(file_name,dimname(1),"units",trim(dim1_units))
    call add_var_att(file_name,dimname(2),"units",trim(dim2_units))
    call add_var_att(file_name,dimname(3),"units",trim(dim3_units))
    call add_var_att(file_name,dimname(4),"units",trim(dim4_units))
    call WRITENET_wv(file_name,dimname(1),(/1/),(/ndims1/),dim1)
    call WRITENET_wv(file_name,dimname(2),(/1/),(/ndims2/),dim2)
    call WRITENET_wv(file_name,dimname(3),(/1/),(/ndims3/),dim3)
    call WRITENET_wv(file_name,dimname(4),(/1/),(/ndims4/),dim4)
  end subroutine WRITENET_4D_PRE_F

  subroutine WRITENET_4D_PRE_D(file_name,ndims1,ndims2,ndims3,ndims4,&
       & dim1_name,dim2_name,dim3_name,dim4_name,dim1_units,dim2_units,dim3_units,dim4_units,&
       & dim1,dim2,dim3,dim4)
    implicit none
    ! Input data
    integer,parameter :: maxlen=400,idx=8
    character(len=*),intent(in) :: file_name
    !-Dimensions-----------------------------------------------
    integer,intent(in) :: ndims1,ndims2,ndims3,ndims4
    character(len=*),intent(in) :: dim1_name,dim1_units
    character(len=*),intent(in) :: dim2_name,dim2_units
    character(len=*),intent(in) :: dim3_name,dim3_units
    character(len=*),intent(in) :: dim4_name,dim4_units
    real(idx),intent(in) :: dim1(ndims1),dim2(ndims2),dim3(ndims3),dim4(ndims4)
    character(len=maxlen) :: dimname(4),typename(4)
    dimname(1)=trim(dim1_name);typename(1)="double"
    dimname(2)=trim(dim2_name);typename(2)="double"
    dimname(3)=trim(dim3_name);typename(3)="double"
    dimname(4)=trim(dim4_name);typename(4)="double"
    call WRITENET_def_dim(file_name,4,(/ndims1,ndims2,ndims3,ndims4/),dimname,typename)
    call add_var_att(file_name,dimname(1),"units",trim(dim1_units))
    call add_var_att(file_name,dimname(2),"units",trim(dim2_units))
    call add_var_att(file_name,dimname(3),"units",trim(dim3_units))
    call add_var_att(file_name,dimname(4),"units",trim(dim4_units))
    call WRITENET_wv(file_name,dimname(1),(/1/),(/ndims1/),dim1)
    call WRITENET_wv(file_name,dimname(2),(/1/),(/ndims2/),dim2)
    call WRITENET_wv(file_name,dimname(3),(/1/),(/ndims3/),dim3)
    call WRITENET_wv(file_name,dimname(4),(/1/),(/ndims4/),dim4)
  end subroutine WRITENET_4D_PRE_D
  subroutine WRITENET_5D_PRE_I(file_name,ndims1,ndims2,ndims3,ndims4,ndims5,&
       & dim1_name,dim2_name,dim3_name,dim4_name,dim5_name,&
       & dim1_units,dim2_units,dim3_units,dim4_units,dim5_units,&
       & dim1,dim2,dim3,dim4,dim5)
    implicit none
    ! Input data
    integer,parameter :: maxlen=400
    character(len=*),intent(in) :: file_name
    !-Dimensions-----------------------------------------------
    integer,intent(in) :: ndims1,ndims2,ndims3,ndims4,ndims5
    character(len=*),intent(in) :: dim1_name,dim1_units
    character(len=*),intent(in) :: dim2_name,dim2_units
    character(len=*),intent(in) :: dim3_name,dim3_units
    character(len=*),intent(in) :: dim4_name,dim4_units
    character(len=*),intent(in) :: dim5_name,dim5_units
    integer,intent(in) :: dim1(ndims1),dim2(ndims2),dim3(ndims3),dim4(ndims4),dim5(ndims5)
    character(len=maxlen) :: dimname(5),typename(5)
    dimname(1)=trim(dim1_name);typename(1)="integer"
    dimname(2)=trim(dim2_name);typename(2)="integer"
    dimname(3)=trim(dim3_name);typename(3)="integer"
    dimname(4)=trim(dim4_name);typename(4)="integer"
    dimname(5)=trim(dim5_name);typename(5)="integer"
    call WRITENET_def_dim(file_name,5,(/ndims1,ndims2,ndims3,ndims4,ndims5/),dimname,typename)
    call add_var_att(file_name,dimname(1),"units",trim(dim1_units))
    call add_var_att(file_name,dimname(2),"units",trim(dim2_units))
    call add_var_att(file_name,dimname(3),"units",trim(dim3_units))
    call add_var_att(file_name,dimname(4),"units",trim(dim4_units))
    call add_var_att(file_name,dimname(5),"units",trim(dim5_units))
    call WRITENET_wv(file_name,dimname(1),(/1/),(/ndims1/),dim1)
    call WRITENET_wv(file_name,dimname(2),(/1/),(/ndims2/),dim2)
    call WRITENET_wv(file_name,dimname(3),(/1/),(/ndims3/),dim3)
    call WRITENET_wv(file_name,dimname(4),(/1/),(/ndims4/),dim4)
    call WRITENET_wv(file_name,dimname(5),(/1/),(/ndims5/),dim5)
  end subroutine WRITENET_5D_PRE_I
  subroutine WRITENET_5D_PRE_F(file_name,ndims1,ndims2,ndims3,ndims4,ndims5,&
       & dim1_name,dim2_name,dim3_name,dim4_name,dim5_name,&
       & dim1_units,dim2_units,dim3_units,dim4_units,dim5_units,&
       & dim1,dim2,dim3,dim4,dim5)
    implicit none
    ! Input data
    integer,parameter :: maxlen=400,idx=4
    character(len=*),intent(in) :: file_name
    !-Dimensions-----------------------------------------------
    integer,intent(in) :: ndims1,ndims2,ndims3,ndims4,ndims5
    character(len=*),intent(in) :: dim1_name,dim1_units
    character(len=*),intent(in) :: dim2_name,dim2_units
    character(len=*),intent(in) :: dim3_name,dim3_units
    character(len=*),intent(in) :: dim4_name,dim4_units
    character(len=*),intent(in) :: dim5_name,dim5_units
    real(idx),intent(in) :: dim1(ndims1),dim2(ndims2),dim3(ndims3),dim4(ndims4),dim5(ndims5)
    character(len=maxlen) :: dimname(5),typename(5)
    dimname(1)=trim(dim1_name);typename(1)="float"
    dimname(2)=trim(dim2_name);typename(2)="float"
    dimname(3)=trim(dim3_name);typename(3)="float"
    dimname(4)=trim(dim4_name);typename(4)="float"
    dimname(5)=trim(dim5_name);typename(5)="float"
    call WRITENET_def_dim(file_name,5,(/ndims1,ndims2,ndims3,ndims4,ndims5/),dimname,typename)
    call add_var_att(file_name,dimname(1),"units",trim(dim1_units))
    call add_var_att(file_name,dimname(2),"units",trim(dim2_units))
    call add_var_att(file_name,dimname(3),"units",trim(dim3_units))
    call add_var_att(file_name,dimname(4),"units",trim(dim4_units))
    call add_var_att(file_name,dimname(5),"units",trim(dim5_units))
    call WRITENET_wv(file_name,dimname(1),(/1/),(/ndims1/),dim1)
    call WRITENET_wv(file_name,dimname(2),(/1/),(/ndims2/),dim2)
    call WRITENET_wv(file_name,dimname(3),(/1/),(/ndims3/),dim3)
    call WRITENET_wv(file_name,dimname(4),(/1/),(/ndims4/),dim4)
    call WRITENET_wv(file_name,dimname(5),(/1/),(/ndims5/),dim5)
  end subroutine WRITENET_5D_PRE_F
  subroutine WRITENET_5D_PRE_D(file_name,ndims1,ndims2,ndims3,ndims4,ndims5,&
       & dim1_name,dim2_name,dim3_name,dim4_name,dim5_name,&
       & dim1_units,dim2_units,dim3_units,dim4_units,dim5_units,&
       & dim1,dim2,dim3,dim4,dim5)
    implicit none
    ! Input data
    integer,parameter :: maxlen=400,idx=8
    character(len=*),intent(in) :: file_name
    !-Dimensions-----------------------------------------------
    integer,intent(in) :: ndims1,ndims2,ndims3,ndims4,ndims5
    character(len=*),intent(in) :: dim1_name,dim1_units
    character(len=*),intent(in) :: dim2_name,dim2_units
    character(len=*),intent(in) :: dim3_name,dim3_units
    character(len=*),intent(in) :: dim4_name,dim4_units
    character(len=*),intent(in) :: dim5_name,dim5_units
    real(idx),intent(in) :: dim1(ndims1),dim2(ndims2),dim3(ndims3),dim4(ndims4),dim5(ndims5)
    character(len=maxlen) :: dimname(5),typename(5)
    dimname(1)=trim(dim1_name);typename(1)="double"
    dimname(2)=trim(dim2_name);typename(2)="double"
    dimname(3)=trim(dim3_name);typename(3)="double"
    dimname(4)=trim(dim4_name);typename(4)="double"
    dimname(5)=trim(dim5_name);typename(5)="double"
    call WRITENET_def_dim(file_name,5,(/ndims1,ndims2,ndims3,ndims4,ndims5/),dimname,typename)
    call add_var_att(file_name,dimname(1),"units",trim(dim1_units))
    call add_var_att(file_name,dimname(2),"units",trim(dim2_units))
    call add_var_att(file_name,dimname(3),"units",trim(dim3_units))
    call add_var_att(file_name,dimname(4),"units",trim(dim4_units))
    call add_var_att(file_name,dimname(5),"units",trim(dim5_units))
    call WRITENET_wv(file_name,dimname(1),(/1/),(/ndims1/),dim1)
    call WRITENET_wv(file_name,dimname(2),(/1/),(/ndims2/),dim2)
    call WRITENET_wv(file_name,dimname(3),(/1/),(/ndims3/),dim3)
    call WRITENET_wv(file_name,dimname(4),(/1/),(/ndims4/),dim4)
    call WRITENET_wv(file_name,dimname(5),(/1/),(/ndims5/),dim5)
  end subroutine WRITENET_5D_PRE_D
  ! *********************************
  ! subrotines for define variables *
  ! *********************************
  subroutine WRITENET_1D_DV_I(file_name,dim1_name,nvar,vars_names,vars_units,vars_miss)
    implicit none
    integer,parameter :: maxlen=400
    character(len=*),intent(in) :: file_name,dim1_name
    integer,intent(in) :: nvar
    character(len=*),intent(in) :: vars_names(nvar),vars_units(nvar)
    integer,intent(in) :: vars_miss
    integer,parameter :: total_dim=1
    integer :: ivar
    character(len=maxlen) :: dimname(total_dim),dtype(nvar)
    dimname(1)=trim(dim1_name)
    do ivar=1,nvar
       dtype(ivar)="integer"
    end do
    call WRITENET_def_var(file_name,1,nvar,dimname,vars_names,dtype)
    do ivar=1,nvar
       call add_var_att(file_name,vars_names(ivar),"units",vars_units(ivar))
       call add_var_att(file_name,vars_names(ivar),"missing_value",vars_miss)
    end do
  end subroutine WRITENET_1D_DV_I
  subroutine WRITENET_1D_DV_F(file_name,dim1_name,nvar,vars_names,vars_units,vars_miss)
    implicit none
    integer,parameter :: idx=4,maxlen=400
    character(len=*),intent(in) :: file_name,dim1_name
    integer,intent(in) :: nvar
    character(len=*),intent(in) :: vars_names(nvar),vars_units(nvar)
    real(idx),intent(in) :: vars_miss
    integer,parameter :: total_dim=1
    character(len=maxlen) :: dimname(total_dim),dtype(nvar)
    integer :: ivar
    dimname(1)=trim(dim1_name)
    do ivar=1,nvar
       dtype(ivar)="float"
    end do
    call WRITENET_def_var(file_name,1,nvar,dimname,vars_names,dtype)
    do ivar=1,nvar
       call add_var_att(file_name,vars_names(ivar),"units",vars_units(ivar))
       call add_var_att(file_name,vars_names(ivar),"missing_value",vars_miss)
    end do
  end subroutine WRITENET_1D_DV_F
  subroutine WRITENET_1D_DV_D(file_name,dim1_name,nvar,vars_names,vars_units,vars_miss)
    implicit none
    integer,parameter :: idx=8,maxlen=400
    character(len=*),intent(in) :: file_name,dim1_name
    integer,intent(in) :: nvar
    character(len=*),intent(in) :: vars_names(nvar),vars_units(nvar)
    real(idx),intent(in) :: vars_miss
    integer,parameter :: total_dim=1
    integer :: ivar
    character(len=maxlen) :: dimname(total_dim),dtype(nvar)
    dimname(1)=trim(dim1_name)
    do ivar=1,nvar
       dtype(ivar)="double"
    end do
    call WRITENET_def_var(file_name,1,nvar,dimname,vars_names,dtype)
    do ivar=1,nvar
       call add_var_att(file_name,vars_names(ivar),"units",vars_units(ivar))
       call add_var_att(file_name,vars_names(ivar),"missing_value",vars_miss)
    end do
  end subroutine WRITENET_1D_DV_D
  subroutine WRITENET_2D_DV_I(file_name,dim1_name,dim2_name,nvar,vars_names,vars_units,vars_miss)
    implicit none
    integer,parameter :: maxlen=400
    character(len=*),intent(in) :: file_name,dim1_name,dim2_name
    integer,intent(in) :: nvar
    character(len=*),intent(in) :: vars_names(nvar),vars_units(nvar)
    integer,intent(in) :: vars_miss
    integer,parameter :: total_dim=2
    character(len=maxlen) :: dimname(total_dim),dtype(nvar)
    integer :: ivar
    dimname(1)=trim(dim1_name)
    dimname(2)=trim(dim2_name)
    do ivar=1,nvar
       dtype(ivar)="integer"
    end do
    call WRITENET_def_var(file_name,2,nvar,dimname,vars_names,dtype)
    do ivar=1,nvar
       call add_var_att(file_name,vars_names(ivar),"units",vars_units(ivar))
       call add_var_att(file_name,vars_names(ivar),"missing_value",vars_miss)
    end do
  end subroutine WRITENET_2D_DV_I
  subroutine WRITENET_2D_DV_F(file_name,dim1_name,dim2_name,nvar,vars_names,vars_units,vars_miss)
    implicit none
    integer,parameter :: idx=4,maxlen=400
    character(len=*),intent(in) :: file_name,dim1_name,dim2_name
    integer,intent(in) :: nvar
    character(len=*),intent(in) :: vars_names(nvar),vars_units(nvar)
    real(idx),intent(in) :: vars_miss
    integer,parameter :: total_dim=2
    character(len=maxlen) :: dimname(total_dim),dtype(nvar)
    integer :: ivar
    dimname(1)=trim(dim1_name)
    dimname(2)=trim(dim2_name)
    do ivar=1,nvar
       dtype(ivar)="float"
    end do
    call WRITENET_def_var(file_name,2,nvar,dimname,vars_names,dtype)
    do ivar=1,nvar
       call add_var_att(file_name,vars_names(ivar),"units",vars_units(ivar))
       call add_var_att(file_name,vars_names(ivar),"missing_value",vars_miss)
    end do
  end subroutine WRITENET_2D_DV_F
  subroutine WRITENET_2D_DV_D(file_name,dim1_name,dim2_name,nvar,vars_names,vars_units,vars_miss)
    implicit none
    integer,parameter :: idx=8,maxlen=400
    character(len=*),intent(in) :: file_name,dim1_name,dim2_name
    integer,intent(in) :: nvar
    character(len=*),intent(in) :: vars_names(nvar),vars_units(nvar)
    real(idx),intent(in) :: vars_miss
    integer,parameter :: total_dim=2
    character(len=maxlen) :: dimname(total_dim),dtype(nvar)
    integer :: ivar
    dimname(1)=trim(dim1_name)
    dimname(2)=trim(dim2_name)
    do ivar=1,nvar
       dtype(ivar)="double"
    end do
    call WRITENET_def_var(file_name,2,nvar,dimname,vars_names,dtype)
    do ivar=1,nvar
       call add_var_att(file_name,vars_names(ivar),"units",vars_units(ivar))
       call add_var_att(file_name,vars_names(ivar),"missing_value",vars_miss)
    end do
  end subroutine WRITENET_2D_DV_D
  subroutine WRITENET_3D_DV_I(file_name,dim1_name,dim2_name,dim3_name, &
       & nvar,vars_names,vars_units,vars_miss)
    implicit none
    integer,parameter :: maxlen=400
    character(len=*),intent(in) :: file_name,dim1_name,dim2_name,dim3_name
    integer,intent(in) :: nvar
    character(len=*),intent(in) :: vars_names(nvar),vars_units(nvar)
    integer,intent(in) :: vars_miss
    integer,parameter :: total_dim=3
    character(len=maxlen) :: dimname(total_dim),dtype(nvar)
    integer :: ivar
    dimname(1)=trim(dim1_name)
    dimname(2)=trim(dim2_name)
    dimname(3)=trim(dim3_name)
    do ivar=1,nvar
       dtype(ivar)="integer"
    end do
    call WRITENET_def_var(file_name,3,nvar,dimname,vars_names,dtype)
    do ivar=1,nvar
       call add_var_att(file_name,vars_names(ivar),"units",vars_units(ivar))
       call add_var_att(file_name,vars_names(ivar),"missing_value",vars_miss)
    end do
  end subroutine WRITENET_3D_DV_I
  subroutine WRITENET_3D_DV_F(file_name,dim1_name,dim2_name,dim3_name, &
       & nvar,vars_names,vars_units,vars_miss)
    implicit none
    integer,parameter :: idx=4,maxlen=400
    character(len=*),intent(in) :: file_name,dim1_name,dim2_name,dim3_name
    integer,intent(in) :: nvar
    character(len=*),intent(in) :: vars_names(nvar),vars_units(nvar)
    real(idx),intent(in) :: vars_miss
    integer,parameter :: total_dim=3
    character(len=maxlen) :: dimname(total_dim),dtype(nvar)
    integer :: ivar
    dimname(1)=trim(dim1_name)
    dimname(2)=trim(dim2_name)
    dimname(3)=trim(dim3_name)
    do ivar=1,nvar
       dtype(ivar)="float"
    end do
    call WRITENET_def_var(file_name,3,nvar,dimname,vars_names,dtype)
    do ivar=1,nvar
       call add_var_att(file_name,vars_names(ivar),"units",vars_units(ivar))
       call add_var_att(file_name,vars_names(ivar),"missing_value",vars_miss)
    end do
  end subroutine WRITENET_3D_DV_F
  subroutine WRITENET_3D_DV_D(file_name,dim1_name,dim2_name,dim3_name, &
       & nvar,vars_names,vars_units,vars_miss)
    implicit none
    integer,parameter :: idx=8,maxlen=400
    character(len=*),intent(in) :: file_name,dim1_name,dim2_name,dim3_name
    integer,intent(in) :: nvar
    character(len=*),intent(in) :: vars_names(nvar),vars_units(nvar)
    real(idx),intent(in) :: vars_miss
      integer,parameter :: total_dim=3
    character(len=maxlen) :: dimname(total_dim),dtype(nvar)
    integer :: ivar
    dimname(1)=trim(dim1_name)
    dimname(2)=trim(dim2_name)
    dimname(3)=trim(dim3_name)
    do ivar=1,nvar
       dtype(ivar)="double"
    end do
    call WRITENET_def_var(file_name,3,nvar,dimname,vars_names,dtype)
    do ivar=1,nvar
       call add_var_att(file_name,vars_names(ivar),"units",vars_units(ivar))
       call add_var_att(file_name,vars_names(ivar),"missing_value",vars_miss)
    end do
  end subroutine WRITENET_3D_DV_D
  subroutine WRITENET_4D_DV_I(file_name,dim1_name,dim2_name,dim3_name,dim4_name, &
       & nvar,vars_names,vars_units,vars_miss)
    implicit none
    integer,parameter :: maxlen=400
    character(len=*),intent(in) :: file_name,dim1_name,dim2_name,dim3_name,dim4_name
    integer,intent(in) :: nvar
    character(len=*),intent(in) :: vars_names(nvar),vars_units(nvar)
    integer,intent(in) :: vars_miss
    integer,parameter :: total_dim=4
    character(len=maxlen) :: dimname(total_dim),dtype(nvar)
    integer :: ivar
    dimname(1)=trim(dim1_name)
    dimname(2)=trim(dim2_name)
    dimname(3)=trim(dim3_name)
    dimname(4)=trim(dim4_name)
    do ivar=1,nvar
       dtype(ivar)="integer"
    end do
    call WRITENET_def_var(file_name,4,nvar,dimname,vars_names,dtype)
    do ivar=1,nvar
       call add_var_att(file_name,vars_names(ivar),"units",vars_units(ivar))
       call add_var_att(file_name,vars_names(ivar),"missing_value",vars_miss)
    end do
  end subroutine WRITENET_4D_DV_I
  subroutine WRITENET_4D_DV_F(file_name,dim1_name,dim2_name,dim3_name,dim4_name, &
       & nvar,vars_names,vars_units,vars_miss)
    implicit none
    integer,parameter :: idx=4,maxlen=400
    character(len=*),intent(in) :: file_name,dim1_name,dim2_name,dim3_name,dim4_name
    integer,intent(in) :: nvar
    character(len=*),intent(in) :: vars_names(nvar),vars_units(nvar)
    real(idx),intent(in) :: vars_miss
    integer,parameter :: total_dim=4
    character(len=maxlen) :: dimname(total_dim),dtype(nvar)
    integer :: ivar
    dimname(1)=trim(dim1_name)
    dimname(2)=trim(dim2_name)
    dimname(3)=trim(dim3_name)
    dimname(4)=trim(dim4_name)
    do ivar=1,nvar
       dtype(ivar)="float"
    end do
    call WRITENET_def_var(file_name,4,nvar,dimname,vars_names,dtype)
    do ivar=1,nvar
       call add_var_att(file_name,vars_names(ivar),"units",vars_units(ivar))
       call add_var_att(file_name,vars_names(ivar),"missing_value",vars_miss)
    end do
  end subroutine WRITENET_4D_DV_F
  subroutine WRITENET_4D_DV_D(file_name,dim1_name,dim2_name,dim3_name,dim4_name, &
       & nvar,vars_names,vars_units,vars_miss)
    implicit none
    integer,parameter :: idx=8,maxlen=400
    character(len=*),intent(in) :: file_name,dim1_name,dim2_name,dim3_name,dim4_name
    integer,intent(in) :: nvar
    character(len=*),intent(in) :: vars_names(nvar),vars_units(nvar)
    real(idx),intent(in) :: vars_miss
    integer,parameter :: total_dim=4
    character(len=maxlen) :: dimname(total_dim),dtype(nvar)
    integer :: ivar
    dimname(1)=trim(dim1_name)
    dimname(2)=trim(dim2_name)
    dimname(3)=trim(dim3_name)
    dimname(4)=trim(dim4_name)
    do ivar=1,nvar
       dtype(ivar)="double"
    end do
    call WRITENET_def_var(file_name,4,nvar,dimname,vars_names,dtype)
    do ivar=1,nvar
       call add_var_att(file_name,vars_names(ivar),"units",vars_units(ivar))
       call add_var_att(file_name,vars_names(ivar),"missing_value",vars_miss)
    end do
  end subroutine WRITENET_4D_DV_D
  subroutine WRITENET_5D_DV_I(file_name,dim1_name,dim2_name,dim3_name,dim4_name,dim5_name, &
       & nvar,vars_names,vars_units,vars_miss)
    implicit none
    integer,parameter :: maxlen=400
    character(len=*),intent(in) :: file_name,dim1_name,dim2_name,dim3_name,dim4_name,dim5_name
    integer,intent(in) :: nvar
    character(len=*),intent(in) :: vars_names(nvar),vars_units(nvar)
    integer,intent(in) :: vars_miss
    integer,parameter :: total_dim=5
    character(len=maxlen) :: dimname(total_dim),dtype(nvar)
    integer :: ivar
    dimname(1)=trim(dim1_name)
    dimname(2)=trim(dim2_name)
    dimname(3)=trim(dim3_name)
    dimname(4)=trim(dim4_name)
    dimname(5)=trim(dim5_name)
    do ivar=1,nvar
       dtype(ivar)="integer"
    end do
    call WRITENET_def_var(file_name,5,nvar,dimname,vars_names,dtype)
    do ivar=1,nvar
       call add_var_att(file_name,vars_names(ivar),"units",vars_units(ivar))
       call add_var_att(file_name,vars_names(ivar),"missing_value",vars_miss)
    end do
  end subroutine WRITENET_5D_DV_I
  subroutine WRITENET_5D_DV_F(file_name,dim1_name,dim2_name,dim3_name,dim4_name,dim5_name, &
       & nvar,vars_names,vars_units,vars_miss)
    implicit none
    integer,parameter :: maxlen=400,idx=4
    character(len=*),intent(in) :: file_name,dim1_name,dim2_name,dim3_name,dim4_name,dim5_name
    integer,intent(in) :: nvar
    character(len=*),intent(in) :: vars_names(nvar),vars_units(nvar)
    real(idx),intent(in) :: vars_miss
    integer,parameter :: total_dim=5
    character(len=maxlen) :: dimname(total_dim),dtype(nvar)
    integer :: ivar
    dimname(1)=trim(dim1_name)
    dimname(2)=trim(dim2_name)
    dimname(3)=trim(dim3_name)
    dimname(4)=trim(dim4_name)
    dimname(5)=trim(dim5_name)
    do ivar=1,nvar
       dtype(ivar)="float"
    end do
    call WRITENET_def_var(file_name,5,nvar,dimname,vars_names,dtype)
    do ivar=1,nvar
       call add_var_att(file_name,vars_names(ivar),"units",vars_units(ivar))
       call add_var_att(file_name,vars_names(ivar),"missing_value",vars_miss)
    end do
  end subroutine WRITENET_5D_DV_F
  subroutine WRITENET_5D_DV_D(file_name,dim1_name,dim2_name,dim3_name,dim4_name,dim5_name, &
       & nvar,vars_names,vars_units,vars_miss)
    implicit none
    integer,parameter :: maxlen=400,idx=8
    character(len=*),intent(in) :: file_name,dim1_name,dim2_name,dim3_name,dim4_name,dim5_name
    integer,intent(in) :: nvar
    character(len=*),intent(in) :: vars_names(nvar),vars_units(nvar)
    real(idx),intent(in) :: vars_miss
    integer,parameter :: total_dim=5
    character(len=maxlen) :: dimname(total_dim),dtype(nvar)
    integer :: ivar
    dimname(1)=trim(dim1_name)
    dimname(2)=trim(dim2_name)
    dimname(3)=trim(dim3_name)
    dimname(4)=trim(dim4_name)
    dimname(5)=trim(dim5_name)
    do ivar=1,nvar
       dtype(ivar)="double"
    end do
    call WRITENET_def_var(file_name,5,nvar,dimname,vars_names,dtype)
    do ivar=1,nvar
       call add_var_att(file_name,vars_names(ivar),"units",vars_units(ivar))
       call add_var_att(file_name,vars_names(ivar),"missing_value",vars_miss)
    end do
  end subroutine WRITENET_5D_DV_D
  ! function create_time_att(start_yymmdd,int_yymmdd) result(time_unit)
  !   implicit none
  !   integer,intent(in) ::start_yymmdd,int_yymmdd
  !   integer :: start_year,start_month,start_day
  !   character :: year_ind*4,month_ind*2,day_ind*2
  !   character :: ref_unit*5
  !   character :: time_unit*100
  !   start_year=start_yymmdd/10000
  !   start_month=start_yymmdd/100-start_year*100
  !   start_day=start_yymmdd-start_year*10000-start_month*100
  !   write(year_ind,'(i4.4)') start_year
  !   write(month_ind,'(i2.2)') start_month
  !   write(day_ind,'(i2.2)') start_day
  !   ! ref unit
  !   if (int_yymmdd .ge. 10000) then
  !      ref_unit="years"
  !   else if (int_yymmdd .ge. 100) then
  !      ref_unit="months"
  !   else
  !      ref_unit="days"
  !   end if
  !   time_unit=trim(ref_unit)//" since "//year_ind//"-"//month_ind//"-"//day_ind//" 00:00:00"
  ! end function create_time_att
END MODULE NCDF_WRITE

