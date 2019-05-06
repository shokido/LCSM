module NCDF_WRITE
  implicit none
  private
  !------------------------------------------------------------
  ! Preparation
  interface WRITENET_PRE
     module procedure WRITENET_1D_PRE,DWRITENET_1D_PRE,WRITENET_2D_PRE,DWRITENET_2D_PRE,&
          & WRITENET_3D_PRE,DWRITENET_3D_PRE,WRITENET_4D_PRE,DWRITENET_4D_PRE
  end interface WRITENET_PRE
  !------------------------------------------------------------
  interface WRITENET_DV
     module procedure WRITENET_1D_DV,DWRITENET_1D_DV,WRITENET_2D_DV,DWRITENET_2D_DV,&
          & WRITENET_3D_DV,DWRITENET_3D_DV,WRITENET_4D_DV,DWRITENET_4D_DV
  end interface WRITENET_DV 
  interface WRITENET_WV
     module procedure WRITENET_1D_WV,DWRITENET_1D_WV,WRITENET_2D_WV,DWRITENET_2D_WV,&
          & WRITENET_3D_WV,DWRITENET_3D_WV,WRITENET_4D_WV,DWRITENET_4D_WV
  end interface WRITENET_WV
  !------------------------------------------------------------
  public :: CHECK_W
  public :: WRITENET_PRE,WRITENET_DV,WRITENET_WV
  public :: WRITENET_DD
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
  !==================================
  ! Subroutine for define dimensions
  !==================================
  subroutine WRITENET_1D_PRE(file_name,ndims1,dim1_name,dim1_units,dim1)
    use NETCDF
    implicit none
    ! Input data-----------------------------------------------
    integer,parameter :: idx = 4
    character(len=*),intent(in) :: file_name
    !-Dimensions-----------------------------------------------
    integer,intent(in) :: ndims1
    character(len=*),intent(in) :: dim1_name,dim1_units
    real(idx),intent(in) :: dim1(ndims1)
    !-Variables------------------------------------------------
    character(len=*),parameter :: units="units"
    integer :: ncid
    integer :: dim1_dimid,dim1_varid
    ! create file
    call CHECK_W(NF90_CREATE(trim(file_name),or(nf90_clobber,nf90_64bit_offset), ncid) )
    ! Define the dimensions
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim1_name), ndims1,dim1_dimid))
    call CHECK_W(NF90_DEF_VAR(ncid, trim(dim1_name), nf90_float,dim1_dimid,dim1_varid))
    call CHECK_W(NF90_PUT_ATT(ncid, dim1_varid,units, trim(dim1_units)))
    call CHECK_W(NF90_ENDDEF(ncid))
    ! Put data
    call CHECK_W( NF90_PUT_VAR(ncid,dim1_varid,dim1))
    ! close file
    call CHECK_W( NF90_CLOSE(ncid))
  end subroutine WRITENET_1D_PRE
  subroutine DWRITENET_1D_PRE(file_name,ndims1,dim1_name,dim1_units,dim1)
    use NETCDF
    implicit none
    ! Input data
    integer,parameter :: idx = 8
    character(len=*),intent(in) :: file_name
    !-Dimensions-----------------------------------------------
    integer,intent(in) :: ndims1
    character(len=*),intent(in) :: dim1_name,dim1_units
    real(idx),intent(in) :: dim1(ndims1)
    !-Variables-----------------------------------
    character(len=*),parameter :: units="units"
    integer :: ncid
    ! prepare dimids
    integer :: dim1_dimid,dim1_varid
    ! create file
    call CHECK_W(NF90_CREATE(trim(file_name),or(NF90_CLOBBER,NF90_64BIT_OFFSET), ncid) )
    ! Define the dimensions
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim1_name), ndims1,dim1_dimid))
    call CHECK_W(NF90_DEF_VAR(ncid, trim(dim1_name), NF90_DOUBLE,dim1_dimid,dim1_varid))
    call CHECK_W(NF90_PUT_ATT(ncid, dim1_varid,units, trim(dim1_units)))
    call CHECK_W(NF90_ENDDEF(ncid))
    ! Put data
    call CHECK_W(NF90_PUT_VAR(ncid,dim1_varid,dim1))
    ! close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine DWRITENET_1D_PRE
  subroutine WRITENET_2D_PRE(file_name,ndims1,ndims2,dim1_name,dim2_name,dim1_units,dim2_units&
       & ,dim1,dim2)
    use NETCDF
    implicit none
    ! Input data
    integer,parameter :: idx = 4
    character(len=*),intent(in) :: file_name
    !-Dimensions-----------------------------------------------
    integer,intent(in) :: ndims1,ndims2
    character(len=*),intent(in) :: dim1_name,dim1_units
    character(len=*),intent(in) :: dim2_name,dim2_units
    real(idx),intent(in) :: dim1(ndims1),dim2(ndims2)
    !-Variables-----------------------------------
    character(len=*),parameter :: units="units",axis="axis"
    integer :: ncid
    ! prepare dimids
    integer :: dim1_dimid,dim1_varid,dim2_dimid,dim2_varid
    ! create file
    ! Define the dimensions
    if (trim(dim1_name) .eq. "") then
       call CHECK_W(NF90_DEF_DIM(ncid,"lon",ndims1,dim1_dimid))
    else
       call CHECK_W(NF90_DEF_DIM(ncid,trim(dim1_name),ndims1,dim1_dimid))
    end if
    if (trim(dim2_name) .eq. "") then
       call CHECK_W(NF90_DEF_DIM(ncid,trim(dim1_name),ndims2,dim2_dimid))
    else
       call CHECK_W(NF90_DEF_DIM(ncid,trim(dim2_name),ndims2,dim2_dimid))
    end if
    call CHECK_W(NF90_DEF_VAR(ncid,trim(dim1_name),NF90_FLOAT,dim1_dimid,dim1_varid))
    call CHECK_W(NF90_DEF_VAR(ncid,trim(dim2_name),NF90_FLOAT,dim2_dimid,dim2_varid))
    if (trim(dim1_units) .eq. "") then
       call CHECK_W(NF90_PUT_ATT(ncid,dim1_varid,units,"degrees_east"))
    else
       call CHECK_W(NF90_PUT_ATT(ncid,dim1_varid,units,trim(dim1_units)))
    end if
    if (trim(dim2_units) .eq. "") then
       call CHECK_W(NF90_PUT_ATT(ncid,dim2_varid,units,"degrees_north"))
    else
       call CHECK_W(NF90_PUT_ATT(ncid,dim2_varid,units,trim(dim2_units)))
    end if
    call CHECK_W(NF90_PUT_ATT(ncid, dim1_varid,axis,"X"))
    call CHECK_W(NF90_PUT_ATT(ncid, dim2_varid,axis,"Y"))
    call CHECK_W(NF90_ENDDEF(ncid))
    ! Put data
    call CHECK_W(NF90_PUT_VAR(ncid,dim1_varid,dim1))
    call CHECK_W(NF90_PUT_VAR(ncid,dim2_varid,dim2))
    ! close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine WRITENET_2D_PRE
  subroutine DWRITENET_2D_PRE(file_name,ndims1,ndims2,dim1_name,dim2_name,dim1_units,dim2_units&
       & ,dim1,dim2)
    use NETCDF
    implicit none
    integer,parameter :: idx = 8
    character(len=*),intent(in) :: file_name
    integer,intent(in) :: ndims1,ndims2
    character(len=*),intent(in) :: dim1_name,dim1_units
    character(len=*),intent(in) :: dim2_name,dim2_units
    real(idx),intent(in) :: dim1(ndims1),dim2(ndims2)
    !-Variables-----------------------------------
    character(len=*),parameter :: units="units",axis="axis"
    integer :: ncid
    ! prepare dimids
    integer :: dim1_dimid,dim1_varid,dim2_dimid,dim2_varid
    ! create file
    call CHECK_W( NF90_CREATE(trim(file_name),or(NF90_CLOBBER,NF90_64BIT_OFFSET), ncid) )
    ! Define the dimensions
    if (trim(dim1_name) .eq. "") then
       call CHECK_W(NF90_DEF_DIM(ncid,"lon",ndims1,dim1_dimid))
    else
       call CHECK_W(NF90_DEF_DIM(ncid,trim(dim1_name),ndims1,dim1_dimid))
    end if
    if (trim(dim2_name) .eq. "") then
       call CHECK_W(NF90_DEF_DIM(ncid,trim(dim1_name),ndims2,dim2_dimid))
    else
       call CHECK_W(NF90_DEF_DIM(ncid,trim(dim2_name),ndims2,dim2_dimid))
    end if
    call CHECK_W(NF90_DEF_VAR(ncid,trim(dim1_name),NF90_DOUBLE,dim1_dimid,dim1_varid))
    call CHECK_W(NF90_DEF_VAR(ncid,trim(dim2_name),NF90_DOUBLE,dim2_dimid,dim2_varid))
    if (trim(dim1_units) .eq. "") then
       call CHECK_W(NF90_PUT_ATT(ncid,dim1_varid,units,"degrees_east"))
    else
       call CHECK_W(NF90_PUT_ATT(ncid,dim1_varid,units,trim(dim1_units)))
    end if
    if (trim(dim2_units) .eq. "") then
       call CHECK_W(NF90_PUT_ATT(ncid,dim2_varid,units,"degrees_north"))
    else
       call CHECK_W(NF90_PUT_ATT(ncid,dim2_varid,units,trim(dim2_units)))
    end if
    call CHECK_W(NF90_PUT_ATT(ncid, dim1_varid,axis,"X"))
    call CHECK_W(NF90_PUT_ATT(ncid, dim2_varid,axis,"Y"))
    call CHECK_W(NF90_ENDDEF(ncid))
    ! Put data
    call CHECK_W(NF90_PUT_VAR(ncid,dim1_varid,dim1))
    call CHECK_W(NF90_PUT_VAR(ncid,dim2_varid,dim2))
    ! close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine DWRITENET_2D_PRE
  subroutine WRITENET_3D_PRE(file_name,ndims1,ndims2,ndims3,dim1_name,dim2_name,dim3_name, &
       & dim1_units,dim2_units,dim3_units,dim1,dim2,dim3)
    use NETCDF
    implicit none
    integer,parameter :: idx = 4
    character(len=*),intent(in) :: file_name
    integer,intent(in) :: ndims1,ndims2,ndims3
    character(len=*),intent(in) :: dim1_name,dim1_units
    character(len=*),intent(in) :: dim2_name,dim2_units
    character(len=*),intent(in) :: dim3_name,dim3_units
    real(idx),intent(in) :: dim1(ndims1),dim2(ndims2),dim3(ndims3)
    !-Variables-----------------------------------
    character(len=*),parameter :: units="units",axis="axis"
    integer :: ncid
    integer :: dim1_dimid,dim1_varid,dim2_dimid,dim2_varid,dim3_dimid,dim3_varid
    ! Create file
    call CHECK_W(NF90_CREATE(trim(file_name),or(NF90_CLOBBER,NF90_64BIT_OFFSET), ncid) )
    ! Define the dimensions
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim1_name), ndims1,dim1_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim2_name), ndims2,dim2_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim3_name), ndims3,dim3_dimid))
    call CHECK_W(NF90_DEF_VAR(ncid, trim(dim1_name), NF90_FLOAT,dim1_dimid,dim1_varid))
    call CHECK_W(NF90_DEF_VAR(ncid, trim(dim2_name), NF90_FLOAT,dim2_dimid,dim2_varid))
    call CHECK_W(NF90_DEF_VAR(ncid, trim(dim3_name), NF90_FLOAT,dim3_dimid,dim3_varid))
    call CHECK_W(NF90_PUT_ATT(ncid, dim1_varid,units, trim(dim1_units)))
    call CHECK_W(NF90_PUT_ATT(ncid, dim2_varid,units, trim(dim2_units)))
    call CHECK_W(NF90_PUT_ATT(ncid, dim3_varid,units, trim(dim3_units)))
    call CHECK_W(NF90_PUT_ATT(ncid,dim1_varid,axis,"X"))
    call CHECK_W(NF90_PUT_ATT(ncid,dim2_varid,axis,"Y"))

    call CHECK_W(NF90_ENDDEF(ncid))
    ! Put data
    call CHECK_W(NF90_PUT_VAR(ncid,dim1_varid,dim1))
    call CHECK_W(NF90_PUT_VAR(ncid,dim2_varid,dim2))
    call CHECK_W(NF90_PUT_VAR(ncid,dim3_varid,dim3))
    ! close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine WRITENET_3D_PRE
  subroutine DWRITENET_3D_PRE(file_name,ndims1,ndims2,ndims3,dim1_name,dim2_name,dim3_name, &
       & dim1_units,dim2_units,dim3_units,dim1,dim2,dim3)
    use netcdf
    implicit none
    integer,parameter :: idx = 8
    character(len=*),intent(in) :: file_name
    integer,intent(in) :: ndims1,ndims2,ndims3
    character(len=*),intent(in) :: dim1_name,dim1_units
    character(len=*),intent(in) :: dim2_name,dim2_units
    character(len=*),intent(in) :: dim3_name,dim3_units
    real(idx),intent(in) :: dim1(ndims1),dim2(ndims2),dim3(ndims3)
    !-Variables-----------------------------------
    character(len=*),parameter :: units="units",axis="axis"
    integer :: ncid
    integer :: dim1_dimid,dim1_varid,dim2_dimid,dim2_varid,dim3_dimid,dim3_varid
    ! Create file
    call CHECK_W(NF90_CREATE(trim(file_name),or(NF90_CLOBBER,NF90_64BIT_OFFSET), ncid) )
    ! Define the dimensions
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim1_name), ndims1,dim1_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim2_name), ndims2,dim2_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim3_name), ndims3,dim3_dimid))
    call CHECK_W(NF90_DEF_VAR(ncid, trim(dim1_name), NF90_DOUBLE,dim1_dimid,dim1_varid))
    call CHECK_W(NF90_DEF_VAR(ncid, trim(dim2_name), NF90_DOUBLE,dim2_dimid,dim2_varid))
    call CHECK_W(NF90_DEF_VAR(ncid, trim(dim3_name), NF90_DOUBLE,dim3_dimid,dim3_varid))
    call CHECK_W(NF90_PUT_ATT(ncid, dim1_varid,units, trim(dim1_units)))
    call CHECK_W(NF90_PUT_ATT(ncid, dim2_varid,units, trim(dim2_units)))
    call CHECK_W(NF90_PUT_ATT(ncid, dim3_varid,units, trim(dim3_units)))
    call CHECK_W(NF90_PUT_ATT(ncid,dim1_varid,axis,"X"))
    call CHECK_W(NF90_PUT_ATT(ncid,dim2_varid,axis,"Y"))

    call CHECK_W(NF90_ENDDEF(ncid))
    ! Put data
    call CHECK_W(NF90_PUT_VAR(ncid,dim1_varid,dim1))
    call CHECK_W(NF90_PUT_VAR(ncid,dim2_varid,dim2))
    call CHECK_W(NF90_PUT_VAR(ncid,dim3_varid,dim3))
    ! close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine DWRITENET_3D_PRE
  !===========================================================================================
  subroutine WRITENET_4D_PRE(file_name,ndims1,ndims2,ndims3,ndims4,dim1_name,dim2_name,dim3_name, &
       & dim4_name,dim1_units,dim2_units,dim3_units,dim4_units,dim1,dim2,dim3,dim4)
    use NETCDF
    implicit none
    integer,parameter :: idx = 4
    character(len=*),intent(in) :: file_name
    integer,intent(in) :: ndims1,ndims2,ndims3,ndims4
    character(len=*),intent(in) :: dim1_name,dim1_units
    character(len=*),intent(in) :: dim2_name,dim2_units
    character(len=*),intent(in) :: dim3_name,dim3_units
    character(len=*),intent(in) :: dim4_name,dim4_units
    real(idx),intent(in) :: dim1(ndims1),dim2(ndims2),dim3(ndims3),dim4(ndims4)
    !-Variables-----------------------------------
    character(len=*),parameter :: units="units",axis="axis"
    integer :: ncid
    integer :: dim1_dimid,dim1_varid,dim2_dimid,dim2_varid
    integer :: dim3_dimid,dim3_varid,dim4_dimid,dim4_varid
    ! Create file
    call CHECK_W(NF90_CREATE(trim(file_name),or(NF90_CLOBBER,NF90_64BIT_OFFSET),ncid) )
    ! Define the dimensions
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim1_name), ndims1,dim1_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim2_name), ndims2,dim2_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim3_name), ndims3,dim3_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim4_name), ndims4,dim4_dimid))
    call CHECK_W(NF90_DEF_VAR(ncid, trim(dim1_name), NF90_FLOAT,dim1_dimid,dim1_varid))
    call CHECK_W(NF90_DEF_VAR(ncid, trim(dim2_name), NF90_FLOAT,dim2_dimid,dim2_varid))
    call CHECK_W(NF90_DEF_VAR(ncid, trim(dim3_name), NF90_FLOAT,dim3_dimid,dim3_varid))
    call CHECK_W(NF90_DEF_VAR(ncid, trim(dim4_name), NF90_FLOAT,dim4_dimid,dim4_varid))
    call CHECK_W(NF90_PUT_ATT(ncid, dim1_varid,units, trim(dim1_units)))
    call CHECK_W(NF90_PUT_ATT(ncid, dim2_varid,units, trim(dim2_units)))
    call CHECK_W(NF90_PUT_ATT(ncid, dim3_varid,units, trim(dim3_units)))
    call CHECK_W(NF90_PUT_ATT(ncid, dim4_varid,units, trim(dim4_units)))
    call CHECK_W(NF90_PUT_ATT(ncid, dim1_varid,axis,"X"))
    call CHECK_W(NF90_PUT_ATT(ncid, dim2_varid,axis,"Y"))
    call CHECK_W(NF90_ENDDEF(ncid))
    ! Put data
    call CHECK_W(NF90_PUT_VAR(ncid,dim1_varid,dim1))
    call CHECK_W(NF90_PUT_VAR(ncid,dim2_varid,dim2))
    call CHECK_W(NF90_PUT_VAR(ncid,dim3_varid,dim3))
    call CHECK_W(NF90_PUT_VAR(ncid,dim4_varid,dim4))
    ! close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine WRITENET_4D_PRE
  subroutine dwritenet_4D_pre(file_name,ndims1,ndims2,ndims3,ndims4,dim1_name,dim2_name,dim3_name, &
       & dim4_name,dim1_units,dim2_units,dim3_units,dim4_units,dim1,dim2,dim3,dim4)
    use netcdf
    implicit none
    integer,parameter :: idx = 8
    character(len=*),intent(in) :: file_name
    integer,intent(in) :: ndims1,ndims2,ndims3,ndims4
    character(len=*),intent(in) :: dim1_name,dim1_units
    character(len=*),intent(in) :: dim2_name,dim2_units
    character(len=*),intent(in) :: dim3_name,dim3_units
    character(len=*),intent(in) :: dim4_name,dim4_units
    real(idx),intent(in) :: dim1(ndims1),dim2(ndims2),dim3(ndims3),dim4(ndims4)
    !-Variables-----------------------------------
    character(len=*),parameter :: units="units",axis="axis"
    integer :: ncid
    integer :: dim1_dimid,dim1_varid,dim2_dimid,dim2_varid
    integer :: dim3_dimid,dim3_varid,dim4_dimid,dim4_varid
    ! Create file
    call CHECK_W(NF90_CREATE(trim(file_name),or(NF90_CLOBBER,NF90_64BIT_OFFSET),ncid) )
    ! Define the dimensions
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim1_name), ndims1,dim1_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim2_name), ndims2,dim2_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim3_name), ndims3,dim3_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim4_name), ndims4,dim4_dimid))
    call CHECK_W(NF90_DEF_VAR(ncid, trim(dim1_name), NF90_DOUBLE,dim1_dimid,dim1_varid))
    call CHECK_W(NF90_DEF_VAR(ncid, trim(dim2_name), NF90_DOUBLE,dim2_dimid,dim2_varid))
    call CHECK_W(NF90_DEF_VAR(ncid, trim(dim3_name), NF90_DOUBLE,dim3_dimid,dim3_varid))
    call CHECK_W(NF90_DEF_VAR(ncid, trim(dim4_name), NF90_DOUBLE,dim4_dimid,dim4_varid))
    call CHECK_W(NF90_PUT_ATT(ncid, dim1_varid,units, trim(dim1_units)))
    call CHECK_W(NF90_PUT_ATT(ncid, dim2_varid,units, trim(dim2_units)))
    call CHECK_W(NF90_PUT_ATT(ncid, dim3_varid,units, trim(dim3_units)))
    call CHECK_W(NF90_PUT_ATT(ncid, dim4_varid,units, trim(dim4_units)))
    call CHECK_W(NF90_PUT_ATT(ncid, dim1_varid,axis,"X"))
    call CHECK_W(NF90_PUT_ATT(ncid, dim2_varid,axis,"Y"))
    call CHECK_W(NF90_ENDDEF(ncid))
    ! Put data
    call CHECK_W(NF90_PUT_VAR(ncid,dim1_varid,dim1))
    call CHECK_W(NF90_PUT_VAR(ncid,dim2_varid,dim2))
    call CHECK_W(NF90_PUT_VAR(ncid,dim3_varid,dim3))
    call CHECK_W(NF90_PUT_VAR(ncid,dim4_varid,dim4))
    ! close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine dwritenet_4D_pre
  !===========================================================================================
  ! subrotines for define variables
  !----------------------------------------------------------------------
  subroutine WRITENET_1D_DV(file_name,dim1_name,nvar,vars_names,vars_units,vars_miss)
    use NETCDF
    implicit none
    integer,parameter :: idx=4
    character(len=*),intent(in) :: file_name,dim1_name
    integer,intent(in) :: nvar
    character(len=*),intent(in) :: vars_names(nvar),vars_units(nvar)
    real(idx),intent(in) :: vars_miss
    integer,parameter :: total_dim=1
    integer :: dimids(total_dim)
    integer :: ncid
    integer :: dim1_dimid
    integer :: vars_varid
    character(len=*),parameter :: units="units"
    character(len=*),parameter :: missing_value="missing_value"
    integer :: ivar
    ! Get ncid & dimid
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_DIMID(ncid,dim1_name,dim1_dimid))
    dimids = (/dim1_dimid/)
    ! Define variable
    call CHECK_W(NF90_REDEF(ncid))
    do ivar=1,nvar
       call CHECK_W(NF90_DEF_VAR(ncid, trim(vars_names(ivar)),NF90_FLOAT, dimids, vars_varid))
       call CHECK_W(NF90_PUT_ATT(ncid, vars_varid,units,vars_units(ivar)))
       call CHECK_W(NF90_PUT_ATT(ncid, vars_varid,missing_value,vars_miss))
    end do
    call CHECK_W(NF90_ENDDEF(ncid))
    ! close file
    call CHECK_W(NF90_CLOSE(ncid) )
  end subroutine WRITENET_1D_DV
  subroutine DWRITENET_1D_DV(file_name,dim1_name,nvar,vars_names,vars_units,vars_miss)
    use NETCDF
    implicit none
    integer,parameter :: idx=8
    character(len=*),intent(in) :: file_name,dim1_name
    integer,intent(in) :: nvar
    character(len=*),intent(in) :: vars_names(nvar),vars_units(nvar)
    real(idx),intent(in) :: vars_miss
    integer,parameter :: total_dim=1
    integer :: dimids(total_dim)
    integer :: ncid
    integer :: dim1_dimid
    integer :: vars_varid
    character(len=*),parameter :: units="units"
    character(len=*),parameter :: missing_value="missing_value"
    integer :: ivar
    ! Get ncid & dimid
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_DIMID(ncid,dim1_name,dim1_dimid))
    dimids = (/dim1_dimid/)
    ! Define variable
    call CHECK_W(NF90_REDEF(ncid))
    do ivar=1,nvar
       call CHECK_W(NF90_DEF_VAR(ncid, trim(vars_names(ivar)),NF90_DOUBLE, dimids, vars_varid))
       call CHECK_W(NF90_PUT_ATT(ncid, vars_varid,units,vars_units(ivar)))
       call CHECK_W(NF90_PUT_ATT(ncid, vars_varid,missing_value,vars_miss))
    end do
    call CHECK_W(NF90_ENDDEF(ncid))
    ! close file
    call CHECK_W(NF90_CLOSE(ncid) )
  end subroutine DWRITENET_1D_DV
    subroutine WRITENET_2D_DV(file_name,dim1_name,dim2_name,nvar,vars_names,vars_units,vars_miss)
    use NETCDF
    implicit none
    integer,parameter :: idx=4
    character(len=*),intent(in) :: file_name,dim1_name,dim2_name
    integer,intent(in) :: nvar
    character(len=*),intent(in) :: vars_names(nvar),vars_units(nvar)
    real(idx),intent(in) :: vars_miss
    integer,parameter :: total_dim=2
    integer :: dimids(total_dim)
    integer :: ncid
    integer :: dim1_dimid,dim2_dimid
    integer :: vars_varid
    character(len=*),parameter :: units="units"
    character(len=*),parameter :: missing_value="missing_value"
    integer :: ivar
    ! Get ncid & dimid
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_DIMID(ncid,dim1_name,dim1_dimid))
    call CHECK_W(NF90_INQ_DIMID(ncid,dim2_name,dim2_dimid))
    dimids = (/dim1_dimid,dim2_dimid/)
    ! Define variable
    call CHECK_W(NF90_REDEF(ncid))
    do ivar=1,nvar
       call CHECK_W(NF90_DEF_VAR(ncid, trim(vars_names(ivar)),NF90_FLOAT, dimids, vars_varid))
       call CHECK_W(NF90_PUT_ATT(ncid, vars_varid,units,vars_units(ivar)))
       call CHECK_W(NF90_PUT_ATT(ncid, vars_varid,missing_value,vars_miss))
    end do
    call CHECK_W(NF90_ENDDEF(ncid))
    ! close file
    call CHECK_W(NF90_CLOSE(ncid) )
  end subroutine WRITENET_2D_DV
  subroutine DWRITENET_2D_DV(file_name,dim1_name,dim2_name,nvar,vars_names,vars_units,vars_miss)
    use NETCDF
    implicit none
    integer,parameter :: idx=8
    character(len=*),intent(in) :: file_name,dim1_name,dim2_name
    integer,intent(in) :: nvar
    character(len=*),intent(in) :: vars_names(nvar),vars_units(nvar)
    real(idx),intent(in) :: vars_miss
    integer,parameter :: total_dim=2
    integer :: dimids(total_dim)
    integer :: ncid
    integer :: dim1_dimid,dim2_dimid
    integer :: vars_varid
    character(len=*),parameter :: units="units"
    character(len=*),parameter :: missing_value="missing_value"
    integer :: ivar
    ! Get ncid & dimid
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_DIMID(ncid,dim1_name,dim1_dimid))
    call CHECK_W(NF90_INQ_DIMID(ncid,dim2_name,dim2_dimid))
    dimids = (/dim1_dimid,dim2_dimid/)
    ! Define variable
    call CHECK_W(NF90_REDEF(ncid))
    do ivar=1,nvar
       call CHECK_W(NF90_DEF_VAR(ncid, trim(vars_names(ivar)),NF90_DOUBLE, dimids, vars_varid))
       call CHECK_W(NF90_PUT_ATT(ncid, vars_varid,units,vars_units(ivar)))
       call CHECK_W(NF90_PUT_ATT(ncid, vars_varid,missing_value,vars_miss))
    end do
    call CHECK_W(NF90_ENDDEF(ncid))
    ! close file
    call CHECK_W(NF90_CLOSE(ncid) )
  end subroutine DWRITENET_2D_DV
  subroutine WRITENET_3D_DV(file_name,dim1_name,dim2_name,dim3_name, &
       & nvar,vars_names,vars_units,vars_miss)
    use NETCDF
    implicit none
    integer,parameter :: idx=4
    character(len=*),intent(in) :: file_name,dim1_name,dim2_name,dim3_name
    integer,intent(in) :: nvar
    character(len=*),intent(in) :: vars_names(nvar),vars_units(nvar)
    real(idx),intent(in) :: vars_miss
    integer,parameter :: total_dim=3
    integer :: dimids(total_dim)
    integer :: ncid
    integer :: dim1_dimid,dim2_dimid,dim3_dimid
    integer :: vars_varid
    character(len=*),parameter :: units="units"
    character(len=*),parameter :: missing_value="missing_value"
    integer :: ivar
    ! Get ncid & dimid
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_DIMID(ncid,dim1_name,dim1_dimid))
    call CHECK_W(NF90_INQ_DIMID(ncid,dim2_name,dim2_dimid))
    call CHECK_W(NF90_INQ_DIMID(ncid,dim3_name,dim3_dimid))
    dimids = (/dim1_dimid,dim2_dimid,dim3_dimid/)
    ! Define variable
    call CHECK_W(NF90_REDEF(ncid))
    do ivar=1,nvar
       call CHECK_W(NF90_DEF_VAR(ncid, trim(vars_names(ivar)),NF90_FLOAT, dimids, vars_varid))
       call CHECK_W(NF90_PUT_ATT(ncid, vars_varid,units,vars_units(ivar)))
       call CHECK_W(NF90_PUT_ATT(ncid, vars_varid,missing_value,vars_miss))
    end do
    call CHECK_W(NF90_ENDDEF(ncid))
    ! close file
    call CHECK_W(NF90_CLOSE(ncid) )
  end subroutine WRITENET_3D_DV
  subroutine DWRITENET_3D_DV(file_name,dim1_name,dim2_name,dim3_name, &
       & nvar,vars_names,vars_units,vars_miss)
    use NETCDF
    implicit none
    integer,parameter :: idx=8
    character(len=*),intent(in) :: file_name,dim1_name,dim2_name,dim3_name
    integer,intent(in) :: nvar
    character(len=*),intent(in) :: vars_names(nvar),vars_units(nvar)
    real(idx),intent(in) :: vars_miss
    integer,parameter :: total_dim=3
    integer :: dimids(total_dim)
    integer :: ncid
    integer :: dim1_dimid,dim2_dimid,dim3_dimid
    integer :: vars_varid
    character(len=*),parameter :: units="units"
    character(len=*),parameter :: missing_value="missing_value"
    integer :: ivar
    ! Get ncid & dimid
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_DIMID(ncid,dim1_name,dim1_dimid))
    call CHECK_W(NF90_INQ_DIMID(ncid,dim2_name,dim2_dimid))
    call CHECK_W(NF90_INQ_DIMID(ncid,dim3_name,dim3_dimid))
    dimids = (/dim1_dimid,dim2_dimid,dim3_dimid/)
    ! Define variable
    call CHECK_W(NF90_REDEF(ncid))
    do ivar=1,nvar
       call CHECK_W(NF90_DEF_VAR(ncid, trim(vars_names(ivar)),NF90_DOUBLE, dimids, vars_varid))
       call CHECK_W(NF90_PUT_ATT(ncid, vars_varid,units,vars_units(ivar)))
       call CHECK_W(NF90_PUT_ATT(ncid, vars_varid,missing_value,vars_miss))
    end do
    call CHECK_W(NF90_ENDDEF(ncid))
    ! close file
    call CHECK_W(NF90_CLOSE(ncid) )
  end subroutine DWRITENET_3D_DV
  subroutine WRITENET_4D_DV(file_name,dim1_name,dim2_name,dim3_name,dim4_name, &
       & nvar,vars_names,vars_units,vars_miss)
    use NETCDF
    implicit none
    integer,parameter :: idx=4
    character(len=*),intent(in) :: file_name,dim1_name,dim2_name,dim3_name,dim4_name
    integer,intent(in) :: nvar
    character(len=*),intent(in) :: vars_names(nvar),vars_units(nvar)
    real(idx),intent(in) :: vars_miss
    integer,parameter :: total_dim=4
    integer :: dimids(total_dim)
    integer :: ncid
    integer :: dim1_dimid,dim2_dimid,dim3_dimid,dim4_dimid
    integer :: vars_varid
    character(len=*),parameter :: units="units"
    character(len=*),parameter :: missing_value="missing_value"
    integer :: ivar
    ! Get ncid & dimid
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_DIMID(ncid,dim1_name,dim1_dimid))
    call CHECK_W(NF90_INQ_DIMID(ncid,dim2_name,dim2_dimid))
    call CHECK_W(NF90_INQ_DIMID(ncid,dim3_name,dim3_dimid))
    call CHECK_W(NF90_INQ_DIMID(ncid,dim4_name,dim4_dimid))
    dimids = (/dim1_dimid,dim2_dimid,dim3_dimid,dim4_dimid/)
    ! Define variable
    call CHECK_W(NF90_REDEF(ncid))
    do ivar=1,nvar
       call CHECK_W(NF90_DEF_VAR(ncid, trim(vars_names(ivar)),NF90_FLOAT, dimids, vars_varid))
       call CHECK_W(NF90_PUT_ATT(ncid, vars_varid,units,vars_units(ivar)))
       call CHECK_W(NF90_PUT_ATT(ncid, vars_varid,missing_value,vars_miss))
    end do
    call CHECK_W(NF90_ENDDEF(ncid))
    ! close file
    call CHECK_W(NF90_CLOSE(ncid) )
  end subroutine WRITENET_4D_DV
  subroutine DWRITENET_4D_DV(file_name,dim1_name,dim2_name,dim3_name,dim4_name, &
       & nvar,vars_names,vars_units,vars_miss)
    use NETCDF
    implicit none
    integer,parameter :: idx=8
    character(len=*),intent(in) :: file_name,dim1_name,dim2_name,dim3_name,dim4_name
    integer,intent(in) :: nvar
    character(len=*),intent(in) :: vars_names(nvar),vars_units(nvar)
    real(idx),intent(in) :: vars_miss
    integer,parameter :: total_dim=4
    integer :: dimids(total_dim)
    integer :: ncid
    integer :: dim1_dimid,dim2_dimid,dim3_dimid,dim4_dimid
    integer :: vars_varid
    character(len=*),parameter :: units="units"
    character(len=*),parameter :: missing_value="missing_value"
    integer :: ivar
    ! Get ncid & dimid
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_DIMID(ncid,dim1_name,dim1_dimid))
    call CHECK_W(NF90_INQ_DIMID(ncid,dim2_name,dim2_dimid))
    call CHECK_W(NF90_INQ_DIMID(ncid,dim3_name,dim3_dimid))
    call CHECK_W(NF90_INQ_DIMID(ncid,dim4_name,dim4_dimid))
    dimids = (/dim1_dimid,dim2_dimid,dim3_dimid,dim4_dimid/)
    ! Define variable
    call CHECK_W(NF90_REDEF(ncid))
    do ivar=1,nvar
       call CHECK_W(NF90_DEF_VAR(ncid, trim(vars_names(ivar)),NF90_DOUBLE, dimids, vars_varid))
       call CHECK_W(NF90_PUT_ATT(ncid, vars_varid,units,vars_units(ivar)))
       call CHECK_W(NF90_PUT_ATT(ncid, vars_varid,missing_value,vars_miss))
    end do
    call CHECK_W(NF90_ENDDEF(ncid))
    ! close file
    call CHECK_W(NF90_CLOSE(ncid) )
  end subroutine DWRITENET_4D_DV
  !======================================================
  ! Subroutines for writing variables
  !=====================================================
  subroutine WRITENET_1D_WV(file_name,varname,start_i1,end_i1,vars)
    use NETCDF
    implicit none
    integer,parameter :: idx=4
    character(len = *),intent(in) :: file_name,varname
    real(idx),intent(in) :: vars(:)
    integer :: start_i1,end_i1
    integer,parameter :: ndims=1
    integer :: start(ndims),count(ndims)
    integer :: ncid,vars_varid
    !-----------------------------
    ! Get file id and variable id
    !-----------------------------
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_VARID(ncid,varname,vars_varid))
    start = (/start_i1/)
    count = (/end_i1-start_i1+1/)
    ! Put variable
    call CHECK_W(NF90_PUT_VAR(ncid, vars_varid, vars,start=start,count=count))
    ! Close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine WRITENET_1D_WV
  subroutine DWRITENET_1D_WV(file_name,varname,start_i1,end_i1,vars)
    use NETCDF
    implicit none
    integer,parameter :: idx=8
    character(len=*),intent(in) :: file_name,varname
    real(idx),intent(in) :: vars(:)
    integer :: start_i1,end_i1
    integer,parameter :: ndims=1
    integer :: start(ndims),count(ndims)
    integer :: ncid,vars_varid
    !-----------------------------
    ! Get file id and variable id
    !-----------------------------
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_VARID(ncid,varname,vars_varid))
    start = (/start_i1/)
    count = (/end_i1-start_i1+1/)
    ! Put variable
    call CHECK_W(NF90_PUT_VAR(ncid, vars_varid, vars,start=start,count=count))
    ! Close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine DWRITENET_1D_WV
  !-------------------------------------
  subroutine WRITENET_2D_WV(file_name,varname,start_i1,end_i1,start_i2,end_i2,vars)
    use NETCDF
    implicit none
    integer,parameter :: idx=4
    character(len=*),intent(in) :: file_name,varname
    real(idx),intent(in) :: vars(:,:)
    integer :: start_i1,end_i1,start_i2,end_i2
    integer,parameter :: ndims=2
    integer :: start(ndims),count(ndims)
    integer :: ncid,vars_varid
    !-----------------------------
    ! Get file id and variable id
    !-----------------------------
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    ! get dimension id
    call CHECK_W(NF90_INQ_VARID(ncid,varname,vars_varid))
    start = (/start_i1,start_i2/)
    count = (/end_i1-start_i1+1,end_i2-start_i2+1/)
    ! put variable
    call CHECK_W(NF90_PUT_VAR(ncid, vars_varid, vars,start=start,count=count))
    ! close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine WRITENET_2D_WV
  subroutine DWRITENET_2D_WV(file_name,varname,start_i1,end_i1,start_i2,end_i2,vars)
    use NETCDF
    implicit none
    integer,parameter :: idx=8
    character(len=*),intent(in) :: file_name,varname
    real(idx),intent(in) :: vars(:,:)
    integer :: start_i1,end_i1,start_i2,end_i2
    integer,parameter :: ndims=2
    integer :: start(ndims),count(ndims)
    integer :: ncid,vars_varid
    !-----------------------------
    ! Get file id and variable id
    !-----------------------------
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    ! get dimension id
    call CHECK_W(NF90_INQ_VARID(ncid,varname,vars_varid))
    start = (/start_i1,start_i2/)
    count = (/end_i1-start_i1+1,end_i2-start_i2+1/)
    ! put variable
    call CHECK_W(NF90_PUT_VAR(ncid, vars_varid, vars,start=start,count=count))
    ! close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine DWRITENET_2D_WV
  !
  ! 3D case
  subroutine WRITENET_3D_WV(file_name,varname,start_i1,end_i1,start_i2,end_i2,&
       & start_i3,end_i3,vars)
    use NETCDF
    implicit none
    integer,parameter :: idx=4
    character(len=*),intent(in) :: file_name,varname
    real(idx),intent(in) :: vars(:,:,:)
    integer :: start_i1,end_i1,start_i2,end_i2,start_i3,end_i3
    integer,parameter :: ndims=3
    integer :: start(ndims),count(ndims)
    integer :: ncid,vars_varid
    !-----------------------------
    ! Get file id and variable id
    !-----------------------------
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_VARID(ncid,varname,vars_varid))
    start = (/start_i1,start_i2,start_i3/)
    count = (/end_i1-start_i1+1,end_i2-start_i2+1,end_i3-start_i3+1/)
    ! Put variable
    call check_w(NF90_PUT_VAR(ncid, vars_varid, vars,start=start,count=count) )
    ! Close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine writenet_3D_wv
  subroutine DWRITENET_3D_WV(file_name,varname,start_i1,end_i1,start_i2,end_i2,&
       & start_i3,end_i3,vars)
    use NETCDF
    implicit none
    integer,parameter :: idx=8
    character(len=*),intent(in) :: file_name,varname
    real(idx),intent(in) :: vars(:,:,:)
    integer :: start_i1,end_i1,start_i2,end_i2,start_i3,end_i3
    integer,parameter :: ndims=3
    integer :: start(ndims),count(ndims)
    integer :: ncid,vars_varid
    !-----------------------------
    ! Get file id and variable id
    !-----------------------------
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_VARID(ncid,varname,vars_varid))
    start = (/start_i1,start_i2,start_i3/)
    count = (/end_i1-start_i1+1,end_i2-start_i2+1,end_i3-start_i3+1/)
    ! Put variable
    call check_w(NF90_PUT_VAR(ncid, vars_varid, vars,start=start,count=count) )
    ! Close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine DWRITENET_3D_WV
  ! 4D case
  subroutine WRITENET_4D_WV(file_name,varname,start_i1,end_i1,start_i2,end_i2,&
       & start_i3,end_i3,start_i4,end_i4,vars)
    use NETCDF
    implicit none
    integer,parameter :: idx=4
    character(len=*),intent(in) :: file_name,varname
    real(idx),intent(in) :: vars(:,:,:,:)
    integer :: start_i1,end_i1,start_i2,end_i2,start_i3,end_i3,start_i4,end_i4
    integer,parameter :: ndims=4
    integer :: start(ndims),count(ndims)
    integer :: ncid,vars_varid
    !-----------------------------
    ! Get file id and variable id
    !-----------------------------
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_VARID(ncid,varname,vars_varid))
    start = (/start_i1,start_i2,start_i3,start_i4/)
    count = (/end_i1-start_i1+1,end_i2-start_i2+1,&
         & end_i3-start_i3+1,end_i4-start_i4+1/)
    ! Put variable
    call CHECK_W(NF90_PUT_VAR(ncid, vars_varid, vars,start=start,count=count) )
    ! Close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine writenet_4D_wv
  subroutine dwritenet_4D_wv(file_name,varname,start_i1,end_i1,start_i2,end_i2,&
       & start_i3,end_i3,start_i4,end_i4,vars)
    use NETCDF
    implicit none
    integer,parameter :: idx=8
    character(len=*),intent(in) :: file_name,varname
    real(idx),intent(in) :: vars(:,:,:,:)
    integer :: start_i1,end_i1,start_i2,end_i2,start_i3,end_i3,start_i4,end_i4
    integer,parameter :: ndims=4
    integer :: start(ndims),count(ndims)
    integer :: ncid,vars_varid
    !-----------------------------
    ! Get file id and variable id
    !-----------------------------
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_VARID(ncid,varname,vars_varid))
    start = (/start_i1,start_i2,start_i3,start_i4/)
    count = (/end_i1-start_i1+1,end_i2-start_i2+1,&
         & end_i3-start_i3+1,end_i4-start_i4+1/)
    ! Put variable
    call CHECK_W(NF90_PUT_VAR(ncid, vars_varid, vars,start=start,count=count) )
    ! Close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine DWRITENET_4D_WV
  ! Define dimension
  subroutine writenet_dd(file_name,ndims1,dim1_name)
    use netcdf
    implicit none
    ! Input data
    character(len=*),intent(in) :: file_name
    integer,intent(in) :: ndims1
    character(len=*),intent(in) :: dim1_name
    integer :: ncid,dim1_dimid
    ! create file
    call check_w(NF90_OPEN(file_name,NF90_WRITE,ncid))
    ! Define the dimensions
    call check_w(NF90_REDEF(ncid))
    call check_w(NF90_DEF_DIM(ncid, trim(dim1_name), ndims1,dim1_dimid))
    call check_w(NF90_ENDDEF(ncid))
    ! close file
    call check_w(NF90_CLOSE(ncid))
  end subroutine writenet_dd
END MODULE NCDF_WRITE

