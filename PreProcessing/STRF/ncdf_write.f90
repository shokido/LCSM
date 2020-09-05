module NCDF_WRITE
  ! Compile method
  !  ifort -c ncdf_write.f90 -I/usr/local/netcdf4/include -L/usr/local/netcdf4/lib/ -lnetcdf -lnetcdff
  !  ar rcv /Users/kido/fortran/library_ifort/libncdf_write.a ncdf_write.o
  !  ranlib /Users/kido/fortran/library_ifort/libncdf_write.a
  implicit none
  private
  !------------------------------------------------------------
  ! Preparation
  interface WRITENET_PRE
     module procedure WRITENET_1D_PRE,DWRITENET_1D_PRE,WRITENET_2D_PRE,DWRITENET_2D_PRE,&
          & WRITENET_3D_PRE,DWRITENET_3D_PRE,WRITENET_4D_PRE,DWRITENET_4D_PRE,&
          & WRITENET_5D_PRE,DWRITENET_5D_PRE
  end interface WRITENET_PRE
  !------------------------------------------------------------
  interface WRITENET_DV
     module procedure WRITENET_1D_DV,DWRITENET_1D_DV,WRITENET_2D_DV,DWRITENET_2D_DV,&
          & WRITENET_3D_DV,DWRITENET_3D_DV,WRITENET_4D_DV,DWRITENET_4D_DV,WRITENET_5D_DV,DWRITENET_5D_DV,&
          & CWRITENET_1D_DV
  end interface WRITENET_DV 
  interface WRITENET_WV
     module procedure WRITENET_1D_WV,DWRITENET_1D_WV,WRITENET_2D_WV,DWRITENET_2D_WV,&
          & WRITENET_3D_WV,DWRITENET_3D_WV,WRITENET_4D_WV,DWRITENET_4D_WV,&
          & WRITENET_5D_WV,DWRITENET_5D_WV,&
          & CWRITENET_1D_WV
  end interface WRITENET_WV
  !------------------------------------------------------------
  public :: CHECK_W
  public :: WRITENET_PRE,WRITENET_DV,WRITENET_WV
  public :: CREATE_TIME_ATT
  public :: WRITENET_DD,ADD_VAR_ATT
  public :: WRITENET_PREDV
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
    integer :: bnds_dimid
    integer :: dim1_dimid,dim1_varid
    ! create file
    call CHECK_W(NF90_CREATE(trim(file_name),nf90_netcdf4, ncid) )
    ! Define the dimensions
    call CHECK_W(NF90_DEF_DIM(ncid,"bnds",2,bnds_dimid))
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
    integer :: bnds_dimid
    integer :: dim1_dimid,dim1_varid
    ! create file
    call CHECK_W(NF90_CREATE(trim(file_name),nf90_netcdf4, ncid) )
    ! Define the dimensions
    call CHECK_W(NF90_DEF_DIM(ncid,"bnds",2,bnds_dimid))
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
    integer :: bnds_dimid
    ! prepare dimids
    integer :: dim1_dimid,dim1_varid,dim2_dimid,dim2_varid
    ! Create file
    call CHECK_W(NF90_CREATE(trim(file_name),nf90_netcdf4, ncid) )
    ! Define the dimensions
    call CHECK_W(NF90_DEF_DIM(ncid,"bnds",2,bnds_dimid))
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
    integer :: bnds_dimid
    ! prepare dimids
    integer :: dim1_dimid,dim1_varid,dim2_dimid,dim2_varid
    ! create file
    call CHECK_W( NF90_CREATE(trim(file_name),nf90_netcdf4, ncid) )
    ! Define the dimensions
    call CHECK_W(NF90_DEF_DIM(ncid,"bnds",2,bnds_dimid))
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
    integer :: bnds_dimid
    integer :: dim1_dimid,dim1_varid,dim2_dimid,dim2_varid,dim3_dimid,dim3_varid
    ! Create file
    call CHECK_W(NF90_CREATE(trim(file_name),nf90_netcdf4, ncid) )
    ! Define the dimensions
    call CHECK_W(NF90_DEF_DIM(ncid,"bnds",2,bnds_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim1_name), ndims1,dim1_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim2_name), ndims2,dim2_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim3_name), NF90_UNLIMITED,dim3_dimid))
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
    integer :: bnds_dimid
    integer :: dim1_dimid,dim1_varid,dim2_dimid,dim2_varid,dim3_dimid,dim3_varid
    ! Create file
    call CHECK_W(NF90_CREATE(trim(file_name),nf90_netcdf4, ncid) )
    ! Define the dimensions
    call CHECK_W(NF90_DEF_DIM(ncid,"bnds",2,bnds_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim1_name), ndims1,dim1_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim2_name), ndims2,dim2_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim3_name), NF90_UNLIMITED,dim3_dimid))
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
    integer :: bnds_dimid
    integer :: dim1_dimid,dim1_varid,dim2_dimid,dim2_varid
    integer :: dim3_dimid,dim3_varid,dim4_dimid,dim4_varid
    ! Create file
    call CHECK_W(NF90_CREATE(trim(file_name),nf90_netcdf4,ncid) )
    ! Define the dimensions
    call CHECK_W(NF90_DEF_DIM(ncid,"bnds",2,bnds_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim1_name), ndims1,dim1_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim2_name), ndims2,dim2_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim3_name), ndims3,dim3_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim4_name),  NF90_UNLIMITED,dim4_dimid))
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
    integer :: bnds_dimid
    integer :: dim1_dimid,dim1_varid,dim2_dimid,dim2_varid
    integer :: dim3_dimid,dim3_varid,dim4_dimid,dim4_varid
    ! Create file
    call CHECK_W(NF90_CREATE(trim(file_name),nf90_netcdf4,ncid) )
    ! Define the dimensions
    call CHECK_W(NF90_DEF_DIM(ncid,"bnds",2,bnds_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim1_name), ndims1,dim1_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim2_name), ndims2,dim2_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim3_name), ndims3,dim3_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim4_name), NF90_UNLIMITED,dim4_dimid))
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
  subroutine WRITENET_5D_PRE(file_name,ndims1,ndims2,ndims3,ndims4,ndims5,dim1_name,dim2_name,dim3_name, &
       & dim4_name,dim5_name,dim1_units,dim2_units,dim3_units,dim4_units,dim5_units,dim1,dim2,dim3,dim4,dim5)
    use NETCDF
    implicit none
    integer,parameter :: idx = 4
    character(len=*),intent(in) :: file_name
    integer,intent(in) :: ndims1,ndims2,ndims3,ndims4,ndims5
    character(len=*),intent(in) :: dim1_name,dim1_units
    character(len=*),intent(in) :: dim2_name,dim2_units
    character(len=*),intent(in) :: dim3_name,dim3_units
    character(len=*),intent(in) :: dim4_name,dim4_units
    character(len=*),intent(in) :: dim5_name,dim5_units
    real(idx),intent(in) :: dim1(ndims1),dim2(ndims2),dim3(ndims3),dim4(ndims4),dim5(ndims5)
    !-Variables-----------------------------------
    character(len=*),parameter :: units="units",axis="axis"
    integer :: ncid
    integer :: bnds_dimid
    integer :: dim1_dimid,dim1_varid,dim2_dimid,dim2_varid
    integer :: dim3_dimid,dim3_varid,dim4_dimid,dim4_varid
    integer :: dim5_dimid,dim5_varid
    ! Create file
    call CHECK_W(NF90_CREATE(trim(file_name),nf90_netcdf4,ncid) )
    ! Define the dimensions
    call CHECK_W(NF90_DEF_DIM(ncid,"bnds",2,bnds_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim1_name), ndims1,dim1_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim2_name), ndims2,dim2_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim3_name), ndims3,dim3_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim4_name), ndims4,dim4_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim5_name),  NF90_UNLIMITED,dim5_dimid))
    call CHECK_W(NF90_DEF_VAR(ncid, trim(dim1_name), NF90_FLOAT,dim1_dimid,dim1_varid))
    call CHECK_W(NF90_DEF_VAR(ncid, trim(dim2_name), NF90_FLOAT,dim2_dimid,dim2_varid))
    call CHECK_W(NF90_DEF_VAR(ncid, trim(dim3_name), NF90_FLOAT,dim3_dimid,dim3_varid))
    call CHECK_W(NF90_DEF_VAR(ncid, trim(dim4_name), NF90_FLOAT,dim4_dimid,dim4_varid))
    call CHECK_W(NF90_DEF_VAR(ncid, trim(dim5_name), NF90_FLOAT,dim5_dimid,dim5_varid))
    call CHECK_W(NF90_PUT_ATT(ncid, dim1_varid,units, trim(dim1_units)))
    call CHECK_W(NF90_PUT_ATT(ncid, dim2_varid,units, trim(dim2_units)))
    call CHECK_W(NF90_PUT_ATT(ncid, dim3_varid,units, trim(dim3_units)))
    call CHECK_W(NF90_PUT_ATT(ncid, dim4_varid,units, trim(dim4_units)))
    call CHECK_W(NF90_PUT_ATT(ncid, dim5_varid,units, trim(dim5_units)))
    call CHECK_W(NF90_PUT_ATT(ncid, dim1_varid,axis,"X"))
    call CHECK_W(NF90_PUT_ATT(ncid, dim2_varid,axis,"Y"))
    call CHECK_W(NF90_ENDDEF(ncid))
    ! Put data
    call CHECK_W(NF90_PUT_VAR(ncid,dim1_varid,dim1))
    call CHECK_W(NF90_PUT_VAR(ncid,dim2_varid,dim2))
    call CHECK_W(NF90_PUT_VAR(ncid,dim3_varid,dim3))
    call CHECK_W(NF90_PUT_VAR(ncid,dim4_varid,dim4))
    call CHECK_W(NF90_PUT_VAR(ncid,dim5_varid,dim5))
    ! close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine WRITENET_5D_PRE
  subroutine DWRITENET_5D_PRE(file_name,ndims1,ndims2,ndims3,ndims4,ndims5,dim1_name,dim2_name,dim3_name, &
       & dim4_name,dim5_name,dim1_units,dim2_units,dim3_units,dim4_units,dim5_units,dim1,dim2,dim3,dim4,dim5)
    use NETCDF
    implicit none
    integer,parameter :: idx = 8
    character(len=*),intent(in) :: file_name
    integer,intent(in) :: ndims1,ndims2,ndims3,ndims4,ndims5
    character(len=*),intent(in) :: dim1_name,dim1_units
    character(len=*),intent(in) :: dim2_name,dim2_units
    character(len=*),intent(in) :: dim3_name,dim3_units
    character(len=*),intent(in) :: dim4_name,dim4_units
    character(len=*),intent(in) :: dim5_name,dim5_units
    real(idx),intent(in) :: dim1(ndims1),dim2(ndims2),dim3(ndims3),dim4(ndims4),dim5(ndims5)
    !-Variables-----------------------------------
    character(len=*),parameter :: units="units",axis="axis"
    integer :: ncid
    integer :: bnds_dimid
    integer :: dim1_dimid,dim1_varid,dim2_dimid,dim2_varid
    integer :: dim3_dimid,dim3_varid,dim4_dimid,dim4_varid
    integer :: dim5_dimid,dim5_varid
    ! Create file
    call CHECK_W(NF90_CREATE(trim(file_name),nf90_netcdf4,ncid) )
    ! Define the dimensions
    call CHECK_W(NF90_DEF_DIM(ncid,"bnds",2,bnds_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim1_name), ndims1,dim1_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim2_name), ndims2,dim2_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim3_name), ndims3,dim3_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim4_name), ndims4,dim4_dimid))
    call CHECK_W(NF90_DEF_DIM(ncid, trim(dim5_name),  NF90_UNLIMITED,dim5_dimid))
    call CHECK_W(NF90_DEF_VAR(ncid, trim(dim1_name), NF90_DOUBLE,dim1_dimid,dim1_varid))
    call CHECK_W(NF90_DEF_VAR(ncid, trim(dim2_name), NF90_DOUBLE,dim2_dimid,dim2_varid))
    call CHECK_W(NF90_DEF_VAR(ncid, trim(dim3_name), NF90_DOUBLE,dim3_dimid,dim3_varid))
    call CHECK_W(NF90_DEF_VAR(ncid, trim(dim4_name), NF90_DOUBLE,dim4_dimid,dim4_varid))
    call CHECK_W(NF90_DEF_VAR(ncid, trim(dim5_name), NF90_DOUBLE,dim5_dimid,dim5_varid))
    call CHECK_W(NF90_PUT_ATT(ncid, dim1_varid,units, trim(dim1_units)))
    call CHECK_W(NF90_PUT_ATT(ncid, dim2_varid,units, trim(dim2_units)))
    call CHECK_W(NF90_PUT_ATT(ncid, dim3_varid,units, trim(dim3_units)))
    call CHECK_W(NF90_PUT_ATT(ncid, dim4_varid,units, trim(dim4_units)))
    call CHECK_W(NF90_PUT_ATT(ncid, dim5_varid,units, trim(dim5_units)))
    call CHECK_W(NF90_PUT_ATT(ncid, dim1_varid,axis,"X"))
    call CHECK_W(NF90_PUT_ATT(ncid, dim2_varid,axis,"Y"))
    call CHECK_W(NF90_ENDDEF(ncid))
    ! Put data
    call CHECK_W(NF90_PUT_VAR(ncid,dim1_varid,dim1))
    call CHECK_W(NF90_PUT_VAR(ncid,dim2_varid,dim2))
    call CHECK_W(NF90_PUT_VAR(ncid,dim3_varid,dim3))
    call CHECK_W(NF90_PUT_VAR(ncid,dim4_varid,dim4))
    call CHECK_W(NF90_PUT_VAR(ncid,dim5_varid,dim5))
    ! close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine DWRITENET_5D_PRE  
  !===========================================================================================
  ! subrotines for define variables
  !----------------------------------------------------------------------
  subroutine CWRITENET_1D_DV(file_name,dim1_name,nvar,vars_names,vars_units)
    use NETCDF
    implicit none
    character(len=*),intent(in) :: file_name,dim1_name
    integer,intent(in) :: nvar
    character(len=*),intent(in) :: vars_names(nvar),vars_units(nvar)
    integer,parameter :: total_dim=1
    integer :: dimids(total_dim)
    integer :: ncid
    integer :: dim1_dimid
    integer :: vars_varid
    character(len=*),parameter :: units="units"
    integer :: ivar
    ! Get ncid & dimid
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_DIMID(ncid,dim1_name,dim1_dimid))
    dimids = (/dim1_dimid/)
    ! Define variable
    call CHECK_W(NF90_REDEF(ncid))
    do ivar=1,nvar
       call CHECK_W(NF90_DEF_VAR(ncid, trim(vars_names(ivar)),NF90_CHAR, dimids, vars_varid))
       call CHECK_W(NF90_PUT_ATT(ncid, vars_varid,units,vars_units(ivar)))
    end do
    call CHECK_W(NF90_ENDDEF(ncid))
    ! close file
    call CHECK_W(NF90_CLOSE(ncid) )
  end subroutine CWRITENET_1D_DV
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
    integer :: ivar,status
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
    ! Close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine DWRITENET_4D_DV
  ! 5D
  subroutine WRITENET_5D_DV(file_name,dim1_name,dim2_name,dim3_name,dim4_name,dim5_name, &
       & nvar,vars_names,vars_units,vars_miss)
    use NETCDF
    implicit none
    integer,parameter :: idx=4
    character(len=*),intent(in) :: file_name,dim1_name,dim2_name,dim3_name,dim4_name,dim5_name
    integer,intent(in) :: nvar
    character(len=*),intent(in) :: vars_names(nvar),vars_units(nvar)
    real(idx),intent(in) :: vars_miss
    integer,parameter :: total_dim=5
    integer :: dimids(total_dim)
    integer :: ncid
    integer :: dim1_dimid,dim2_dimid,dim3_dimid,dim4_dimid,dim5_dimid
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
    call CHECK_W(NF90_INQ_DIMID(ncid,dim5_name,dim5_dimid))
    dimids = (/dim1_dimid,dim2_dimid,dim3_dimid,dim4_dimid,dim5_dimid/)
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
  end subroutine WRITENET_5D_DV
  subroutine DWRITENET_5D_DV(file_name,dim1_name,dim2_name,dim3_name,dim4_name,dim5_name, &
       & nvar,vars_names,vars_units,vars_miss)
    use NETCDF
    implicit none
    integer,parameter :: idx=8
    character(len=*),intent(in) :: file_name,dim1_name,dim2_name,dim3_name,dim4_name,dim5_name
    integer,intent(in) :: nvar
    character(len=*),intent(in) :: vars_names(nvar),vars_units(nvar)
    real(idx),intent(in) :: vars_miss
    integer,parameter :: total_dim=5
    integer :: dimids(total_dim)
    integer :: ncid
    integer :: dim1_dimid,dim2_dimid,dim3_dimid,dim4_dimid,dim5_dimid
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
    call CHECK_W(NF90_INQ_DIMID(ncid,dim5_name,dim5_dimid))
    dimids = (/dim1_dimid,dim2_dimid,dim3_dimid,dim4_dimid,dim5_dimid/)
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
  end subroutine DWRITENET_5D_DV  
  !======================================================
  ! Subroutines for dimension preparation
  !=====================================================
  subroutine WRITENET_PREDV(file_name,ndim,dim_names,dim_sizes,dim_units,nvar,var_names,var_units,var_miss)
    use NETCDF
    implicit none
    integer,parameter :: idx=4
    integer,intent(in) :: ndim
    character(len=*),intent(in) :: file_name
    character(len=*),intent(in) :: dim_names(ndim),dim_units(ndim)
    integer,intent(in) :: dim_sizes(ndim)
    integer,intent(in) :: nvar
    character(len=*),intent(in) :: var_names(nvar),var_units(nvar)
    real(idx),intent(in) :: var_miss
    integer :: ncid
    integer :: dimids(ndim)
    integer :: varid_dims(ndim),varid_vars(nvar)
    integer :: idim,ivar
    integer :: dimid,varid
    character(len=*),parameter :: units="units",axis="axis",missing_value="missing_value"
    ! Create file
    call CHECK_W(NF90_CREATE(trim(file_name),nf90_netcdf4,ncid))
   ! Define the dimensions
    do idim = 1,ndim
       call CHECK_W(NF90_DEF_DIM(ncid, trim(dim_names(idim)),dim_sizes(idim),dimid))
       dimids(idim)=dimid
       call CHECK_W(NF90_DEF_VAR(ncid, trim(dim_names(idim)),NF90_FLOAT,dimid,varid))
       call CHECK_W(NF90_PUT_ATT(ncid, varid,units, trim(dim_units(idim))))
       if (idim .eq. 1) then
          call CHECK_W(NF90_PUT_ATT(ncid,varid,axis,"X"))
       else if (idim .eq. 2) then
          call CHECK_W(NF90_PUT_ATT(ncid,varid,axis,"Y"))
       end if
    end do
    do ivar=1,nvar
       call CHECK_W(NF90_DEF_VAR(ncid, trim(var_names(ivar)),NF90_FLOAT, dimids, varid))
       call CHECK_W(NF90_PUT_ATT(ncid, varid,units,var_units(ivar)))
       call CHECK_W(NF90_PUT_ATT(ncid, varid,missing_value,var_miss))
    end do
    ! close file
    call CHECK_W(NF90_ENDDEF(ncid))
    call CHECK_W(NF90_CLOSE(ncid) )
  end subroutine WRITENET_PREDV
  subroutine DWRITENET_PREDV(file_name,ndim,dim_names,dim_sizes,dim_units,nvar,var_names,var_units,var_miss)
    use NETCDF
    implicit none
    integer,parameter :: idx=8
    integer,intent(in) :: ndim
    character(len=*),intent(in) :: file_name
    character(len=*),intent(in) :: dim_names(ndim),dim_units(ndim)
    integer :: dim_sizes(ndim)
    integer,intent(in) :: nvar
    character(len=*),intent(in) :: var_names(nvar),var_units(nvar)
    real(idx),intent(in) :: var_miss
    integer :: ncid
    integer :: dimids(ndim)
    integer :: idim,ivar
    integer :: dimid,varid
    character(len=*),parameter :: units="units",axis="axis",missing_value="missing_value"
    ! Create file
    call CHECK_W(NF90_CREATE(trim(file_name),nf90_netcdf4,ncid))
    ! Define the dimensions
    do idim = 1,ndim
       call CHECK_W(NF90_DEF_DIM(ncid, trim(dim_names(idim)),dim_sizes(idim),dimid))
       dimids(idim)=dimid
       call CHECK_W(NF90_DEF_VAR(ncid, trim(dim_names(idim)),NF90_DOUBLE,dimid,varid))
       call CHECK_W(NF90_PUT_ATT(ncid, varid,units, trim(dim_units(idim))))
       if (idim .eq. 1) then
          call CHECK_W(NF90_PUT_ATT(ncid,varid,axis,"X"))
       else if (idim .eq. 2) then
          call CHECK_W(NF90_PUT_ATT(ncid,varid,axis,"Y"))
       end if
    end do
    do ivar=1,nvar
       call CHECK_W(NF90_DEF_VAR(ncid, trim(var_names(ivar)),NF90_DOUBLE, dimids, varid))
       call CHECK_W(NF90_PUT_ATT(ncid, varid,units,var_units(ivar)))
       call CHECK_W(NF90_PUT_ATT(ncid, varid,missing_value,var_miss))
    end do
    ! close file
    call CHECK_W(NF90_ENDDEF(ncid))
    call CHECK_W(NF90_CLOSE(ncid) )
  end subroutine DWRITENET_PREDV
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
  subroutine CWRITENET_1D_WV(file_name,varname,start_i1,end_i1,vars)
    use NETCDF
    implicit none
    character(len=*),intent(in) :: file_name,varname
    character(len=*),intent(in) :: vars(:)
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
  end subroutine CWRITENET_1D_WV

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
  end subroutine WRITENET_4D_WV
  subroutine DWRITENET_4D_wv(file_name,varname,start_i1,end_i1,start_i2,end_i2,&
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
  subroutine WRITENET_5D_wv(file_name,varname,start_i1,end_i1,start_i2,end_i2,&
       & start_i3,end_i3,start_i4,end_i4,start_i5,end_i5,vars)
    use NETCDF
    implicit none
    integer,parameter :: idx=4
    character(len=*),intent(in) :: file_name,varname
    real(idx),intent(in) :: vars(:,:,:,:,:)
    integer :: start_i1,end_i1
    integer :: start_i2,end_i2
    integer :: start_i3,end_i3
    integer :: start_i4,end_i4
    integer :: start_i5,end_i5
    integer,parameter :: ndims=5
    integer :: start(ndims),count(ndims)
    integer :: ncid,vars_varid
    !-----------------------------
    ! Get file id and variable id
    !-----------------------------
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_VARID(ncid,varname,vars_varid))
    start = (/start_i1,start_i2,start_i3,start_i4,start_i5/)
    count = (/end_i1-start_i1+1,end_i2-start_i2+1,&
         & end_i3-start_i3+1,end_i4-start_i4+1,end_i5-start_i5+1/)
    ! Put variable
    call CHECK_W(NF90_PUT_VAR(ncid, vars_varid, vars,start=start,count=count) )
    ! Close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine WRITENET_5D_WV
  subroutine DWRITENET_5D_wv(file_name,varname,start_i1,end_i1,start_i2,end_i2,&
       & start_i3,end_i3,start_i4,end_i4,start_i5,end_i5,vars)
    use NETCDF
    implicit none
    integer,parameter :: idx=8
    character(len=*),intent(in) :: file_name,varname
    real(idx),intent(in) :: vars(:,:,:,:,:)
    integer :: start_i1,end_i1
    integer :: start_i2,end_i2
    integer :: start_i3,end_i3
    integer :: start_i4,end_i4
    integer :: start_i5,end_i5
    integer,parameter :: ndims=5
    integer :: start(ndims),count(ndims)
    integer :: ncid,vars_varid
    !-----------------------------
    ! Get file id and variable id
    !-----------------------------
    call CHECK_W(NF90_OPEN(file_name,NF90_WRITE,ncid))
    call CHECK_W(NF90_INQ_VARID(ncid,varname,vars_varid))
    start = (/start_i1,start_i2,start_i3,start_i4,start_i5/)
    count = (/end_i1-start_i1+1,end_i2-start_i2+1,&
         & end_i3-start_i3+1,end_i4-start_i4+1,end_i5-start_i5+1/)
    ! Put variable
    call CHECK_W(NF90_PUT_VAR(ncid, vars_varid, vars,start=start,count=count) )
    ! Close file
    call CHECK_W(NF90_CLOSE(ncid))
  end subroutine DWRITENET_5D_WV
  function create_time_att(start_yymmdd,int_yymmdd) result(time_unit)
    implicit none
    integer,intent(in) ::start_yymmdd,int_yymmdd
    integer :: start_year,start_month,start_day
    character :: year_ind*4,month_ind*2,day_ind*2
    character :: ref_unit*5
    character :: time_unit*100
    start_year=start_yymmdd/10000
    start_month=start_yymmdd/100-start_year*100
    start_day=start_yymmdd-start_year*10000-start_month*100
    write(year_ind,'(i4.4)') start_year
    write(month_ind,'(i2.2)') start_month
    write(day_ind,'(i2.2)') start_day
    ! ref unit
    if (int_yymmdd .ge. 10000) then
       ref_unit="years"
    else if (int_yymmdd .ge. 100) then
       ref_unit="months"
    else
       ref_unit="days"
    end if
    time_unit=trim(ref_unit)//" since "//year_ind//"-"//month_ind//"-"//day_ind//" 00:00:00"
  end function create_time_att
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
  subroutine add_var_att(file_name,varname,att_name,att)
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
  end subroutine add_var_att
END MODULE NCDF_WRITE

