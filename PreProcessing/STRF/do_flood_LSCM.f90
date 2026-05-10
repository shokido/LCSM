program do_flood
  ! ifort -o exec_flood.out do_flood_lom.f90 -lnetcdf -lnetcdff -lncdf_read -lncdf_write $FPATH_FORT $LPATH_FORT
  use ncdf_read
  use ncdf_write
  implicit none
  integer,parameter :: idx = 8,maxlen=400
  integer :: nlon_in,nlat_in,nlev_in,ntime_in
  integer :: nx_rho,ny_rho,nz_rho,nz_w
  real(idx),allocatable :: lon_in(:),lat_in(:),lev_in(:),time_in(:)
  real(idx),allocatable :: data_in(:,:,:,:)
  real(idx),allocatable :: x_1d(:),y_1d(:)
  real(idx),allocatable :: x_rho(:,:),y_rho(:,:),h_rho(:,:)
  real(idx),allocatable :: data_2d(:,:)
  character(len=maxlen) :: fname_grid
  real(idx),allocatable :: data_out(:,:,:,:)
  real(idx),allocatable :: s_rho(:),s_w(:),cs_rho(:),cs_w(:)
  real(idx),allocatable :: mask_rho(:,:),mask_u(:,:),mask_v(:,:)
  real(idx),allocatable :: mask_dry(:,:),mask_wet(:,:),mask_bathy(:,:)
  character(len=maxlen) :: lonname,latname,levname,timename
  character(len=maxlen) :: lonunits,latunits,levunits,timeunits,varunits
  real(idx) :: missing_value
  real(idx),parameter :: abs_range=1.0e4
  integer :: ilev,itime,ix,iy
  integer :: nbdry,nbwet,idry,iwet
  integer, allocatable :: wet(:,:),dry(:,:)
  integer :: nfile,ifile,ind_lev
  character(len=maxlen),allocatable :: fname_in(:),fname_out(:),varflag(:),varname(:)
  real(idx) :: H
  H=4000.0_idx
  namelist/grid/fname_grid
  namelist/vars/lonname,latname,levname,timename,nfile
  namelist/files/fname_in,fname_out,varflag,varname
  open(10,file="filename_do_flood.nml",status='old')
  read(10,grid)
  read(10,vars)
  allocate(fname_in(nfile));  allocate(fname_out(nfile))
  allocate(varflag(nfile));   allocate(varname(nfile))
  read(10,files)
  close(10)
  ! Get coordinate variable
  call get_variable(trim(fname_grid),"x_p","x_p",nx_rho,x_1d)
  call get_variable(trim(fname_grid),"y_p","y_p",ny_rho,y_1d)
  allocate(mask_rho(nx_rho,ny_rho))
  call get_variable(trim(fname_grid),"mask_p",1,nx_rho,1,ny_rho,mask_rho)
  allocate(h_rho(nx_rho,ny_rho))
  h_rho=H
  allocate(x_rho(nx_rho,ny_rho))
  allocate(y_rho(nx_rho,ny_rho))
  do ix = 1,nx_rho
     x_rho(ix,1:ny_rho)=x_1d(ix)
  end do
  do iy = 1,ny_rho
     y_rho(1:nx_rho,iy)=y_1d(iy)
  end do

  ! Input file
  do ifile =1,nfile
     call get_variable(fname_in(ifile),lonname,lonname,nlon_in,lon_in)
     call get_attribute(fname_in(ifile),lonname,"units",lonunits)
     call get_variable(fname_in(ifile),latname,latname,nlat_in,lat_in)
     call get_attribute(fname_in(ifile),latname,"units",latunits)
     call get_variable(fname_in(ifile),levname,levname,nlev_in,lev_in)
     call get_attribute(fname_in(ifile),levname,"units",levunits)
     call get_variable(fname_in(ifile),timename,timename,ntime_in,time_in)
     call get_attribute(fname_in(ifile),timename,"units",timeunits)
     call get_attribute(fname_in(ifile),varname(ifile),"units",varunits)
     call get_attribute(fname_in(ifile),varname(ifile),"missing_value",missing_value)
     allocate(data_in(nlon_in,nlat_in,1,1))
     ! Output file
     call writenet_pre(fname_out(ifile),nlon_in,nlat_in,nlev_in,ntime_in,&
          & lonname,latname,levname,timename,lonunits,latunits,levunits,timeunits,&
          & lon_in,lat_in,lev_in,time_in)
     call writenet_dv(fname_out(ifile),lonname,latname,levname,timename,1,(/varname(ifile)/),(/varunits/),missing_value)
     allocate(data_out(nlon_in,nlat_in,1,1))

     allocate(mask_dry(nx_rho,ny_rho)); allocate(mask_wet(nx_rho,ny_rho))
     allocate(mask_bathy(nx_rho,ny_rho))

     do itime =1,ntime_in
        write(*,*) "time=",itime
        do ilev=1,nlev_in
           mask_dry=0.0_idx
           mask_wet=0.0_idx
           mask_bathy=0.0_idx
           call get_variable(fname_in(ifile),varname(ifile),1,nlon_in,1,nlat_in,ilev,ilev,itime,itime,data_in)
           data_out=data_in
           ! Find mask
           where(abs(data_in(1:nlon_in,1:nlat_in,1,1)) .ge. abs_range) mask_dry=1.0
           ind_lev=max(1,ilev-1)
           where(h_rho(1:nx_rho,1:ny_rho) .gt. lev_in(ind_lev)) mask_bathy=1.0
           mask_dry=mask_dry*mask_rho*mask_bathy
           where(abs(data_in(1:nlon_in,1:nlat_in,1,1)) .lt. abs_range) mask_wet=1.0
           nbdry=int(sum(mask_dry))  ;   nbwet=int(sum(mask_wet))
           write(*,*) ilev,"DRY=",nbdry,"WET=",nbwet
           if (nbdry .gt. 0 .and. nbwet .gt. 0) then
              allocate(wet(nbwet,2)); allocate(dry(nbdry,2))
              idry=1; iwet=1
              do iy=1,nlat_in
                 do ix =1,nlon_in
                    if (mask_dry(ix,iy) .eq. 1.0) then
                       dry(idry,1)=ix ; dry(idry,2)=iy
                       idry=idry+1
                    end if
                    if (mask_wet(ix,iy) .eq. 1.0) then
                       wet(iwet,1)=ix ; wet(iwet,2)=iy
                       iwet=iwet+1
                    end if
                 end do
              end do
              ! Flooding
              if (varflag(ifile) .eq. "rho") then
                 call  flood(data_in, wet, dry, x_rho, y_rho,0,data_out(1:nlon_in,1:nlat_in,1,1),&
                      & nlon_in,nlat_in,nbwet, nbdry)
              end if
              deallocate(wet) ; deallocate(dry)
           end if
           data_out(1:nlon_in,1:nlat_in,1,1)=data_out(1:nlon_in,1:nlat_in,1,1)*mask_rho(1:nlon_in,1:nlat_in)+(1.0_idx-mask_rho(1:nlon_in,1:nlat_in))*missing_value
           call writenet_wv(fname_out(ifile),varname(ifile),1,nlon_in,1,nlat_in,ilev,ilev,itime,itime,data_out)
        end do
      end do
     deallocate(data_in);deallocate(data_out)
     deallocate(mask_dry);deallocate(mask_wet);deallocate(mask_bathy)
     deallocate(lon_in); deallocate(lat_in) ;deallocate(lev_in); deallocate(time_in)
  end do

  deallocate(mask_rho); deallocate(h_rho)
  deallocate(x_rho); deallocate(y_rho)

  deallocate(fname_in); deallocate(fname_out)
  deallocate(varflag) ;   deallocate(varname)
contains
  subroutine flood(zslice, wet, dry, x, y, dmax,&
       &                 flooded_zslice, im, jm, nbwet, nbdry)

    !-----------------------------------------------------------------------

    implicit none

    !-----------------------------------------------------------------------
    !
    !     input arrays
    !
    !-----------------------------------------------------------------------

    integer, dimension(nbwet, 2), intent(in) :: wet
    integer, dimension(nbdry, 2), intent(in) :: dry
    real(idx), dimension(im, jm), intent(in) :: zslice, x, y

    !-----------------------------------------------------------------------
    !
    !     output variables
    !
    !-----------------------------------------------------------------------
    real(idx), dimension(im, jm), intent(out) :: flooded_zslice
    !-----------------------------------------------------------------------
    !
    !     local variables
    !
    !-----------------------------------------------------------------------

    integer :: im, jm, nbwet, nbdry, n, m, dmax
    integer :: idry, jdry, iwet, jwet, dmin_idx, iclose, jclose
    real(idx), dimension(nbwet) :: d

    !-----------------------------------------------------------------------
    !-----------------------------------------------------------------------

    flooded_zslice = zslice

    ! loop over the dry point that need to be flooded
    do n=1,nbdry
       idry = dry(n,1)
       jdry = dry(n,2) 
       ! compute wet point distance
       do m=1,nbwet
          iwet = wet(m,1)
          jwet = wet(m,2)
          d(m) = sqrt( (x(idry,jdry) - x(iwet,jwet)) * &
               &                 (x(idry,jdry) - x(iwet,jwet))   &
               &               + (y(idry,jdry) - y(iwet,jwet)) * &
               &                 (y(idry,jdry) - y(iwet,jwet)) )
       end do
       dmin_idx = minloc(d,1)
       if (dmax .eq. 0) then
          iclose = wet(dmin_idx,1)
          jclose = wet(dmin_idx,2)
          flooded_zslice(idry,jdry) = zslice(iclose,jclose)
       else
          if (d(dmin_idx) < dmax) then
             iclose = wet(dmin_idx,1)
             jclose = wet(dmin_idx,2)
             flooded_zslice(idry,jdry) = zslice(iclose,jclose)
          end if
       end if
    end do
    !-----------------------------------------------------------------------
  end subroutine flood
  !-----------------------------------------------------------------------
end program do_flood
