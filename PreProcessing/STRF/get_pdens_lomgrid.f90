program get_pdens_2d
  ! Get density
  ! ifort -o exec_get_pdens_2d.out get_pdens_2d.f90 $FPATH_FORT $LPATH_FORT -lnetcdf -lnetcdff -lncdf_read -lncdf_write
  use ncdf_read
  use ncdf_write
  implicit none
  integer,parameter :: idx = 8,maxlen=400
  integer :: nlon,nlat,nlev,ntime
  character(len=maxlen) :: lonname,latname,levname,timename
  character(len=maxlen) :: fname_temp,varname_temp
  character(len=maxlen) :: fname_salt,varname_salt
  character(len=maxlen) :: fname_dens
  character(len=maxlen) :: lon_unit,lat_unit,lev_unit,time_unit
  integer :: nlon_in,nlat_in,nlev_in,ntime_in
  real(idx),allocatable :: lon(:),lat(:),lev(:),time(:)
  real(idx),allocatable :: tmp_2d(:,:)
  real(idx),allocatable :: temp_in(:,:,:,:),salt_in(:,:,:,:)
  integer :: ilon,ilat,ilev,itime
  real(idx),allocatable :: dens(:,:,:,:)
  real(idx),parameter :: missing_value=1.0e20,abs_range=1.0e3
  namelist/fflag/lonname,latname,levname,timename
  namelist/temp/fname_temp,varname_temp
  namelist/salt/fname_salt,varname_salt
  namelist/pdens/fname_dens
  open(10,file='filename_get_pdens_lomgrid.nml',status='old')
  read(10,fflag)
  read(10,temp)
  read(10,salt)
  read(10,pdens)
  close(10)
  call get_variable(trim(fname_temp),"x","y",lonname,nlon,nlat,tmp_2d)
  allocate(lon(nlon))
  lon(1:nlon)=tmp_2d(1:nlon,1)
  allocate(lat(nlat))
  call get_variable(trim(fname_temp),"x","y",latname,nlon,nlat,tmp_2d)
  lat(1:nlat)=tmp_2d(1,1:nlat)
  call get_attribute(trim(fname_temp),lonname,"units",lon_unit)
  call get_attribute(trim(fname_temp),latname,"units",lat_unit)
  call get_variable(trim(fname_temp),levname,levname,nlev,lev)
  call get_attribute(trim(fname_temp),levname,"units",lev_unit)
  call get_variable(trim(fname_temp),timename,timename,ntime,time)
  call get_attribute(trim(fname_temp),timename,"units",time_unit)
  allocate(temp_in(1:nlon,1:nlat,1:nlev,1:1))
  allocate(salt_in(1:nlon,1:nlat,1:nlev,1:1))
  allocate(dens(1:nlon,1:nlat,1:nlev,1:1))
  ! Prepare output file
  call writenet_pre(trim(fname_dens),nlon,nlat,nlev,ntime,"lon","lat","lev","time",&
       & trim(lon_unit),trim(lat_unit),trim(lev_unit),trim(time_unit),&
       & lon,lat,lev,time)
  call writenet_dv(trim(fname_dens),"lon","lat","lev","time",1,(/"dens"/),(/"kg/m^3"/),missing_value)
  write(*,*) "Finish definition"
  do itime = 1,ntime
     write(*,*) itime
     call get_variable(trim(fname_temp),varname_temp,1,nlon,1,nlat,1,nlev,itime,itime,temp_in)
     call get_variable(trim(fname_salt),varname_salt,1,nlon,1,nlat,1,nlev,itime,itime,salt_in)  
     do ilev = 1,nlev
        do ilat = 1,nlat
           do ilon = 1,nlon
              if (abs(temp_in(ilon,ilat,ilev,1)).le. abs_range) then
                 dens(ilon,ilat,ilev,1)=cal_dens_sptp(salt_in(ilon,ilat,ilev,1),temp_in(ilon,ilat,ilev,1),0.0_idx)
              else
                 dens(ilon,ilat,ilev,1)=missing_value
              end if
           end do
        end do
     end do
     call writenet_wv(trim(fname_dens),"dens",1,nlon,1,nlat,1,nlev,itime,itime,dens)

  end do
  deallocate(lon) ; deallocate(lat) ; deallocate(lev) ;deallocate(time)
  deallocate(temp_in) ; deallocate(salt_in)
  deallocate(dens)
contains
  function cal_dens_sptp(s,t,p) result(dens)
    !!------------------------------------------------------------
    !! calculate in-situ density of seawater from
    !! salinity, potential temperature, pressure, and reference temperature
    !! Based on Appendix. B of Mcdougall et al. (2003) ;
    !! "Accurate and Computationally Efficient Algorithms for Potential Temperature and Density of Seawater"
    !!------------------------------------------------------------
    implicit none
    integer,parameter :: idx=8
    real(idx) :: s,t,p
    real(idx) :: consn,consd
    real(idx) :: t1n,t2n,t3n,s1n,s1t1n,s2n
    real(idx) :: p1n,p1t2n,p1s1n,p2n,p2t2n
    real(idx) :: t1d,t2d,t3d,t4d
    real(idx) :: s1d,s1t1d,s1t3d,s15d,s15t2d
    real(idx) :: p1d,p2t3d,p3t1d
    real(idx) :: P1,P2,dens
    !! Calculate density of seawater
    !!
    consn=9.99843699e2_idx
    t1n = 7.35212840e0_idx  ; t2n = -5.45928211e-2_idx ; t3n = 3.98476704e-4_idx
    s1n = 2.96938239e0_idx  ; s1t1n = -7.23268813e-3_idx ; s2n = 2.12382341e-3_idx
    p1n = 1.04004591e-2_idx ; p1t2n = 1.03970529e-7_idx ; p1s1n = 5.18761880e-6_idx  
    p2n = -3.24041825e-8_idx ; p2t2n = -1.23869360e-11_idx
    P1 = consn + t1n * t + t2n * (t**2.0_idx) + t3n * (t**3.0_idx) + &
         & s1n * s + s1t1n * (s*t) + s2n * (s**2.0_idx) + &
         & p1n * p + p1t2n * (p * t**2.0_idx) + p1s1n * (p*s) + &
         & p2n * (p**2.0_idx) + p2t2n * (p**2.0_idx)*(t**2)
    consd = 1.0_idx
    t1d = 7.28606739e-3_idx  ; t2d = -4.60835542e-5_idx
    t3d = 3.68390573e-7_idx  ; t4d = 1.80809186e-10_idx
    s1d = 2.14691708e-3_idx  ; s1t1d = -9.27062484e-6_idx
    s1t3d = -1.78343643e-10_idx
    s15d = 4.76534122e-6_idx ; s15t2d = 1.63410736e-9_idx
    p1d = 5.30848875e-6_idx ; p2t3d = -3.03175128e-16_idx ; p3t1d = -1.27934137e-17_idx  
    P2 = consd + t1d * t + t2d * (t**2_idx) + t3d * (t**3_idx) + t4d * (t**4_idx) + &
         & s1d * s + s1t1d * s*t + s1t3d * s * (t**3_idx) + &
         & + s15d * (s**1.5_idx) + s15t2d * (s**1.5_idx) * (t**2_idx) + &
         & p1d * p + p2t3d * (p**2_idx)* (t**3_idx) + p3t1d * (p**3_idx)*t
    dens = P1 / P2
  end function cal_dens_sptp
end program get_pdens_2d
