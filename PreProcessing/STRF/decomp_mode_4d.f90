program decomp_mode_4d
  use ncdf_read
  use ncdf_write
  implicit none
  ! ifort  -o exec_decomp_mode_4d.out decomp_mode_4d.f90 $FPATH_FORT $LPATH_FORT -lncdf_read  -lncdf_write -lnetcdf -lnetcdff -fpp -llapack -llapsub
  integer,parameter :: idx=8,maxlen=400
  integer :: nlon_in,nlat_in,nz_in,ntime_in
  real(idx),allocatable :: lon_in(:),lat_in(:),lev_in(:),time_in(:),pdens_in(:,:,:,:)
  character(len=maxlen) :: fname_pdens,varname_lon,varname_lat,varname_lev,varname_time,varname_pdens
  integer :: nz_out
  character(len=maxlen) :: lon_units,lat_units,lev_units,time_units,mode_units
  real(idx),allocatable :: lev_out(:)
  real(idx),allocatable :: cn_out(:,:,:,:),bn_out(:,:,:,:),hn_out(:,:,:,:),bvf_out(:,:,:,:)
  real(idx),allocatable :: phi_out(:,:,:,:,:),phidz_out(:,:,:,:,:),psi_w_out(:,:,:,:,:)

  integer :: nmode
  real(idx),allocatable :: mode_out(:),cn(:),bn(:),hn(:)
  real(idx) :: missing_value
  integer :: im,iz,imode,itime,ilon,ilat
  character(len=maxlen) :: fname_out
  real(idx),parameter :: g=9.8_idx,rho0=1024.0_idx
  integer :: nz
  real(idx) :: H,lambda
  real(idx),allocatable :: s_psi(:),lev_psi(:),lev_phi(:),lev_bvf(:),pdens(:),bvf(:)
  real(idx),allocatable :: phi_tmp(:,:),phidz_tmp(:,:),psi_w_tmp(:,:)
  real(idx),allocatable :: lev_psi_new(:),lev_phi_new(:)
  real(idx) :: ref_dens,mld,lower,upper
  integer :: mld_ind
  namelist/params/nz,H,lambda
  namelist/modes/nmode
  namelist/fnames/fname_pdens,varname_lon,varname_lat,varname_lev,varname_time,varname_pdens,fname_out
  open(10,file="filename_decomp_mode_4d.nml",status="old")
  read(10,nml=params)
  ! Allocate neccesarry arrays
  allocate(s_psi(nz+1)); allocate(lev_psi(nz+1))
  allocate(lev_phi(nz)); allocate(lev_bvf(nz-1))
  allocate(pdens(nz)) ; allocate(bvf(nz-1))
  allocate(lev_psi_new(nz+1));  allocate(lev_phi_new(nz))
  read(10,nml=modes)
  allocate(mode_out(nmode)); allocate(cn(nmode))
  allocate(bn(nmode)) ; allocate(hn(nmode))
  allocate(phi_tmp(nz,nmode))
  allocate(phidz_tmp(nz+1,nmode))
  allocate(psi_w_tmp(nz+1,nmode))
  read(10,nml=fnames)
  close(10)
  
  ! Read netcdf file
  ! Pdens
  call get_variable(fname_pdens,varname_lon,varname_lon,nlon_in,lon_in)
  call get_attribute(fname_pdens,varname_lon,"units",lon_units)
  call get_variable(fname_pdens,varname_lat,varname_lat,nlat_in,lat_in)
  call get_attribute(fname_pdens,varname_lat,"units",lat_units)
  call get_variable(fname_pdens,varname_lev,varname_lev,nz_in,lev_in)
  call get_variable(fname_pdens,varname_time,varname_time,ntime_in,time_in)
  call get_attribute(fname_pdens,varname_time,"units",time_units)
  allocate(pdens_in(nlon_in,nlat_in,nz_in,1))
  ! Prepare common vertical grid
  do iz = 1,nz
     s_psi(iz) = real((iz-1),idx) / real((nz+1),idx)
     lev_psi(iz)= H *log(1.0_idx - s_psi(iz)*(1.0_idx - exp(lambda))) / lambda
  end do
  iz = nz+1
  s_psi(nz+1)=1.0_idx
  lev_psi(iz)= H *log(1.0_idx - s_psi(iz)*(1.0_idx - exp(lambda))) / lambda
  lev_phi(1:nz)=0.5_idx*(lev_psi(1:nz)+lev_psi(2:nz+1))

  nz_out=nz
  allocate(lev_out(nz_out))
  lev_out(1:nz_out)=lev_phi(1:nz)

  allocate(bvf_out(nlon_in,nlat_in,nz_out,1)) 
  allocate(cn_out(nlon_in,nlat_in,nmode,1))
  allocate(bn_out(nlon_in,nlat_in,nmode,1))
  allocate(hn_out(nlon_in,nlat_in,nmode,1))
  allocate(phi_out(nlon_in,nlat_in,nz_out,nmode,1))
  allocate(phidz_out(nlon_in,nlat_in,nz_out,nmode,1))
  allocate(psi_w_out(nlon_in,nlat_in,nz_out,nmode,1))

  do im = 1,nmode
     mode_out(im) = im
  end do
  lev_units="m"
  mode_units=""
  call get_attribute(fname_pdens,varname_pdens,"missing_value",missing_value)
  call writenet_pre(trim(fname_out),nlon_in,nlat_in,nz,nmode,ntime_in,&
       & "lon","lat","lev","mode","time",&
       & lon_units,lat_units,lev_units,mode_units,time_units,lon_in,lat_in,lev_out,mode_out,time_in)
  call writenet_dv(trim(fname_out),"lon","lat","mode","time",1,(/"cn"/),(/"m/s"/),missing_value)
  call writenet_dv(trim(fname_out),"lon","lat","mode","time",2,(/"ohn","obn"/),(/"1/m","1/m"/),missing_value)
  call writenet_dv(trim(fname_out),"lon","lat","lev","time",1,(/"bvf"/),(/"1/s^2"/),missing_value)
  call writenet_dv(trim(fname_out),"lon","lat","lev","mode","time",3,(/"phi","phidz","psi_w"/),&
       & (/"m","m","m"/),missing_value)

  write(*,*) "lon=",nlon_in,"lat=",nlat_in

  do itime = 1,ntime_in
     call get_variable(fname_pdens,varname_pdens,1,nlon_in,1,nlat_in,1,nz_in,itime,itime,pdens_in)
     do ilat =1,nlat_in
        write(*,*) "ilat=",ilat
        do ilon = 1,nlon_in
           if (pdens_in(ilon,ilat,1,1) .ne. missing_value) then
              ! Vertical intepolation
              call linear_int(nz_in,lev_in,pdens_in(ilon,ilat,1:nz_in,1),nz,lev_phi,pdens)
              ! Calculate bvf
              do iz = 1,nz-1
                 upper=pdens(iz)
                 lower=pdens(iz+1)
                 bvf(iz) = -1.0_idx * g * (upper-lower) / (rho0 * (lev_phi(iz+1)-lev_phi(iz)))
                 bvf(iz)=max(bvf(iz),1.0e-6)
                 lev_bvf(iz) = 0.5_idx*(lev_phi(iz)+lev_phi(iz+1))
              end do
              ! Get MLD
              ref_dens=pdens(1)+0.125_idx
              mld_ind=sum(minloc(pdens,MASK=(pdens >= ref_dens)))
              mld=lev_phi(mld_ind)
              call cal_vm_raw(nz,lev_psi,bvf,nmode,mld,cn,hn,bn,lev_psi_new,lev_phi_new,phi_tmp,phidz_tmp,psi_w_tmp)
              call linear_int(nz-1,lev_bvf(1:nz-1),bvf(1:nz-1),nz_out,lev_out,bvf_out(ilon,ilat,1:nz_out,1))
              do im = 1,nmode
                 call linear_int(nz,lev_phi_new(1:nz),phi_tmp(1:nz,im),nz_out,lev_out,phi_out(ilon,ilat,1:nz_out,im,1))
                 call linear_int(nz+1,lev_psi_new(1:nz+1),phidz_tmp(1:nz+1,im),nz_out,lev_out,phidz_out(ilon,ilat,1:nz_out,im,1))
                 call linear_int(nz+1,lev_psi_new(1:nz+1),psi_w_tmp(1:nz+1,im),nz_out,lev_out,psi_w_out(ilon,ilat,1:nz_out,im,1))
              end do
              bn_out(ilon,ilat,1:nmode,1)=bn(1:nmode)
              cn_out(ilon,ilat,1:nmode,1)=cn(1:nmode)
              hn_out(ilon,ilat,1:nmode,1)=hn(1:nmode)
              bvf_out(ilon,ilat,1:nz_out,1)=bvf(1:nz_out)
           else
              bn_out(ilon,ilat,1:nmode,1)=missing_value
              cn_out(ilon,ilat,1:nmode,1)=missing_value
              hn_out(ilon,ilat,1:nmode,1)=missing_value
              bvf_out(ilon,ilat,1:nz_out,1)=missing_value
              phi_out(ilon,ilat,1:nz_out,1:nmode,1)=missing_value
              phidz_out(ilon,ilat,1:nz_out,1:nmode,1)=missing_value
              psi_w_out(ilon,ilat,1:nz_out,1:nmode,1)=missing_value
           end if
        end do
     end do
     call writenet_wv(trim(fname_out),"cn",1,nlon_in,1,nlat_in,1,nmode,itime,itime,cn_out)
     call writenet_wv(trim(fname_out),"obn",1,nlon_in,1,nlat_in,1,nmode,itime,itime,bn_out)
     call writenet_wv(trim(fname_out),"ohn",1,nlon_in,1,nlat_in,1,nmode,itime,itime,hn_out)
     call writenet_wv(trim(fname_out),"bvf",1,nlon_in,1,nlat_in,1,nz_out,itime,itime,bvf_out)
     call writenet_wv(trim(fname_out),"phi",1,nlon_in,1,nlat_in,1,nz_out,1,nmode,itime,itime,phi_out)
     call writenet_wv(trim(fname_out),"phidz",1,nlon_in,1,nlat_in,1,nz_out,1,nmode,itime,itime,phidz_out)
     call writenet_wv(trim(fname_out),"psi_w",1,nlon_in,1,nlat_in,1,nz_out,1,nmode,itime,itime,psi_w_out)
  end do

  deallocate(mode_out); deallocate(cn); deallocate(bn) ; deallocate(hn)
  deallocate(phi_tmp)
  deallocate(phidz_tmp)
  deallocate(psi_w_tmp)
  deallocate(bvf_out)
  deallocate(phi_out); deallocate(phidz_out);deallocate(psi_w_out)
  deallocate(lev_in); deallocate(pdens_in)
  deallocate(s_psi); deallocate(lev_psi)
  deallocate(lev_phi); deallocate(lev_bvf)
  deallocate(pdens)  ; deallocate(bvf)
  deallocate(lev_psi_new);  deallocate(lev_phi_new)
contains
  subroutine linear_int(N,level,data,newN,newlev,newdata)
    implicit none
    integer,intent(in) :: N
    real(idx),intent(in) :: level(n),data(n)
    integer,intent(in) :: newN
    real(idx),intent(in) :: newlev(newN)
    real(idx),intent(inout) :: newdata(newN)
    real(idx) :: w1,w2
    real(idx) :: p0,p1,p2,p3
    integer :: mini,i
    ! check order
    ! level_in shold be ascending order
    ! calculate gradient----------------
    ! Interpolation
    do i = 1,NewN
       !     |-------|----.............-----|-------|
       !  level(1)  level(2)                  (N-1)     N
       if (newlev(i) .le. level(1)) then
          p1=(level(2)-newlev(i))/(level(2)-level(1))
          p2=(newlev(i)-level(1))/(level(2)-level(1))
          newdata(i)=p1*data(1)+p2*data(2)
       else if (newlev(i) .ge. level(N)) then
          p1=(level(n)-newlev(i))/(level(n)-level(n-1))
          p2=(newlev(i)-level(n-1))/(level(n)-level(n-1))
          newdata(i)=p1*data(n-1)+p2*data(N)
       else
          mini=maxloc(level,1,mask=(level<=newlev(i)))
          p1=(level(mini+1)-newlev(i))/(level(mini+1)-level(mini))
          p2=(newlev(i)-level(mini))/(level(mini+1)-level(mini))
          newdata(i)=p1*data(mini)+p2*data(mini+1)
       end if
    end do
  end subroutine linear_int
  subroutine linear_int_miss(ndata,level_in,data_in,newN,newlev,newdata,undef)
    implicit none
    integer,intent(in) :: ndata
    real(idx),dimension(ndata),intent(in) :: level_in,data_in
    integer,intent(in) :: newN
    real(idx),dimension(newN),intent(inout) :: newlev,newdata
    real(idx),intent(in) :: undef
    integer :: neff
    real(idx) :: level(ndata),data(ndata) ! indices larger than n is undef
    real(idx) :: w1,w2
    real(idx) :: p0,p1,p2,p3
    integer :: mini,i
    ! Remove masked region for analysis
    neff=0
    do i = 1,ndata
       if (data_in(i) .ne. undef) then
          neff = neff +1
          level(neff)=level_in(i)
          data(neff)=data_in(i)
       end if
    end do
    ! Start interpolation
    do i = 1,NewN
       !     |-------|----.............-----|-------|
       !  level(1)  level(2)                  (N-1)     N
       if (newlev(i) .le. level(1)) then
          p1=(level(2)-newlev(i))/(level(2)-level(1))
          p2=(newlev(i)-level(1))/(level(2)-level(1))
          newdata(i)=p1*data(1)+p2*data(2)
       else if (newlev(i) .ge. level(neff)) then
          p1=(level(neff)-newlev(i))/(level(neff)-level(neff-1))
          p2=(newlev(i)-level(neff-1))/(level(neff)-level(neff-1))
          newdata(i)=p1*data(neff-1)+p2*data(neff)
       else
          mini=maxloc(level,1,mask=(level<=newlev(i)))
          p1=(level(mini+1)-newlev(i))/(level(mini+1)-level(mini))
          p2=(newlev(i)-level(mini))/(level(mini+1)-level(mini))
          newdata(i)=p1*data(mini)+p2*data(mini+1)
       end if
    end do
  end subroutine linear_int_miss
  function z_step(z,H) result(ret)
    real(idx),intent(in) :: z,H
    real(idx) :: ret
    if (z .lt. H) then
       ret=1.0_idx / H
    else
       ret=0.0_idx
    end if
  end function z_step
  function z_smooth(z,H) result(ret)
    real(idx),intent(in) :: z,H
    real(idx) :: ret
    if (z .lt. H) then
       ret=2.0_idx / (3.0_idx * H)
    else if (z .lt. 2*H) then
       ret=2.0_idx *(2.0_idx*H - z)/ (3.0_idx * H * H)
    else
       ret=0.0_idx
    end if
  end function z_smooth
  ! Refine coordinate
  subroutine refine_coordinate(nz,z_psi,z_phi,bvf_in,z_psi_out,z_phi_out,bvf_out)
    implicit none
    integer,intent(in) :: nz
    real(idx),intent(in) :: z_psi(nz+1),z_phi(nz),bvf_in(nz-1)
    real(idx),intent(inout) :: z_psi_out(nz+1),z_phi_out(nz),bvf_out(nz-1)
    real(idx) :: tmp,acc(nz+1)
    real(idx) :: s_phi(nz),tmp_array(nz+1)
    integer :: iz,ind
    tmp=0.0_idx
    ! Integration
    acc(1)=0.0_idx
    iz = 1
    tmp = tmp + sqrt(bvf_in(iz))*(z_psi(iz+1)-z_psi(iz))
    acc(iz+1)=tmp
    do iz = 2,nz-1
       tmp = tmp + sqrt(0.5_idx*(bvf_in(iz-1)+bvf(iz)))*(z_psi(iz+1)-z_psi(iz))
       acc(iz+1)=tmp
    end do
    iz = nz
    tmp = tmp + sqrt(bvf_in(iz-1))*(z_psi(iz+1)-z_psi(iz))
    acc(iz+1)=tmp
    acc=acc/tmp
    do iz = 1,nz
       s_phi(iz) = (2.0_idx * iz - 1.0_idx) / real(2.0_idx * nz)
    end do
    !     Regrid
    call linear_int(nz+1,acc(1:nz+1),z_psi(1:nz+1),nz,s_phi(1:nz),z_phi_out(1:nz))
    z_psi_out(1)=0.0_idx
    z_psi_out(nz+1)=z_psi(nz+1)
    do iz = 2,nz
       z_psi_out(iz) = 0.5_idx * (z_phi_out(iz-1) + z_phi_out(iz))
    end do
    ! Interpolate buoyancy frequency
    call linear_int(nz-1,z_psi(2:nz),bvf_in(1:nz-1),nz-1,z_psi_out(2:nz),bvf_out(1:nz-1))
  end subroutine refine_coordinate
  !
  subroutine cal_vm_raw(nz_in,z_psi_in,bvf_in,nmode,mld,cn,hn,bn,z_psi_new,z_phi_new,phi_out,phidz_out,psiw_out)
    use lapsub
    implicit none
    integer,parameter :: idx=8
    integer,intent(in) :: nz_in
    real(idx),intent(in) :: z_psi_in(nz_in+1),bvf_in(nz_in-1)
    integer,intent(in) :: nmode
    real(idx),intent(in) :: mld
    real(idx),intent(inout) :: cn(nmode),hn(nmode),bn(nmode)
    real(idx),intent(inout) :: z_psi_new(nz_in+1),z_phi_new(nz_in)
    real(idx),intent(inout) :: phi_out(nz_in,nmode),phidz_out(nz_in+1,nmode),psiw_out(nz_in+1,nmode)
    real(idx) :: z_phi_in(nz_in),bvf(nz_in-1)
    real(idx) :: bvf_new(nz_in-1)
    real(idx) :: A(nz_in,nz_in)
    real(idx) :: eigval(nz_in),vec_pdens(nz_in,nz_in),vec(nz_in,nmode)
    real(idx) :: phi(nz_in),psi(nz_in+1),psi_rho(nz_in+1) !phi->u,v,p psi->w
    integer :: iz,im,ind1,ind2
    real(idx) :: tmp1,phi_top,phi_bottom,w1,w2
    real(idx),parameter :: bvf_min=1.0e-8_idx
    ! z_in shoud be increasing array
    if (z_psi_in(1) .gt. 0) then
       write(*,*) "Warning...z_psi_in(1) is considered as sea surface"
    end if
    ! Calculate buoyancy frequency
    do iz =1,nz_in
       z_phi_in(iz) = (z_psi_in(iz)+z_psi_in(iz+1)) * 0.5_idx
    end do
    bvf(1:nz_in-1)=bvf_in(1:nz_in-1)
    do iz = 1,nz_in-1
       if (bvf(iz) .le. bvf_min) then
          bvf(iz) = bvf_min
       end if
    end do
!    call refine_coordinate(nz_in,z_psi_in,z_phi_in,bvf,z_psi_new,z_phi_new,bvf_new)
    z_phi_new=z_phi_in
    z_psi_new=z_psi_in
    bvf_new=bvf_in
    do iz = 1,nz_in-1
       bvf_new(iz)=max(bvf_new(iz),bvf_min)
    end do
    ! Formulate matrix
    A = 0.0_idx
    A(1,1)=  -1.0_idx / (bvf_new(1)*(z_psi_new(2)-z_psi_new(1))*(z_phi_new(2)-z_phi_new(1)))
    A(1,2)=   1.0_idx / (bvf_new(1)*(z_psi_new(2)-z_psi_new(1))*(z_phi_new(2)-z_phi_new(1)))
    do iz=2,nz_in-1
       A(iz,iz-1) =   1.0_idx / (bvf_new(iz-1)*(z_psi_new(iz+1)-z_psi_new(iz))*(z_phi_new(iz)-z_phi_new(iz-1)))
       A(iz,iz) = -1.0_idx / (bvf_new(iz-1)*(z_psi_new(iz+1)-z_psi_new(iz))*(z_phi_new(iz)-z_phi_new(iz-1))) &
            &  -1.0_idx / (bvf_new(iz)*(z_psi_new(iz+1)-z_psi_new(iz))*(z_phi_new(iz+1)-z_phi_new(iz))) 
       A(iz,iz+1) = 1.0_idx / (bvf_new(iz)*(z_psi_new(iz+1)-z_psi_new(iz))*(z_phi_new(iz+1)-z_phi_new(iz)))
    end do
    A(nz_in,nz_in-1)   =  1.0_idx / (bvf_new(nz_in-1)*(z_psi_new(nz_in+1)-z_psi_new(nz_in))*(z_phi_new(nz_in)-z_phi_new(nz_in-1)))
    A(nz_in,nz_in)   =  -1.0_idx / (bvf_new(nz_in-1)*(z_psi_new(nz_in+1)-z_psi_new(nz_in))*(z_phi_new(nz_in)-z_phi_new(nz_in-1)))
    call cal_eig_both(nz_in,A,eigval,vec_pdens)

    ! Reshape
    do im=1,nmode
       cn(im) = sqrt(-1.0_idx / eigval(im+1))
       vec(1:nz_in,im) = vec_pdens(1:nz_in,im+1)
    end do
    !
    !  Mode function-------------------------
    !  1st mode->vec(:,1), 2nd mode ->vec(:,2)
    w1=(z_phi_new(2)-0.0_idx)/(z_phi_new(2)-z_phi_new(1))
    w2=(0.0_idx-z_phi_new(1))/(z_phi_new(2)-z_phi_new(1))
    do im=1,nmode
       Hn(im) = 0.0_idx
       phi_top=vec(1,im)*w1+vec(2,im)*w2
       do iz = 1,nz_in
          phi(iz) = vec(iz,im)/phi_top
          phi_out(iz,im)=phi(iz)
       end do
       ! Psi_w
       psi(1) = 0.0_idx ; psi(nz_in+1) = 0.0_idx
       psiw_out(1,im)=psi(1)
       psiw_out(nz_in+1,im)=psi(nz_in+1)
       do iz = nz_in,1,-1
          psi(iz) = psi(iz+1) + phi(iz) * (z_psi_new(iz+1)-z_psi_new(iz))
          psiw_out(iz,im)=psi(iz)
       end do
       ! Psi_rho
       psi_rho(1) = 0.0_idx ; psi_rho(nz_in+1) = 0.0_idx
       phidz_out(1,im)=psi_rho(1)
       phidz_out(nz_in+1,im)=psi_rho(nz_in+1)
       do iz = 2,nz_in
          psi_rho(iz) = (phi(iz) - phi(iz-1)) / (z_phi_new(iz-1)-z_phi_new(iz))
          phidz_out(iz,im)=psi_rho(iz)
       end do
       ! Integration
       tmp1=0.0_idx
       iz=1
       phi_top=phi(iz)+((phi(iz+1)-phi(iz))/(z_phi_new(iz+1)-z_phi_new(iz)))*(z_psi_new(iz)-z_phi_new(iz))

       tmp1 = tmp1 + (z_phi_new(iz)-z_psi_new(iz))*0.5_idx*(phi(iz)*phi(iz)+phi_top*phi_top)
       do iz = 1,nz_in-1
          tmp1 = tmp1 + 0.5*(phi(iz)*phi(iz)+phi(iz+1)*phi(iz+1))*(z_phi_new(iz+1)-z_phi_new(iz))
       end do
       iz=nz_in
       phi_bottom=phi(iz)+((phi(iz)-phi(iz-1))/(z_phi_new(iz)-z_phi_new(iz-1)))*(z_psi_new(iz+1)-z_phi_new(iz))
       tmp1 = tmp1 + (z_psi_new(iz+1)-z_phi_new(iz))*0.5_idx*(phi(iz)*phi(iz)+phi_bottom*phi_bottom)
       Hn(im) = 1.0_idx / tmp1

       tmp1=0.0_idx
       iz=1
       tmp1 = tmp1 + (z_phi_new(iz)-z_psi_new(iz))*0.5_idx*(phi(iz)*z_step(z_phi_new(iz),mld)+phi_top*z_step(z_psi_new(iz),mld))
       do iz = 1,nz_in-1
          tmp1 = tmp1 + 0.5*(phi(iz)*z_step(z_phi_new(iz),mld)+phi(iz+1)*z_step(z_phi_new(iz+1),mld))*&
               & (z_phi_new(iz+1)-z_phi_new(iz))
       end do
       iz=nz_in
       tmp1 = tmp1 + (z_psi_new(iz+1)-z_phi_new(iz))*0.5_idx*(phi(iz)*z_step(z_phi_new(iz),mld)+phi_bottom*z_step(z_psi_new(iz+1),mld))
       bn(im)=Hn(im)*tmp1
    end do
  end subroutine cal_vm_raw
end program decomp_mode_4d
