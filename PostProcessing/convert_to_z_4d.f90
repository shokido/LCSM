program convert_to_z_4d
  use ncdf_read
  use ncdf_write
  ! ifort  -o exec_convert_to_z_4d.out convert_to_z_4d.f90 $FPATH_FORT $LPATH_FORT -lncdf_read  -lncdf_write -lnetcdf -lnetcdff
  implicit none
  integer,parameter :: idx = 8,maxlen=400
  integer :: nfile
  character(len=maxlen),allocatable :: fname_strf(:),fname_ocn(:),fname_out(:)
  character(len=maxlen) :: fname_grid,fname_lev
  character(maxlen) :: time_unit,lev_unit,lat_unit,lon_unit
  integer :: nlon,nlat,nlev,nm
  integer :: nlev_out
  integer :: ix,iy,iz,im,it
  integer :: iz_dw,iz_up
  real(idx), allocatable :: lev(:),phi(:,:,:,:),phidz(:,:,:,:),psi_w(:,:,:,:)
  real(idx), allocatable :: lon(:),lat(:),x(:),y(:)
  real(idx), allocatable :: lev_out(:)
  integer :: nx_p,ny_p,nx_u,ny_v
  real(idx), allocatable :: x_p(:),y_p(:),x_u(:),y_v(:),lon_p(:),lat_p(:)
  real(idx),allocatable :: mask_ori(:,:),mask_rho(:,:)
  integer :: ntime
  real(idx),allocatable :: time(:)
  real(idx),allocatable :: rho(:,:,:,:),rhosum(:,:,:,:)
  real(idx),allocatable :: w(:,:,:,:),wsum(:,:,:,:)
  real(idx),allocatable :: p(:,:,:,:),psum(:,:,:,:),tmp_data_p(:,:,:,:)
  real(idx),allocatable :: u(:,:,:,:),usum(:,:,:,:),tmp_data_u(:,:,:,:)
  real(idx),allocatable :: v(:,:,:,:),vsum(:,:,:,:),tmp_data_v(:,:,:,:)
  integer :: ncid
  integer :: start_p(4),count_p(4),start_u(4),count_u(4),start_v(4),count_v(4)
  real(idx),parameter :: missing_value=9999.0
  integer,parameter :: nvar_out=5
  real(idx) :: dudx,dvdy
  real(idx) :: wgt_dw,wgt_up
  character(len=4) :: varnames_out(nvar_out),varunits_out(nvar_out)
  character :: ichar*2
  real(idx),parameter :: one_ov_g=1.0_idx/9.8_idx,rho0=1024.0_idx
  integer :: im1,im2,ifile
  namelist/param/im1,im2,nfile
  namelist/fnames/fname_grid,fname_lev,fname_strf,fname_ocn,fname_out
  open(10,file="filenames_convert_to_z_4d.nml",status="old")
  read(10,nml=param)
  write(*,*) "sa"
  allocate(fname_strf(nfile))
  allocate(fname_ocn(nfile))
  allocate(fname_out(nfile))
  read(10,nml=fnames)
  varnames_out(1)="p"; varunits_out(1)="m^2/s^2"
  varnames_out(2)="u"; varunits_out(2)="m/s"
  varnames_out(3)="v"; varunits_out(3)="m/s"
  varnames_out(4)="rho"; varunits_out(4)=""
  varnames_out(5)="w"; varunits_out(5)="m/s"

  open(20,file=fname_lev,status="old")
  read(20,*) nlev_out
  allocate(lev_out(nlev_out)) !;   allocate(var_1d(nlev_out))
  do iz = 1,nlev_out
     read(20,*) lev_out(iz)
  end do
  close(20)

  !===============================================
  ! Read oceanic grid
  !===============================================
  do ifile=1,nfile
     ! Get vertical coordinate
     call get_variable(trim(fname_strf(ifile)),"lev","lev",nlev,lev)
     call get_attribute(trim(fname_strf(ifile)),"lev","units",lev_unit)
     ! Get modal function
     call get_variable(trim(fname_strf(ifile)),"lon","lat","lev","mode","phi",nx_p,ny_p,nlev,nm,phi)
     call get_variable(trim(fname_strf(ifile)),"lon","lat","lev","mode","phidz",nx_p,ny_p,nlev,nm,phidz)
     call get_variable(trim(fname_strf(ifile)),"lon","lat","lev","mode","psi_w",nx_p,ny_p,nlev,nm,psi_w)


     call get_variable(trim(fname_ocn(ifile)),"x_p","x_p",nx_p,x_p)
     call get_variable(trim(fname_ocn(ifile)),"y_p","y_p",ny_p,y_p)
     call get_variable(trim(fname_ocn(ifile)),"x_p","lon_p",nx_p,lon_p)
     call get_variable(trim(fname_ocn(ifile)),"y_p","lat_p",ny_p,lat_p)
     call get_variable(trim(fname_ocn(ifile)),"x_u","x_u",nx_u,x_u) 
     call get_variable(trim(fname_ocn(ifile)),"y_v","y_v",ny_v,y_v)
     call get_variable(trim(fname_grid),"x_p","y_p","mask_p",nx_p,ny_p,mask_ori)

     write(*,*) nx_p,ny_p
     nlon=nx_p-2
     nlat=ny_p-2
     allocate(x(nlon)) ; allocate(y(nlat))
     x(1:nlon)=x_p(2:nx_p-1) ; y=y_p(2:ny_p-1)
     allocate(lon(nlon)) ; allocate(lat(nlat))
     lon(1:nlon)=lon_p(2:nx_p-1) ; lat=lat_p(2:ny_p-1)
     allocate(mask_rho(1:nlon,1:nlat))
     mask_rho(1:nlon,1:nlat)=mask_ori(2:nx_p-1,2:ny_p-1)

     call get_variable(trim(fname_ocn(ifile)),"time","time",ntime,time)
     call get_attribute(trim(fname_ocn(ifile)),"time","units",time_unit)
!     ntime=2
     !===============================================!
     !Output file                                    !
     !===============================================!
     write(*,*) "Create output file"
     call writenet_pre(trim(fname_out(ifile)),nlon,nlat,nlev_out,ntime,"lon","lat","lev","time",&
          & "degrees_east","degrees_north","m",trim(time_unit),&
          & lon,lat,lev_out,time)
     call writenet_dv(trim(fname_out(ifile)),"lon",1,(/"x"/),(/"m"/),missing_value)
     call writenet_wv(trim(fname_out(ifile)),"x",1,nlon,x)
     call writenet_dv(trim(fname_out(ifile)),"lat",1,(/"y"/),(/"m"/),missing_value)
     call writenet_wv(trim(fname_out(ifile)),"y",1,nlat,y)
     call writenet_dv(trim(fname_out(ifile)),"lon","lat",1,(/"mask_p"/),(/""/),missing_value)
     call writenet_wv(trim(fname_out(ifile)),"mask_p",1,nlon,1,nlat,mask_rho)
     call writenet_dv(trim(fname_out(ifile)),"lon","lat","lev","time",nvar_out,varnames_out,varunits_out,missing_value)
     write(*,*) "Complete file creation"
     allocate(p(1:nlon,1:nlat,1:nlev_out,1)); allocate(psum(1:nlon,1:nlat,1:nlev_out,1))
     allocate(u(1:nlon,1:nlat,1:nlev_out,1)); allocate(usum(1:nlon,1:nlat,1:nlev_out,1))
     allocate(v(1:nlon,1:nlat,1:nlev_out,1)); allocate(vsum(1:nlon,1:nlat,1:nlev_out,1))
     allocate(rho(1:nlon,1:nlat,1:nlev_out,1)); allocate(rhosum(1:nlon,1:nlat,1:nlev_out,1))
     allocate(w(1:nlon,1:nlat,1:nlev_out,1)); allocate(wsum(1:nlon,1:nlat,1:nlev_out,1))
!     ntime=1
     do it = 1,ntime
        write(*,*) it
        usum=0.0_idx ; vsum=0.0_idx ; psum=0.0_idx; rhosum=0.0_idx ; wsum=0.0_idx
        do im = im1,im2
           call get_variable(fname_ocn(ifile),"p",2,nlon+1,2,nlat+1,im,im,it,it,tmp_data_p)
           call get_variable(fname_ocn(ifile),"u",1,nlon+1,2,nlat+1,im,im,it,it,tmp_data_u)
           call get_variable(fname_ocn(ifile),"v",2,nlon+1,1,nlat+1,im,im,it,it,tmp_data_v)
           do iz = 1,nlev_out
              if (lev_out(iz) .ge. lev(nlev)) then
                 iz_dw=nlev-1
              else if (lev_out(iz) .le. lev(1)) then
                 iz_dw=1
              else
                 iz_dw=sum(maxloc(lev,MASK=(lev<=lev_out(iz))))
                 iz_dw=min(nlev-1,iz_dw)
                 iz_dw=max(1,iz_dw)
              end if
              iz_up=iz_dw+1
              wgt_dw=(lev(iz_up)-lev_out(iz))/(lev(iz_up)-lev(iz_dw))
              wgt_up=(lev_out(iz)-lev(iz_dw))/(lev(iz_up)-lev(iz_dw))
              do iy = 1,nlat
                 do ix = 1,nlon
                    ! P loop
                    if (tmp_data_p(ix,iy,1,1) .ne. missing_value) then
                       p(ix,iy,iz,1)= tmp_data_p(ix,iy,1,1)*(wgt_dw*phi(ix+1,iy+1,iz_dw,im)+wgt_up*phi(ix+1,iy+1,iz_up,im))*rho0
                       psum(ix,iy,iz,1)=psum(ix,iy,iz,1)+p(ix,iy,iz,1)
                       rho(ix,iy,iz,1)=tmp_data_p(ix,iy,1,1)*(wgt_dw*phidz(ix+1,iy+1,iz_dw,im)+wgt_up*phidz(ix+1,iy+1,iz_up,im))*(-1.0_idx)*one_ov_g*rho0
                       rhosum(ix,iy,iz,1)=rhosum(ix,iy,iz,1)+rho(ix,iy,iz,1)
                    else
                       psum(ix,iy,iz,1)= missing_value
                       p(ix,iy,iz,1)= missing_value
                       rhosum(ix,iy,iz,1)= missing_value
                       rho(ix,iy,iz,1)= missing_value
                    end if
                    ! U loop
                    if (tmp_data_u(ix,iy,1,1) .ne. missing_value .and. tmp_data_u(ix+1,iy,1,1) .ne. missing_value) then
                       u(ix,iy,iz,1)= 0.5_idx*(tmp_data_u(ix,iy,1,1)+tmp_data_u(ix+1,iy,1,1))*(wgt_dw*phi(ix+1,iy+1,iz_dw,im)+wgt_up*phi(ix+1,iy+1,iz_up,im))
                       usum(ix,iy,iz,1)=usum(ix,iy,iz,1)+u(ix,iy,iz,1)
                    else
                       usum(ix,iy,iz,1)= missing_value
                       u(ix,iy,iz,1)= missing_value
                    end if
                    ! V loop
                    if (tmp_data_v(ix,iy,1,1) .ne. missing_value .and. tmp_data_v(ix,iy+1,1,1) .ne. missing_value) then
                       v(ix,iy,iz,1)= 0.5_idx * (tmp_data_v(ix,iy,1,1)+tmp_data_v(ix,iy+1,1,1))*(wgt_dw*phi(ix+1,iy+1,iz_dw,im)+wgt_up*phi(ix+1,iy+1,iz_up,im))
                       vsum(ix,iy,iz,1)=vsum(ix,iy,iz,1)+v(ix,iy,iz,1)
                    else
                       vsum(ix,iy,iz,1)= missing_value
                       v(ix,iy,iz,1)= missing_value
                    end if
                    ! W loop
                    if (tmp_data_u(ix,iy,1,1) .ne. missing_value .and. tmp_data_u(ix+1,iy,1,1) .ne. missing_value &
                         & .and. tmp_data_v(ix,iy,1,1) .ne. missing_value .and. tmp_data_v(ix,iy+1,1,1) .ne. missing_value) then
                       dudx=(tmp_data_u(ix+1,iy,1,1)-tmp_data_u(ix,iy,1,1))/(x_u(ix+1)-x_u(ix))
                       dvdy=(tmp_data_v(ix,iy+1,1,1)-tmp_data_v(ix,iy,1,1))/(y_v(iy+1)-y_v(iy))
                       w(ix,iy,iz,1)=-1.0_idx*(dudx+dvdy)*(wgt_dw*psi_w(ix+1,iy+1,iz_dw,im)+wgt_up*psi_w(ix+1,iy+1,iz_up,im))
!                       psi_w(ix+1,iy+1,iz,im)
                       wsum(ix,iy,iz,1)=wsum(ix,iy,iz,1)+w(ix,iy,iz,1)
                    else
                       wsum(ix,iy,iz,1)= missing_value
                       w(ix,iy,iz,1)= missing_value
                    end if
                    ! if (ix .eq. 47 .and. iy .eq. 25 .and. iz .eq. 1) then
                    !    write(*,*) usum(ix,iy,iz,1),u(ix,iy,iz,1),phi(ix+1,iy+1,iz_dw,im)
                    !    write(*,*) tmp_data_v(ix,iy+1,1,1)                    
                    !    write(*,*)
                    ! end if

                 end do
              end do

           end do
        end do
        call writenet_wv(trim(fname_out(ifile)),varnames_out(1),1,nlon,1,nlat,1,nlev_out,it,it,psum)
        call writenet_wv(trim(fname_out(ifile)),varnames_out(2),1,nlon,1,nlat,1,nlev_out,it,it,usum)
        call writenet_wv(trim(fname_out(ifile)),varnames_out(3),1,nlon,1,nlat,1,nlev_out,it,it,vsum)
        call writenet_wv(trim(fname_out(ifile)),varnames_out(4),1,nlon,1,nlat,1,nlev_out,it,it,rhosum)
        call writenet_wv(trim(fname_out(ifile)),varnames_out(5),1,nlon,1,nlat,1,nlev_out,it,it,wsum)
     end do
     deallocate(p); deallocate(psum); deallocate(tmp_data_p)
     deallocate(u); deallocate(usum); deallocate(tmp_data_u)
     deallocate(v); deallocate(vsum); deallocate(tmp_data_v)
     deallocate(w); deallocate(wsum)
     deallocate(rho); deallocate(rhosum)
     deallocate(x); deallocate(y);deallocate(mask_rho)
     deallocate(lon); deallocate(lat)
  end do
  deallocate(fname_strf)
  deallocate(fname_ocn)
  deallocate(fname_out)
end program convert_to_z_4d
