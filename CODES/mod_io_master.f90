module mod_io_master
  use run_param
  use run_types
  use calendar_sub
  use ncdf_read
  implicit none  
contains
  function modify_time(ntime,time_in,time_units,start_yymmdd,start_hhmmss) result(time)
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
    case("minutes")
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
       call calendar_cal_length_ymdhms(start_yymmdd,start_hhmmss,tmp_yymmdd,tmp_hhmmss,1,time(it))
    end do
  end function modify_time
  subroutine calc_doy_from_units(ntime, time_tmp, time_units, doy)
      implicit none
      integer, intent(in) :: ntime
      real(idx), intent(in) :: time_tmp(ntime)
      character(len=*), intent(in) :: time_units
      real(idx), intent(out) :: doy(ntime)
      integer :: stamp_ind
      integer :: ref_yymmdd, ref_hhmmss
      integer :: it, ymd, hms, year, year0101
      real(idx) :: tmp
      call get_reftime(trim(time_units), stamp_ind, ref_yymmdd, ref_hhmmss)
      do it = 1, ntime
         call calendar_cal_ymdhms_after(ref_yymmdd, ref_hhmmss, time_tmp(it), stamp_ind, ymd, hms)
         year = ymd/10000
         year0101 = year*10000 + 101
         tmp = 0.0_idx
         call calendar_cal_day_length_ymdhms(year0101, 0, ymd, hms, tmp)
         doy(it) = tmp
   end do
   end subroutine calc_doy_from_units
   subroutine get_time_cyc_now(time_now, start_yymmdd, start_hhmmss, time_cyc_now)
      implicit none
      real(idx), intent(in) :: time_now
      integer, intent(in) :: start_yymmdd, start_hhmmss
      real(idx), intent(out) :: time_cyc_now
      integer :: ymd, hms, year, year0101
      real(idx) :: tmp
      call calendar_cal_ymdhms_after(start_yymmdd, start_hhmmss, time_now, 1, ymd, hms) ! flag=1: day
      year = ymd/10000
      year0101 = year*10000 + 101
      tmp = 0.0_idx
      call calendar_cal_day_length_ymdhms(year0101, 0, ymd, hms, tmp)
      time_cyc_now = tmp
   end subroutine get_time_cyc_now  
   subroutine time_wgt(time_array,time,i1,i2,w1,w2)
    real(idx),intent(in) :: time_array(:),time
    integer,intent(out) :: i1,i2    
    integer :: ntime
    real(idx) :: w1,w2
    ntime=size(time_array)
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
  subroutine time_wgt_cyclic(time_cyc_array, time_cyc_now, Tcycle, i1, i2, w1, w2)
      implicit none
      real(idx), intent(in) :: time_cyc_array(:)
      real(idx), intent(in) :: time_cyc_now, Tcycle
      integer, intent(out) :: i1, i2
      real(idx), intent(out) :: w1, w2
      integer :: ntime
      real(idx) :: tmin, tmax, dt, tnow
      ntime = size(time_cyc_array)
      tmin = minval(time_cyc_array)
      tmax = maxval(time_cyc_array)
      if (ntime == 1) then
         i1=1; i2=1; w1=1.0_idx; w2=0.0_idx
         return
      end if
      if (time_cyc_now >= tmin .and. time_cyc_now <= tmax) then
         i1 = sum(maxloc(time_cyc_array, mask=(time_cyc_array<=time_cyc_now)))
         i2 = min(i1+1, ntime)
         if (i1 == i2) then
            w1=1.0_idx; w2=0.0_idx
         else
            w1 = (time_cyc_array(i2)-time_cyc_now) / (time_cyc_array(i2)-time_cyc_array(i1))
            w2 = 1.0_idx - w1
         end if
      else
         i1 = ntime
         i2 = 1
         if (time_cyc_now < tmin) then
            tnow = time_cyc_now + Tcycle
         else
            tnow = time_cyc_now
         end if

         dt = (Tcycle - time_cyc_array(i1)) + time_cyc_array(i2)
         w2 = (tnow - time_cyc_array(i1)) / dt
         w1 = 1.0_idx - w2
      end if
  end subroutine time_wgt_cyclic
  function set_data(ind1,ind2,wgt1,wgt2,data_1d) result(data_ret)
    implicit none
    integer,intent(in) :: ind1,ind2
    real(idx),intent(in) :: wgt1,wgt2
    real(idx),intent(in) :: data_1d(:)
    real(idx) :: data_ret
    data_ret= wgt1*data_1d(ind1)+wgt2*data_1d(ind2)
  end function set_data
  subroutine read_data_TLL_p(nfile,fnames,timename,varname,nx,ny,wgrd,start_yymmdd,start_hhmmss)
    implicit none
    integer,intent(in) :: nfile
    character(len=maxlen),intent(in) :: fnames(nfile),timename,varname
    integer,intent(in) :: nx,ny,start_yymmdd,start_hhmmss
    type(TLL_dta),intent(inout) :: wgrd
    integer :: nt,ntime
    real(idx),allocatable :: time_tmp(:),time_cyc_tmp(:),time(:),time_out(:),v_3d(:,:,:)
    character(len=maxlen) :: time_units
    integer :: ifile,it1,it2
    ntime=0
    do ifile=1,nfile
       call get_dimsize(fnames(ifile),timename,nt)
       ntime=ntime+nt
    end do

    it1=1;it2=0
    allocate(wgrd%time%val(1:ntime))
    allocate(wgrd%time_cyc%val(1:ntime)) 
    allocate(wgrd%data%val(0:nx+1,0:ny+1,1:ntime))
    allocate(wgrd%data_now%val(0:nx+1,0:ny+1))
    allocate(wgrd%data_mod%val(0:nx+1,0:ny+1))

    do ifile=1,nfile
       call get_dimsize(trim(fnames(ifile)),timename,nt)
       call get_variable(trim(fnames(ifile)),timename,(/1/),(/nt/),time_tmp)
       call get_attribute(trim(fnames(ifile)),timename,"units",time_units)
       allocate(time(nt))
       allocate(time_cyc_tmp(nt))
       time=modify_time(nt,time_tmp,time_units,start_yymmdd,start_hhmmss)
       it2=it1+nt-1
       call get_variable(trim(fnames(ifile)),varname,(/1,1,1/),(/nx+2,ny+2,nt/),v_3d)
       call calc_doy_from_units(nt, time_tmp, time_units, time_cyc_tmp)
       wgrd%time%val(it1:it2)=time(1:nt)
       wgrd%time_cyc%val(it1:it2) = time_cyc_tmp(1:nt) 
       wgrd%data%val(0:nx+1,0:ny+1,it1:it2)=v_3d(1:nx+2,1:ny+2,1:nt)
       it1=it2+1
       deallocate(time)
       deallocate(time_cyc_tmp)
    end do
  end subroutine read_data_TLL_p
  subroutine get_data_TLL_p(time_now,start_yymmdd,start_hhmmss,nx,ny,wgrd)
    implicit none
    real(idx),intent(in) :: time_now
    integer, intent(in) :: start_yymmdd, start_hhmmss,nx,ny
    type(TLL_dta),intent(inout) :: wgrd
    real(idx) :: time_int,time_cyc_now
    real(idx) :: tmp1
    integer :: ix,iy
   if (wgrd%Lcycle == 'T') then
      call get_time_cyc_now(time_now, start_yymmdd, start_hhmmss, time_cyc_now)
      call time_wgt_cyclic(wgrd%time_cyc%val, time_cyc_now, wgrd%Tcycle, &
                           wgrd%ind1, wgrd%ind2, wgrd%wgt1, wgrd%wgt2)
   else
      call time_wgt(wgrd%time%val, time_now, wgrd%ind1, wgrd%ind2, wgrd%wgt1, wgrd%wgt2)
   end if
    do iy = 0,ny+1
       do ix = 0,nx+1
          tmp1=set_data(wgrd%ind1,wgrd%ind2,wgrd%wgt1,wgrd%wgt2,wgrd%data%val(ix,iy,:))
          wgrd%data_now%val(ix,iy)=tmp1
       end do
    end do
  end subroutine get_data_TLL_p
  subroutine read_data_TLLL_p(nfile,fnames,timename,varname,nx,ny,nm,wgrd,start_yymmdd,start_hhmmss)
    implicit none
    integer,intent(in) :: nfile
    character(len=maxlen),intent(in) :: fnames(nfile),timename,varname
    integer,intent(in) :: nx,ny,nm,start_yymmdd,start_hhmmss
    type(TLLL_dta),intent(inout) :: wgrd
    integer :: nt,ntime
    real(idx),allocatable :: time_tmp(:),time_cyc_tmp(:),time(:),time_out(:),v_4d(:,:,:,:)
    character(len=maxlen) :: time_units
    integer :: ifile,it1,it2
    ntime=0
    do ifile=1,nfile
       call get_dimsize(fnames(ifile),timename,nt)
       ntime=ntime+nt
    end do

    it1=1;it2=0
    allocate(wgrd%time%val(1:ntime))
    allocate(wgrd%time_cyc%val(1:ntime)) 
    allocate(wgrd%data%val(0:nx+1,0:ny+1,1:nm,1:ntime))
    allocate(wgrd%data_now%val(0:nx+1,0:ny+1,1:nm))
    allocate(wgrd%data_mod%val(0:nx+1,0:ny+1,1:nm))

    do ifile=1,nfile
       call get_dimsize(trim(fnames(ifile)),timename,nt)
       call get_variable(trim(fnames(ifile)),timename,(/1/),(/nt/),time_tmp)
       call get_attribute(trim(fnames(ifile)),timename,"units",time_units)
       allocate(time(nt))
       allocate(time_cyc_tmp(nt))
       time=modify_time(nt,time_tmp,time_units,start_yymmdd,start_hhmmss)
       it2=it1+nt-1
       call get_variable(trim(fnames(ifile)),varname,(/1,1,1,1/),(/nx+2,ny+2,nm,nt/),v_4d)
       call calc_doy_from_units(nt, time_tmp, time_units, time_cyc_tmp)
       wgrd%time%val(it1:it2)=time(1:nt)
       wgrd%time_cyc%val(it1:it2) = time_cyc_tmp(1:nt) 
       wgrd%data%val(0:nx+1,0:ny+1,1:nm,it1:it2)=v_4d(1:nx+2,1:ny+2,1:nm,1:nt)
       it1=it2+1
       deallocate(time)
       deallocate(time_cyc_tmp)
    end do
  end subroutine read_data_TLLL_p
  subroutine get_data_TLLL_p(time_now,start_yymmdd,start_hhmmss,nx,ny,nm,wgrd)
    implicit none
    real(idx),intent(in) :: time_now
    integer, intent(in) :: start_yymmdd, start_hhmmss,nx,ny,nm
    type(TLLL_dta),intent(inout) :: wgrd
    real(idx) :: time_int,time_cyc_now
    real(idx) :: tmp1
    integer :: ix,iy,im
   if (wgrd%Lcycle == 'T') then
      call get_time_cyc_now(time_now, start_yymmdd, start_hhmmss, time_cyc_now)
      call time_wgt_cyclic(wgrd%time_cyc%val, time_cyc_now, wgrd%Tcycle, &
                           wgrd%ind1, wgrd%ind2, wgrd%wgt1, wgrd%wgt2)
   else
      call time_wgt(wgrd%time%val, time_now, wgrd%ind1, wgrd%ind2, wgrd%wgt1, wgrd%wgt2)
   end if
   do im =1,nm
    do iy = 0,ny+1
       do ix = 0,nx+1
          tmp1=set_data(wgrd%ind1,wgrd%ind2,wgrd%wgt1,wgrd%wgt2,wgrd%data%val(ix,iy,im,:))
          wgrd%data_now%val(ix,iy,im)=tmp1
       end do
    end do
   end do
  end subroutine get_data_TLLL_p
end module mod_io_master

