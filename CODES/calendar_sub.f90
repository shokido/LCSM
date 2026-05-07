module calendar_sub
  ! Module for treat date and time
  implicit none
  private
  interface calendar_cal_ymdhms_after
     module procedure :: calendar_cal_ymdhms_after,dcalendar_cal_ymdhms_after
  end interface calendar_cal_ymdhms_after
  interface calendar_cal_day_length_ymdhms
     module procedure :: calendar_cal_day_length_ymdhms,dcalendar_cal_day_length_ymdhms
  end interface calendar_cal_day_length_ymdhms
  interface calendar_cal_length_ymdhms
     module procedure :: calendar_cal_length_ymdhms,dcalendar_cal_length_ymdhms
  end interface calendar_cal_length_ymdhms

!  public :: calendar_sub
  public :: isleap,dayofmonth,hhmmss_to_day,day_to_hhmmss
  public :: jd_to_yymmdd,yymmdd_to_jd
  public :: get_reftime
  public :: calendar_create_time_att
  public :: calendar_cal_ymdhms_after
  public :: calendar_cal_day_length_ymdhms
  public :: calendar_cal_length_ymdhms
contains
  ! Check whether leap-year or not
  function isleap(year) result(ret)
    implicit none
    integer,intent(in) :: year
    logical :: ret
    ret = (mod(year,4) == 0 .and. .not. mod(year,100) == 0)&
          .or. (mod(year,400) == 0)
  end function isleap
  ! Get number of days for specific year and month
  function dayofmonth(month,year) result(day)
    implicit none
    integer,intent(in) :: month,year
    integer :: day
    integer :: nmonth(12),nmonth_leap(12)
    logical :: leap_flag
    nmonth=(/31,28,31,30,31,30,31,31,30,31,30,31/)
    nmonth_leap=(/31,29,31,30,31,30,31,31,30,31,30,31/)
    leap_flag=isleap(year)
    if (leap_flag .eqv. .True.) then
       day=nmonth_leap(month)
    else
       day=nmonth(month)
    end if
  end function dayofmonth
  function hhmmss_to_day(hhmmss) result(day)
    implicit none
    integer,parameter :: idx = 4
    integer,intent(in) :: hhmmss
    real(idx) :: day
    integer :: hh,mm,ss
    hh=hhmmss/10000
    mm=hhmmss/100-hh*100
    ss=hhmmss-hh*10000-mm*100
    day=hh/24.0_idx + mm / (24.0_idx*60.0_idx) + ss / (24.0_idx*60.0_idx*60.0_idx)
  end function hhmmss_to_day
  function dhhmmss_to_day(hhmmss) result(day)
    implicit none
    integer,parameter :: idx = 8
    integer,intent(in) :: hhmmss
    real(idx) :: day
    integer :: hh,mm,ss
    hh=hhmmss/10000
    mm=hhmmss/100-hh*100
    ss=hhmmss-hh*10000-mm*100
    day=hh/24.0_idx + mm / (24.0_idx*60.0_idx) + ss / (24.0_idx*60.0_idx*60.0_idx)
  end function dhhmmss_to_day
  function day_to_hhmmss(day) result(hhmmss)
    implicit none
    integer,parameter :: idx = 4
    real(idx) :: day
    integer :: hhmmss
    integer :: hh,mm,ss
    hh=int(24.0_idx*day)
    mm=int((24.0_idx*day-hh)*60.0_idx)
    ss=int(60.0_idx*((24.0_idx*day-hh)*60.0_idx-mm))
    hhmmss=10000*hh+100*mm+ss
  end function day_to_hhmmss
  function dday_to_hhmmss(day) result(hhmmss)
    implicit none
    integer,parameter :: idx = 8
    real(idx) :: day
    integer :: hhmmss
    integer :: hh,mm,ss
    hh=int(24.0_idx*day)
    mm=int((24.0_idx*day-hh)*60.0_idx)
    ss=int(60.0_idx*((24.0_idx*day-hh)*60.0_idx-mm))
    hhmmss=10000*hh+100*mm+ss
  end function dday_to_hhmmss  
  function yymmdd_to_jd(yymmdd) result(jd)
    implicit none
    integer,intent(in) :: yymmdd
    integer :: year,month,day
    integer :: a,y,m
    integer :: jd
    year = yymmdd / 10000
    month = yymmdd / 100 - year*100
    day = yymmdd - year * 10000 - month * 100
    a = int((14-month)/12)
    y = year + 4800 - a
    m = month + 12 * a - 3
    ! 12:00's JDN
    jd = day + int((153*m+2)/5) + 365 * y + int(y/4) - int(y/100) + int(y/400)-32045
  end function yymmdd_to_jd
  function jd_to_yymmdd(jd) result(yymmdd)
    implicit none
    integer,intent(in) :: jd
    integer:: year,month,day
    integer :: yymmdd
    integer :: f,e,g,h
    f = jd + 1401 + (((4 * jd + 274277) / 146097) * 3) / 4 - 38
    e = 4 * f + 3
    g = (mod(e,1461)/4)
    h = 5 * g + 2
    day = (mod(h,153)/5)+1
    month = mod((h/153)+2,12)+1
    year = (e/1461) - 4716 + (12 + 2 - month) / 12
    yymmdd = year*10000+month*100+day
  end function jd_to_yymmdd
  subroutine calendar_cal_ymdhms_after(start_yymmdd,start_hhmmss,time,flag,end_yymmdd,end_hhmmss)
    implicit none
    integer,parameter :: idx =4
    integer,intent(in) :: start_yymmdd,start_hhmmss
    integer,intent(in) :: flag
    real(idx),intent(in) :: time
    integer,intent(inout) :: end_yymmdd,end_hhmmss
    integer :: start_jd
    real(idx) :: start_tmp,end_jd
    integer :: start_year,start_month,start_day
    integer :: end_year,end_month,end_day
    real(idx) :: tmp
    select case (flag)
    case(-10000) ! Unit = second
       start_jd = yymmdd_to_jd(start_yymmdd)
       start_tmp= hhmmss_to_day(start_hhmmss)
       end_jd=start_jd*1.0_idx+start_tmp+time/(24.0_idx*60.0_idx*60.0_idx)
       end_yymmdd=jd_to_yymmdd(int(end_jd))
       tmp=end_jd-int(end_jd)
       end_hhmmss=day_to_hhmmss(tmp)
    case(-100) ! Unit = minute
       start_jd = yymmdd_to_jd(start_yymmdd)
       start_tmp= hhmmss_to_day(start_hhmmss)
       end_jd=start_jd*1.0_idx+start_tmp+time/(24.0_idx*60.0_idx)
       end_yymmdd=jd_to_yymmdd(int(end_jd))
       tmp=end_jd-int(end_jd)
       end_hhmmss=day_to_hhmmss(tmp)
    case(-1) ! Unit = hour
       start_jd = yymmdd_to_jd(start_yymmdd)
       start_tmp= hhmmss_to_day(start_hhmmss)
       end_jd=start_jd*1.0_idx+start_tmp+time/(24.0_idx)
       end_yymmdd=jd_to_yymmdd(int(end_jd))
       tmp=end_jd-int(end_jd)
       end_hhmmss=day_to_hhmmss(tmp)
    case (1) ! Unit = day
       start_jd = yymmdd_to_jd(start_yymmdd)
       start_tmp= hhmmss_to_day(start_hhmmss)
       end_jd=start_jd*1.0_idx+start_tmp+time
       end_yymmdd=jd_to_yymmdd(int(end_jd))
       tmp=end_jd-int(end_jd)
       end_hhmmss=day_to_hhmmss(tmp)
    case (100) ! Unit = month
       start_jd = yymmdd_to_jd(start_yymmdd)
       start_tmp= hhmmss_to_day(start_hhmmss)
       start_year = start_yymmdd / 10000
       start_month = start_yymmdd / 100 - start_year*100
       start_day=start_yymmdd-start_year*10000-start_month*100
       end_month=start_month+int(time)
       if (end_month .gt. 12) then
          end_year = start_year+int(end_month/12)
          end_month=end_month-int(end_month/12)*12
       else
          end_year = start_year
       end if
       tmp = (time-int(time))*dayofmonth(end_month,end_year)
       end_jd = yymmdd_to_jd(end_year*10000+end_month*100+start_day)+tmp+start_tmp
       end_yymmdd=jd_to_yymmdd(int(end_jd))
       tmp=end_jd-int(end_jd)
       end_hhmmss=day_to_hhmmss(tmp)
    case(10000) ! Unit = year
       start_jd = yymmdd_to_jd(start_yymmdd)
       start_tmp= hhmmss_to_day(start_hhmmss)
       start_year = start_yymmdd / 10000
       start_month = start_yymmdd / 100 - start_year*100
       start_day=start_yymmdd-start_year*10000-start_month*100
       end_year=start_year+int(time)
       tmp = (time-int(time))*365.0_idx
       end_jd = yymmdd_to_jd(end_year*10000+start_month*100+start_day)+tmp+start_tmp
       end_yymmdd=jd_to_yymmdd(int(end_jd))
       tmp=end_jd-int(end_jd)
       end_hhmmss=day_to_hhmmss(tmp)
    end select
  end subroutine calendar_cal_ymdhms_after
  subroutine dcalendar_cal_ymdhms_after(start_yymmdd,start_hhmmss,time,flag,end_yymmdd,end_hhmmss)
    implicit none
    integer,parameter :: idx =8
    integer,intent(in) :: start_yymmdd,start_hhmmss
    integer,intent(in) :: flag
    real(idx),intent(in) :: time
    integer,intent(inout) :: end_yymmdd,end_hhmmss
    integer :: start_jd
    real(idx) :: start_tmp,end_jd
    integer :: start_year,start_month,start_day
    integer :: end_year,end_month,end_day
    real(idx) :: tmp
    !hhmmss_to_day(start_hhmmss)
    select case (flag)
    case(-10000) ! Unit = second
       start_jd = yymmdd_to_jd(start_yymmdd)
       start_tmp= dhhmmss_to_day(start_hhmmss)
       end_jd=start_jd*1.0_idx+start_tmp+time/(24.0_idx*60.0_idx*60.0_idx)
       end_yymmdd=jd_to_yymmdd(int(end_jd))
       tmp=end_jd-int(end_jd)
       end_hhmmss=dday_to_hhmmss(tmp)
    case(-100) ! Unit = minute
       start_jd = yymmdd_to_jd(start_yymmdd)
       start_tmp= dhhmmss_to_day(start_hhmmss)
       end_jd=start_jd*1.0_idx+start_tmp+time/(24.0_idx*60.0_idx)
       end_yymmdd=jd_to_yymmdd(int(end_jd))
       tmp=end_jd-int(end_jd)
       end_hhmmss=dday_to_hhmmss(tmp)
    case(-1) ! Unit = hour
       start_jd = yymmdd_to_jd(start_yymmdd)
       start_tmp= dhhmmss_to_day(start_hhmmss)
       end_jd=start_jd*1.0_idx+start_tmp+time/(24.0_idx)
       end_yymmdd=jd_to_yymmdd(int(end_jd))
       tmp=end_jd-int(end_jd)
       end_hhmmss=dday_to_hhmmss(tmp)
    case (1) ! Unit = day
       start_jd = yymmdd_to_jd(start_yymmdd)
       start_tmp= dhhmmss_to_day(start_hhmmss)
       end_jd=start_jd*1.0_idx+start_tmp+time
       end_yymmdd=jd_to_yymmdd(int(end_jd))
       tmp=end_jd-int(end_jd)
       end_hhmmss=dday_to_hhmmss(tmp)
    case (100) ! Unit = month
       start_jd = yymmdd_to_jd(start_yymmdd)
       start_tmp= hhmmss_to_day(start_hhmmss)
       start_year = start_yymmdd / 10000
       start_month = start_yymmdd / 100 - start_year*100
       start_day=start_yymmdd-start_year*10000-start_month*100
       end_month=start_month+int(time)
       if (end_month .gt. 12) then
          end_year = start_year+int((end_month-1)/12)
          end_month=end_month-int((end_month-1)/12)*12
       else
          end_year = start_year
       end if
       tmp = (time-int(time))*dayofmonth(end_month,end_year)
       end_jd = yymmdd_to_jd(end_year*10000+end_month*100+start_day)+tmp+start_tmp
       end_yymmdd=jd_to_yymmdd(int(end_jd))
       tmp=end_jd-int(end_jd)
       end_hhmmss=dday_to_hhmmss(tmp)
    case(10000) ! Unit = year
       start_jd = yymmdd_to_jd(start_yymmdd)
       start_tmp= dhhmmss_to_day(start_hhmmss)
       start_year = start_yymmdd / 10000
       start_month = start_yymmdd / 100 - start_year*100
       start_day=start_yymmdd-start_year*10000-start_month*100
       end_year=start_year+int(time)
       tmp = (time-int(time))*365.0_idx
       end_jd = yymmdd_to_jd(end_year*10000+start_month*100+start_day)+tmp+start_tmp
       end_yymmdd=jd_to_yymmdd(int(end_jd))
       tmp=end_jd-int(end_jd)
       end_hhmmss=dday_to_hhmmss(tmp)
    end select
  end subroutine dcalendar_cal_ymdhms_after
  ! Not include first day
  subroutine calendar_cal_day_length_ymdhms(start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss,days)
    implicit none
    integer,parameter :: idx = 4
    integer,intent(in) :: start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss
    real(idx),intent(inout) :: days
    integer :: start_jd,end_jd
    real(idx) :: start_tmp,end_tmp
    start_jd = yymmdd_to_jd(start_yymmdd)
    start_tmp=hhmmss_to_day(start_hhmmss)
    end_jd = yymmdd_to_jd(end_yymmdd)
    end_tmp=hhmmss_to_day(end_hhmmss)
    days=(end_jd - start_jd)*1.0_idx + end_tmp-start_tmp
  end subroutine calendar_cal_day_length_ymdhms
  subroutine dcalendar_cal_day_length_ymdhms(start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss,days)
    implicit none
    integer,parameter :: idx = 8
    integer,intent(in) :: start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss
    real(idx),intent(inout) :: days
    integer :: start_jd,end_jd
    real(idx) :: start_tmp,end_tmp
    start_jd = yymmdd_to_jd(start_yymmdd)
    start_tmp=dhhmmss_to_day(start_hhmmss)
    end_jd = yymmdd_to_jd(end_yymmdd)
    end_tmp=dhhmmss_to_day(end_hhmmss)
    days=(end_jd - start_jd)*1.0_idx + end_tmp-start_tmp
  end subroutine dcalendar_cal_day_length_ymdhms
  ! Not include initial date
  subroutine calendar_cal_length_ymdhms(start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss,flag,time)
    implicit none
    integer,parameter :: idx = 4
    integer,intent(in) :: start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss
    real(idx),intent(inout) :: time
    real(idx) :: tmp
    integer :: flag
    integer :: start_year,end_year,start_month,end_month,start_day,end_day
    select case (flag)
    case(-10000) ! Unit = seconds
       tmp=0.0_idx
       call calendar_cal_day_length_ymdhms(start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss,tmp)
       time=tmp*24.0_idx*60.0_idx*60.0_idx
    case(-100) ! Unit = minute
       tmp=0.0_idx
       call calendar_cal_day_length_ymdhms(start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss,tmp)
       time=tmp*24.0_idx*60.0_idx
    case(-1) ! Unit = hour
       tmp=0.0_idx
       call calendar_cal_day_length_ymdhms(start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss,tmp)
       time=tmp*24.0_idx
    case (1) ! Unit = day
       tmp=0.0_idx
       call calendar_cal_day_length_ymdhms(start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss,tmp)
       time=tmp
    case (100) ! Unit = month
       start_year = start_yymmdd / 10000
       start_month = start_yymmdd / 100 - start_year*100
       start_day= start_yymmdd - start_year*10000 -start_month*100
       end_year = end_yymmdd / 10000
       end_month = end_yymmdd / 100 - end_year*100
       end_day= end_yymmdd - end_year*10000 -end_month*100
       call calendar_cal_day_length_ymdhms(end_year*10000+end_month*100+start_day,start_hhmmss,end_yymmdd,end_hhmmss,tmp)
       time=(end_year - start_year) * 12+(end_month-start_month)+tmp/(30.0_idx)
    case(10000) ! Unit = year
       start_year = start_yymmdd / 10000
       start_month = start_yymmdd / 100 - start_year*100
       start_day= start_yymmdd - start_year*10000 -start_month*100
       end_year = end_yymmdd / 10000
       end_month = end_yymmdd / 100 - end_year*100
       end_day= end_yymmdd - end_year*10000 -end_month*100
       call calendar_cal_day_length_ymdhms(end_year*10000+start_month*100+start_day,start_hhmmss,end_yymmdd,end_hhmmss,tmp)
       time=end_year - start_year+tmp/(365.0_idx)
    end select
  end subroutine calendar_cal_length_ymdhms
  subroutine dcalendar_cal_length_ymdhms(start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss,flag,time)
    implicit none
    integer,parameter :: idx = 8
    integer,intent(in) :: start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss
    real(idx),intent(inout) :: time
    real(idx) :: tmp
    integer :: flag
    integer :: start_year,end_year,start_month,end_month,start_day,end_day
    select case (flag)
    case(-10000) ! Unit = seconds
       tmp=0.0_idx
       call dcalendar_cal_day_length_ymdhms(start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss,tmp)
       time=tmp*24.0_idx*60.0_idx*60.0_idx
    case(-100) ! Unit = minute
       tmp=0.0_idx
       call dcalendar_cal_day_length_ymdhms(start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss,tmp)
       time=tmp*24.0_idx*60.0_idx
    case(-1) ! Unit = hour
       tmp=0.0_idx
       call dcalendar_cal_day_length_ymdhms(start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss,tmp)
       time=tmp*24.0_idx
    case (1) ! Unit = day
       tmp=0.0_idx
       call dcalendar_cal_day_length_ymdhms(start_yymmdd,start_hhmmss,end_yymmdd,end_hhmmss,tmp)
       time=tmp
    case (100) ! Unit = month
       start_year = start_yymmdd / 10000
       start_month = start_yymmdd / 100 - start_year*100
       start_day= start_yymmdd - start_year*10000 -start_month*100
       end_year = end_yymmdd / 10000
       end_month = end_yymmdd / 100 - end_year*100
       end_day= end_yymmdd - end_year*10000 -end_month*100
       call dcalendar_cal_day_length_ymdhms(end_year*10000+end_month*100+start_day,start_hhmmss,end_yymmdd,end_hhmmss,tmp)
       time=(end_year - start_year) * 12+(end_month-start_month)+tmp/(30.0_idx)
    case(10000) ! Unit = year
       start_year = start_yymmdd / 10000
       start_month = start_yymmdd / 100 - start_year*100
       start_day= start_yymmdd - start_year*10000 -start_month*100
       end_year = end_yymmdd / 10000
       end_month = end_yymmdd / 100 - end_year*100
       end_day= end_yymmdd - end_year*10000 -end_month*100
       call dcalendar_cal_day_length_ymdhms(end_year*10000+start_month*100+start_day,start_hhmmss,end_yymmdd,end_hhmmss,tmp)
       time=end_year - start_year+tmp/(365.0_idx)
    end select
  end subroutine dcalendar_cal_length_ymdhms
  subroutine get_reftime(units,stamp_ind,ref_yymmdd,ref_hhmmss)
    implicit none
    character(len=*),intent(in) :: units
    integer,intent(inout) :: ref_yymmdd,ref_hhmmss
    integer :: stamp_ind
    integer :: ind1,ind2
    character :: yr_char*4,mn_char*2,dy_char*2,hr_char*2,min_char*2,sec_char*2
    integer :: yr_ind,mn_ind,dy_ind
    integer :: hr_ind,min_ind,sec_ind
    ! hours case
    if (index(units,"seconds") .ne. 0 .or. index(units,"Seconds") .ne. 0) then
       stamp_ind = -10000     ! second case
    else if (index(units,"minutes") .ne. 0 .or. index(units,"Minutes") .ne. 0) then
       stamp_ind = -100     ! minute case
    else if (index(units,"hours") .ne. 0 .or. index(units,"Hours") .ne. 0) then
       stamp_ind = -1     ! hour case
    else if (index(units,"days") .ne. 0 .or. index(units,"Days") .ne. 0) then
       stamp_ind = 1     ! day case
    else if (index(units,"months") .ne. 0 .or. index(units,"Months") .ne. 0) then
       stamp_ind = 100     ! month case
    else if (index(units,"years") .ne. 0 .or. index(units,"Years") .ne. 0) then
       stamp_ind = 10000     ! year case
    else
       stamp_ind = 1
    end if
    ind1=index(units,"since")+6
    ind2=ind1+index(units(ind1:),"-")-1
    yr_char=units(ind1:ind2-1)
    ! Month
    ind1=ind2+1
    ind2=ind1+index(units(ind1:),"-")-1
    mn_char=units(ind1:ind2-1)
    ! Day
    ind1=ind2+1
    ind2=ind1+index(units(ind1:)," ")-1
    dy_char=units(ind1:ind2-1)
    ! Hour
    ind1=ind2+2
    ind2=ind1+index(units(ind1:),":")-1
    hr_char=units(ind1:ind2-1)
    ! Minute
    ind1=ind2+1
    ind2=ind1+index(units(ind1:),":")-1
    min_char=units(ind1:ind2-1)
    ! Second
    ind1=ind2+1
    ind2=ind1+2
    sec_char=units(ind1:ind2-1)
    read(yr_char,*) yr_ind
    read(mn_char,*) mn_ind
    read(dy_char,*) dy_ind
    ref_yymmdd=yr_ind*10000+mn_ind*100+dy_ind
    read(hr_char,*) hr_ind
    read(min_char,*) min_ind
    read(sec_char,*) sec_ind
    ref_hhmmss=hr_ind*10000+min_ind*100+sec_ind
  end subroutine get_reftime

  function calendar_create_time_att(start_yymmdd,start_hhmmss,int_yymmdd) result(time_unit)
    implicit none
    integer,intent(in) ::start_yymmdd,start_hhmmss,int_yymmdd
    integer :: start_year,start_month,start_day
    character :: year_ind*4,month_ind*2,day_ind*2
    integer :: start_hour,start_min,start_sec
    character :: hour_ind*2,min_ind*2,sec_ind*2
    character :: ref_unit*8
    character :: time_unit*100
    start_year=start_yymmdd/10000
    start_month=start_yymmdd/100-start_year*100
    start_day=start_yymmdd-start_year*10000-start_month*100
    write(year_ind,'(i4.4)') start_year
    write(month_ind,'(i2.2)') start_month
    write(day_ind,'(i2.2)') start_day
    start_hour=start_hhmmss/10000
    start_min=start_hhmmss/100-start_hour*100
    start_sec=start_hhmmss-start_hour*10000-start_min*100
    write(hour_ind,'(i2.2)') start_hour
    write(min_ind,'(i2.2)') start_min
    write(sec_ind,'(i2.2)') start_sec
    ! ref unit
    if (int_yymmdd .ge. 10000) then
       ref_unit="years"
    else if (int_yymmdd .ge. 100) then
       ref_unit="months"
    else if (int_yymmdd .ge. 1) then
       ref_unit="days"
    else if (int_yymmdd .ge. -1) then
       ref_unit="hours"
    else if (int_yymmdd .ge. -100) then
       ref_unit="minutes"
    else
       ref_unit="seconds"
    end if
    time_unit=trim(ref_unit)//" since "//year_ind//"-"//month_ind//"-"//day_ind//" "&
    & //hour_ind//":"//min_ind//":"//sec_ind
  end function calendar_create_time_att
end module calendar_sub
