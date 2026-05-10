import numpy as np
import netCDF4 as ncdf
import datetime as dt
gname='eqpac_30'
fname_grid='../GRID/grid_'+gname+'.nc'
fname_wind='wstress_eqpatch_'+gname+'.nc'

nc_in=ncdf.Dataset(fname_grid,'r')
mask=nc_in.variables["mask_p"][:]
lon=nc_in.variables["lon_p"][:]
lat=nc_in.variables["lat_p"][:]
nc_in.close()


ref_dt=dt.datetime(1900,1,1)
dt1=dt.datetime(1900,1,1)
dt2=dt.datetime(1900,1,30)
ntime=(dt2-dt1).days
time_in=[]
uw_out=np.zeros((ntime,len(lat),len(lon)))
vw_out=np.zeros((ntime,len(lat),len(lon)))
for it in range(0,ntime):
    dt_now=dt1+dt.timedelta(days=it)
    time_in.append((dt_now-ref_dt).days)
    for ilat in range(0,len(lat)):
        for ilon in range(0,len(lon)):
            uw_out[it,ilat,ilon]=0.2*np.exp(-1.0*((lat[ilat]-0)/10.0)**2)*np.exp(-1.0*((lon[ilon]-180)/30.0)**2)*np.exp(-1.0*((dt_now-dt1).days/10.0)**2)
time_in=np.asarray(time_in)
nc_mask=ncdf.Dataset(fname_wind,'w')
nc_mask.createDimension('lon',len(lon[:]))
nc_mask.createDimension('lat',len(lat[:]))
nc_mask.createDimension('time',ntime)
nc_mask.createVariable('lon',lon.dtype,('lon'))
nc_mask.createVariable('lat',lat.dtype,('lat'))
nc_mask.createVariable('time',time_in.dtype,('time'))
nc_mask.createVariable('uw',mask.dtype,('time','lat','lon'))
nc_mask.createVariable('vw',mask.dtype,('time','lat','lon'))
nc_mask['lon'].units="degrees_east"
nc_mask['lat'].units="degrees_north"
nc_mask['time'].units="days since "+str(ref_dt)
nc_mask['uw'].long_name="Zonal wind stress"
nc_mask['vw'].long_name="Meridional wind stress"
nc_mask['uw'].units="N/m^2"
nc_mask['vw'].units="N/m^2"
nc_mask['lon'][:]=lon[:]
nc_mask['lat'][:]=lat[:]
nc_mask['time'][:]=time_in[:]
nc_mask['uw'][:,:,:]=uw_out
nc_mask['vw'][:,:,:]=vw_out
nc_mask.close()


