import numpy as np
import netCDF4 as ncdf
import datetime as dt

gname='eqpac_30'
fname_grid='../GRID/grid_'+gname+'.nc'
fname_strf='WOA18_strf_ann_'+gname+'_mode5.nc'
fname_out='WOA18_cn_ann_1d_'+gname+'_mode5.nc'


nc_in=ncdf.Dataset(fname_grid,'r')
mask=nc_in.variables["mask_p"][:]
lon=nc_in.variables["lon_p"][:]
lat=nc_in.variables["lat_p"][:]
nc_in.close()

nc_strf=ncdf.Dataset(fname_strf,'r')
modes=nc_strf.variables['mode'][:];nmode=len(modes)
cn=nc_strf.variables['cn'][:]
time_in=nc_strf.variables['time'][:]
time_units=nc_strf.variables['time'].units
# Timemean
cn_mean=np.nanmean(cn,axis=0)
# FLDMEAN
cn_mean=np.mean(np.mean(cn_mean,axis=1),axis=1)
print(cn_mean)
nc_strf.close()

ntime=1
time_in=time_in[0:ntime]
nc_mask=ncdf.Dataset(fname_out,'w')
nc_mask.createDimension('lon',len(lon[:]))
nc_mask.createDimension('lat',len(lat[:]))
nc_mask.createDimension('mode',nmode)
nc_mask.createDimension('time',ntime)
nc_mask.createVariable('lon',lon.dtype,('lon'))
nc_mask.createVariable('lat',lat.dtype,('lat'))
nc_mask.createVariable('mode',modes.dtype,('mode'))
nc_mask.createVariable('time',time_in.dtype,('time'))
nc_mask.createVariable('cn',mask.dtype,('time','mode','lat','lon'))
nc_mask['lon'].units="degrees_east"
nc_mask['lat'].units="degrees_north"
nc_mask['mode'].units=""
nc_mask['time'].units=time_units
nc_mask['cn'].units="m/s"
nc_mask['lon'][:]=lon[:]
nc_mask['lat'][:]=lat[:]
nc_mask['mode'][:]=modes[:]
nc_mask['time'][:]=time_in[:]
nc_mask['cn'][0,:,:,:]=cn_mean[:,np.newaxis,np.newaxis]#*mask
nc_mask.close()


