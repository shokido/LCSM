import numpy as np
import netCDF4 as ncdf
import datetime as dt
gname='eqpac_30'
fname_grid='../../GRID/grid_'+gname+'.nc'
fname_strf='SW_strf_'+gname+'.nc'
H=130
cp=2.9 # m/s
gprime=cp*cp/H
r_day=730.0
r=1.0/(r_day*60*60*24)
A=r*cp*cp
print("A=",A)

time_ref=dt.datetime(1900,1,1)

nmode=1
modes=np.arange(1,nmode+1)
ntime=1
times=np.arange(0,1,ntime)
nc_in=ncdf.Dataset(fname_grid,'r')
mask=nc_in.variables["mask_p"][:]
lon=nc_in.variables["lon_p"][:]
lat=nc_in.variables["lat_p"][:]
nc_in.close()
ohn=np.ones((len(lat),len(lon)))
cn=np.ones((len(lat),len(lon)))*cp
ohn[:]=(1/H)*mask
nlev=2
lev=np.zeros(nlev)
print(modes)
nc_mask=ncdf.Dataset(fname_strf,'w')
nc_mask.createDimension('lon',len(lon[:]))
nc_mask.createDimension('lat',len(lat[:]))
nc_mask.createDimension('lev',len(lev[:]))
nc_mask.createDimension('mode',nmode)
nc_mask.createDimension('time',ntime)
nc_mask.createVariable('lon',lon.dtype,('lon'))
nc_mask.createVariable('lat',lat.dtype,('lat'))
nc_mask.createVariable('lev',lev.dtype,('lev'))
nc_mask.createVariable('mode',modes.dtype,('mode'))
nc_mask.createVariable('time',times.dtype,('time'))
nc_mask.createVariable('cn',mask.dtype,('time','mode','lat','lon'))
nc_mask.createVariable('ohn',mask.dtype,('time','mode','lat','lon'))
nc_mask.createVariable('obn',mask.dtype,('time','mode','lat','lon'))
nc_mask['lon'].units="degrees_east"
nc_mask['lat'].units="degrees_north"
nc_mask['mode'].units=""
nc_mask['time'].units="days since "+str(time_ref)
nc_mask['cn'].units="m/s"
nc_mask['ohn'].units="1/m"
nc_mask['lon'][:]=lon[:]
nc_mask['lat'][:]=lat[:]
nc_mask['time'][:]=times[:]
nc_mask['cn'][0,0,:,:]=cn[:]
nc_mask['ohn'][0,0,:,:]=ohn[:]
nc_mask['obn'][0,0,:,:]=ohn[:]
nc_mask.close()
