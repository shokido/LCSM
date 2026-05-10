import subprocess
import netCDF4 as ncdf
import numpy as np
gname='indian'
gname="eqpac_30"
fname_gdes='../GRID/gdes_grid_'+gname+'.dat'
fname_topo='../DATA/etopo5.nc'
fname_grid='grid_'+gname+'.nc'
varname='topo'
lonname='lon'
latname='lat'
fname_mask='mask_'+gname+'.nc'
hthres=-100.0

# Remap topography
fname_tmp='topo_'+gname+'.nc'
command='cdo remapbil,'+fname_gdes+' '+fname_topo+' '+fname_tmp
subprocess.call(command.split())

lonname='lon_p'
latname='lat_p'
nc_in=ncdf.Dataset(fname_grid,'r')
mask=nc_in.variables["mask_p"][:]
lon=nc_in.variables[lonname][:]
lat=nc_in.variables[latname][:]
print(np.max(lon))
print(np.min(lon))
print(np.max(lat))
print(np.min(lat))
nc_in.close()

nc_topo=ncdf.Dataset(fname_tmp,'r')
topo=nc_topo.variables[varname][:]
index_topo=np.where((topo>hthres))
mask[index_topo[0],index_topo[1]]=0.0
nc_topo.close()

nc_mask=ncdf.Dataset(fname_mask,'w')
nc_mask.createDimension('lon',len(lon[:]))
nc_mask.createDimension('lat',len(lat[:]))
nc_mask.createVariable('lon',lon.dtype,('lon'))
nc_mask.createVariable('lat',lat.dtype,('lat'))
nc_mask.createVariable('topo',mask.dtype,('lat','lon'))
nc_mask.createVariable('mask',mask.dtype,('lat','lon'))
nc_mask['lon'][:]=lon[:]
nc_mask['lat'][:]=lat[:]
nc_mask['mask'][:]=mask[:]
nc_mask['topo'][:]=topo[:]
nc_mask.close()
command='rm '+fname_tmp
subprocess.call(command.split())

