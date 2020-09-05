import subprocess
import netCDF4 as ncdf
import numpy as np
grd_name='indian'

fname_gdes='../GRID/gdes_grid_'+grd_name+'.dat'
fname_topo='../DATA/etopo5.nc'
fname_grid='grid_'+grd_name+'.nc'
varname='topo'
lonname='lon'
latname='lat'
fname_mask='mask_'+grd_name+'.nc'
hthres=-100.0

# Remap topography
fname_tmp='topo_'+grd_name+'.nc'
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
nc_mask.createDimension(lonname,len(lon[:]))
nc_mask.createDimension(latname,len(lat[:]))
nc_mask.createVariable(lonname,lon.dtype,(lonname))
nc_mask.createVariable(latname,lat.dtype,(latname))
nc_mask.createVariable('topo',mask.dtype,(latname,lonname))
nc_mask.createVariable('mask_p',mask.dtype,(latname,lonname))
nc_mask[lonname][:]=lon[:]
nc_mask[latname][:]=lat[:]
nc_mask['mask_p'][:]=mask[:]
nc_mask['topo'][:]=topo[:]
nc_mask.close()
command='rm '+fname_tmp
subprocess.call(command.split())

