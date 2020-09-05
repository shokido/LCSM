import numpy as np
import netCDF4 as ncdf
fname_in='ORAS4_strf_ann_indian_30.nc'
fname_out='cn_ORAS4_ann_1d_30.nc'

nc_in=ncdf.Dataset(fname_in,'r')
cn=nc_in.variables['cn'][:]
mode=nc_in.variables['mode'][:]
# Timemean
cn_mean=np.mean(cn,axis=0)
# FLDMEAN
cn_mean=np.mean(np.mean(cn_mean,axis=1),axis=1)
nc_in.close()

nc_out=ncdf.Dataset(fname_out,'w')
nc_out.createDimension('mode',len(mode))
nc_out.createVariable('mode',mode.dtype,('mode'))
nc_out.createVariable('cn',mode.dtype,('mode'))
nc_out.variables['mode'][:]=mode[:]
nc_out.variables['mode'].units=''
nc_out.variables['cn'][:]=cn_mean[:]
nc_out.variables['cn'].units='m/s'
nc_out.variables['cn'].units='Phase speed of vertical modes'
nc_out.original_file=fname_in
nc_out.close()

