# Python script for construct LOM grid from ROMS grid
import netCDF4 as ncdf
import numpy as np

gname="indian"
gname="eqpac_30"
fname_grd="grid_"+gname+".nc"
fname_mask="mask_"+gname+"_rev.nc"

nc_grd=ncdf.Dataset(fname_grd,"r+")
nc_mask=ncdf.Dataset(fname_mask,"r")
mask_p=nc_mask.variables["mask"][:,:]
nc_grd.variables["mask_p"][:,:]=mask_p
nc_grd.close()
nc_mask.close()
