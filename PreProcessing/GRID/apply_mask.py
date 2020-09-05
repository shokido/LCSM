# Python script for construct LOM grid from ROMS grid
import netCDF4 as ncdf
import numpy as np

grd_name="pacific05"
fname_grd="grid_"+grd_name+".nc"
fname_mask="mask_"+grd_name+"_rev.nc"

nc_grd=ncdf.Dataset(fname_grd,"r+")
nc_mask=ncdf.Dataset(fname_mask,"r")
mask_p=nc_mask.variables["mask_p"][:,:]
nc_grd.variables["mask_p"][:,:]=mask_p
nc_grd.close()
nc_mask.close()
