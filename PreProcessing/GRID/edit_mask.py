# Python script for construct LOM grid from ROMS grid
import netCDF4 as ncdf
import numpy as np
import subprocess

grd_name="pacific05"
fname_mask="mask_"+grd_name+".nc"
fname_mask_new="mask_"+grd_name+"_rev.nc"


command='cp '+fname_mask+' '+fname_mask_new
subprocess.call(command.split())
nc_mask=ncdf.Dataset(fname_mask_new,"r+")
mask_p=nc_mask.variables["mask_p"][:]
# Manual edition
# West
mask_p[96:,0]=0
#mask_p[47:74,0:11]=0
mask_p[11:45,0:41]=0
# East
mask_p[86:,285:]=0
mask_p[83:,293:]=0
mask_p[71:,313:]=0
mask_p[68:71,315:332]=0
nc_mask.variables["mask_p"][:,:]=mask_p
nc_mask.close()
