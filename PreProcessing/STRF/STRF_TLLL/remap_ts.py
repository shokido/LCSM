import subprocess
import netCDF4 as ncdf
import numpy as np

# Python code to do remapping

grd_name='indian'
fname_gdes='../GRID/gdes_grid_'+grd_name+'.dat'
fnames_in=["../DATA/ORAS4_temp_ann.nc","../DATA/ORAS4_salt_ann.nc"]
varnames_in=["TO","SO"]
fnames_out=["ORAS4_temp_ann_"+grd_name+".nc","ORAS4_salt_ann_"+grd_name+".nc"]
nfile=len(fnames_in)

for i in range(0,nfile):
    command='cdo remapbil,'+fname_gdes+' '+fnames_in[i]+' '+fnames_out[i]
    subprocess.call(command.split())
