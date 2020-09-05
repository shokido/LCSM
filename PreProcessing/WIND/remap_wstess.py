import subprocess
import netCDF4 as ncdf
import numpy as np

# Python code for remapping wind data

grd_name='indian'
fname_gdes='../GRID/gdes_grid_'+grd_name+'.dat'
fnames_in=['/Volumes/Promise-Pegasus/kido/Reanalysis/Atmosphere/JRA55do/v1.3/Output/Monthly/jra55do_v1.3_uw_monthly_clm.nc','/Volumes/Promise-Pegasus/kido/Reanalysis/Atmosphere/JRA55do/v1.3/Output/Monthly/jra55do_v1.3_vw_monthly_clm.nc']
varnames_in=['uw','vw']
fnames_out=["jra55do_uw_clm_"+grd_name+".nc","jra55do_vw_clm_"+grd_name+".nc"]


nfile=len(fnames_in)

for i in range(0,nfile):
    command='cdo remapbil,'+fname_gdes+' -selvar,'+varnames_in[i]+' '+fnames_in[i]+' '+fnames_out[i]
    subprocess.call(command.split())
