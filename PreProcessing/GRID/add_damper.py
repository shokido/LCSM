import numpy as np
import netCDF4 as ncdf
import sys
factor=1.0#/(60.0*60.0*24.0)
v1=1.0
v2=10.0
thres=300.0*1000

grd_name="eqpac_30"
fname_grid="grid_"+grd_name+".nc"
nc1=ncdf.Dataset(fname_grid,"r+")
lon_p=nc1.variables["lon_p"][:]
y_u=nc1.variables["y_u_2d"][:,:]
y_v=nc1.variables["y_v_2d"][:,:]
damp_u=np.copy(nc1.variables["damp_u"][:,:])
damp_v=np.copy(nc1.variables["damp_v"][:,:])

y_u=y_u[:,0];y_v=y_v[:,0]
ny_u=len(y_u)
ny_v=len(y_v)
ndim_u=np.shape(damp_u);nx_u=ndim_u[1]
ndim_v=np.shape(damp_v);nx_v=ndim_v[1]
damp_func_u=y_u.copy();damp_func_u[:]=0
damp_func_v=y_v.copy();damp_func_v[:]=0

# Northen boundary
ymax=np.max(y_u)
y2=ymax;y1=ymax-thres
a=y_u[(y_u<=y2) & (y_u>=y1)]
damp_func_u[:]=v1
damp_func_v[:]=v1
damp_func_u[(y_u<=y2) & (y_u>=y1)]=v1+(v2-v1)*(a-y1)/(y2-y1)
ymax=np.max(y_v)
y2=ymax;y1=ymax-thres
a=y_v[(y_v<=y2) & (y_v>=y1)]
damp_func_v[(y_v<=y2) & (y_v>=y1)]=v1+(v2-v1)*(a-y1)/(y2-y1)

# Southern boundary
y2=thres;y1=np.min(y_u)
a=y_u[(y_u<=y2) & (y_u>=y1)]

damp_func_u[(y_u<=y2) & (y_u>=y1)]=v1+(v2-v1)*(y2-a)/(y2-y1)
y2=thres;y1=np.min(y_v)
a=y_v[(y_v<=y2) & (y_v>=y1)]
damp_func_v[(y_v<=y2) & (y_v>=y1)]=v1+(v2-v1)*(y2-a)/(y2-y1)

for ix in range(0,nx_u):
    damp_u[0:ny_u,ix]=damp_func_u[0:ny_u]*factor

for ix in range(0,nx_v):
    damp_v[0:ny_v,ix]=damp_func_v[0:ny_v]*factor

nc1.variables["damp_u"][:,:]=damp_u    
nc1.variables["damp_v"][:,:]=damp_v
nc1.close()
