import numpy as np
import netCDF4 as ncdf
import sys
factor=1.0/(60.0*60.0*24.0)

grid_name="indian"
fname_grid="grid_"+grid_name+".nc"
nc1=ncdf.Dataset(fname_grid,"r+")
y_u=nc1.variables["y_u"][:]
damp_u=nc1.variables["damp_u"]

#print(y_u)
ny=len(y_u)
ndim=np.shape(damp_u)
nx=ndim[1]
damp_func=y_u.copy()
damp_func[:]=0

# Southern boundary
y1=150.0*1000.0
y2=300.0*1000.0
damp_func[y_u<=y1]=1.0
a=y_u[(y_u<=y2) & (y_u>=y1)]
damp_func[(y_u<=y2) & (y_u>=y1)]=(y2-a)/(y1)

# # Meridional boundary
# damp_func[y_u<=150.0*1000.0]=1.0
# a=y_u[(y_u<=300.0*1000.0) & (y_u>=150.0*1000.0)]
# damp_func[(y_u<=300.0*1000.0) & (y_u>=150.0*1000.0)]=(300.0*1000.0-a)/(150.0*1000.0)
# y_max=np.max(y_u)
# damp_func[y_u>=y_max-150.0*1000.0]=1.0
# a=y_max-y_u[(y_u>=y_max-300.0*1000.0) & (y_u<=y_max-150.0*1000.0)]
# damp_func[(y_u>=y_max-300.0*1000.0) & (y_u<=y_max-150.0*1000.0)]=(300.0*1000.0-a)/(150.0*1000.0)

for ix in range(0,nx):
    damp_u[0:ny,ix]=damp_func[0:ny]*factor
nc1.close()
