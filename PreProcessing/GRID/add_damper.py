import numpy as np
import netCDF4 as ncdf
import sys
factor=1.0/(60.0*60.0*24.0)

gname="eqpac_30"
fname_grid="grid_"+gname+".nc"
nc1=ncdf.Dataset(fname_grid,"r+")
y_u=nc1.variables["y_u"][:]
damp_u=nc1.variables["damp_u"]
damp_v=nc1.variables["damp_v"]
damp_p=nc1.variables["damp_p"]

#print(y_u)
ny=len(y_u)
ndim=np.shape(damp_u)
nx=ndim[1]
damp_func=y_u.copy()
damp_func[:]=0

# --- Southern boundary sponge layer ---
# sy1: Boundary of the fully damped region
# sy2: Point where damping starts to transition from 0 to 1
sy1 = 150.0 * 1000.0
sy2 = 300.0 * 1000.0

# Set values for the southern edge
damp_func[y_u <= sy1] = 1.0
s_mask = (y_u > sy1) & (y_u <= sy2)
# Linear transition from 1.0 at sy1 to 0.0 at sy2
damp_func[s_mask] = (sy2 - y_u[s_mask]) / (sy2 - sy1)

# --- Northern boundary sponge layer ---
# Calculate based on the maximum Y coordinate
y_max = np.max(y_u)
ny1 = y_max - 150.0 * 1000.0  # 150km from the north end (1.0 north of here)
ny2 = y_max - 300.0 * 1000.0  # 300km from the north end (transition start)

# Set values for the northern edge
damp_func[y_u >= ny1] = 1.0
n_mask = (y_u < ny1) & (y_u >= ny2)
# Linear transition from 0.0 at ny2 to 1.0 at ny1
damp_func[n_mask] = (y_u[n_mask] - ny2) / (ny1 - ny2)

# Map the 1D damping function to the 2D grid and apply the time factor
for ix in range(0, nx):
    damp_u[0:ny, ix] = damp_func[0:ny] * factor

damp_p[:]=0.0
damp_v[:]=0.0
# Save and close the file
nc1.close()