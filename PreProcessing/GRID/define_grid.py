# Python script for defining grid
import numpy as np
import netCDF4 as netCDF
import math
import os
import subprocess

fname_grid="grid_indian.nc"
fname_gdes="gdes_grid_indian.dat"
lon_w=28.0
lon_e=114.0
lat_s=-25.0
lat_n=25.0
dlon=1.0
dlat=1.0
missing_value=9999.0

def cal_dis_lon_lat(lon1,lon2,lat1,lat2):
    R=6378137
    pi = np.pi
    deg_to_rad = 2.0 * pi / (360.0)
    dlon=deg_to_rad * (lon2-lon1)
    dlat=deg_to_rad * (lat2-lat1)
    a=np.sin(dlat/2.0) * np.sin(dlat/2.0) + np.cos(deg_to_rad * (lat1)) * np.cos(deg_to_rad * (lat2)) * np.sin(dlon/2.0) * np.sin(dlon/2.0)
    dist= 2.0 * R * math.atan2(np.sqrt(a),np.sqrt(1.0-a))
    return(dist)
def dis_to_lat(dist):
    R=6378137
    pi = np.pi
    deg_to_rad = 2.0 * pi / (360.0)
    lat=360*dist / (2.0 * pi * R)# * rad_to_deg
    return(lat)

def cal_f(lat):
    pi = np.pi
    deg_to_rad = 2.0 * pi / (360.0)
    omega = 7.29e-5
    f = 2.0 * omega * np.sin(lat * deg_to_rad)
    return(f)
def gen_cgrid(x_vert,y_vert):
    nx=x_vert.size ; ny=y_vert.size
    x1=1.5 * x_vert[0]-0.5 * x_vert[1]
    x2=1.5 * x_vert[nx-1] - 0.5 * x_vert[nx-2]
    x_rho=np.linspace(x1,x2,nx+1)
    x_u=np.linspace(x_vert[0],x_vert[nx-1],nx)
    x_v=np.linspace(x1,x2,nx+1)
    y1=1.5 * y_vert[0]-0.5 * y_vert[1]
    y2=1.5 * y_vert[ny-1] - 0.5 * y_vert[ny-2]
    y_rho=np.linspace(y1,y2,ny+1)
    y_u=np.linspace(y1,y2,ny+1)
    y_v=np.linspace(y_vert[0],y_rho[ny-1],ny)
    return(x_rho,y_rho,x_u,y_u,x_v,y_v)

    
#========================================================
# Number of grid
nx = int((lon_e-lon_w) / dlon + 1)
ny = int((lat_n-lat_s) / dlat + 1)

lon_vert = np.linspace(lon_w,lon_e,nx)
lat_vert = np.linspace(lat_s,lat_n,ny)

x_vert=np.zeros(nx)
y_vert=np.zeros(ny)
for ix in range(0,nx):
    x_vert[ix]=cal_dis_lon_lat(lon_vert[0],lon_vert[ix],0.0,0.0)
for iy in range(0,ny):
    y_vert[iy]=cal_dis_lon_lat(lon_vert[0],lon_vert[0],lat_vert[0],lat_vert[iy])

x_rho,y_rho,x_u,y_u,x_v,y_v=gen_cgrid(x_vert,y_vert)
lon_rho,lat_rho,lon_u,lat_u,lon_v,lat_v=gen_cgrid(lon_vert,lat_vert)

nx_rho=np.shape(x_rho)[0]
ny_rho=np.shape(y_rho)[0]
nx_u=np.shape(x_u)[0]
ny_u=np.shape(y_u)[0]
nx_v=np.shape(x_v)[0]
ny_v=np.shape(y_v)[0]

# Create Corioris parameter
f=np.zeros(ny+1)
for iy in range(0,ny+1):
    f[iy]= cal_f(lat_rho[iy])

# Create damper
damp_p_out=np.zeros((ny_rho,nx_rho))
damp_u_out=np.zeros((ny_u,nx_u))
damp_v_out=np.zeros((ny_v,nx_v))

mask_p=np.ones((ny_rho,nx_rho))
    
isexist=os.path.exists(fname_grid)
if (isexist):
    command=("rm",fname_grid)
    subprocess.check_call(command)

nc_grid=netCDF.Dataset(fname_grid,"w")
nc_grid.createDimension('x_p',nx_rho)
nc_grid.createDimension('y_p',ny_rho)
nc_grid.createDimension('x_u',nx_u)
nc_grid.createDimension('y_u',ny_u)
nc_grid.createDimension('x_v',nx_v)
nc_grid.createDimension('y_v',ny_v)

xp = nc_grid.createVariable('x_p',x_rho.dtype, ('x_p'))
xp.long_name = 'zonal distance at p-grid'
xp.units = 'm'
xp.missing_value = 9999.

yp = nc_grid.createVariable('y_p',y_rho.dtype, ('y_p'))
yp.long_name = 'zonal distance at p-grid'
yp.units = 'm'
yp.missing_value = 9999.

xu = nc_grid.createVariable('x_u',x_u.dtype, ('x_u'))
xu.long_name = 'zonal distance at u-grid'
xu.units = 'm'
xu.missing_value = 9999.

yu = nc_grid.createVariable('y_u',y_u.dtype, ('y_u'))
yu.long_name = 'zonal distance at u-grid'
yu.units = 'm'
yu.missing_value = 9999.
    
xv = nc_grid.createVariable('x_v',x_v.dtype, ('x_v'))
xv.long_name = 'zonal distance at v-grid'
xv.vnits = 'm'
xv.missing_valve = 9999.

yv = nc_grid.createVariable('y_v',y_v.dtype, ('y_v'))
yv.long_name = 'zonal distance at v-grid'
yv.vnits = 'm'
yv.missing_valve = 9999.

lonp = nc_grid.createVariable('lon_p',x_rho.dtype, ('x_p'))
lonp.long_name = 'Longitude at p-grid'
lonp.units = 'degrees_east'
lonp.missing_value = 9999.

latp = nc_grid.createVariable('lat_p',y_rho.dtype, ('y_p'))
latp.long_name = 'Latitude at p-grid'
latp.units = 'degrees_north'
latp.missing_value = 9999.

lonu = nc_grid.createVariable('lon_u',x_u.dtype, ('x_u'))
lonu.long_name = 'Longitude at u-grid'
lonu.units = 'degrees_east'
lonu.missing_value = 9999.

latu = nc_grid.createVariable('lat_u',y_u.dtype, ('y_u'))
latu.long_name = 'Latitude at u-grid'
latu.units = 'degrees_north'
latu.missing_value = 9999.

lonv = nc_grid.createVariable('lon_v',x_v.dtype, ('x_v'))
lonv.long_name = 'Longitude at v-grid'
lonv.units = 'degrees_east'
lonv.missing_value = 9999.

latv = nc_grid.createVariable('lat_v',y_v.dtype, ('y_v'))
latv.long_name = 'Latitude at v-grid'
latv.units = 'degrees_north'
latv.missing_value = 9999.

fp = nc_grid.createVariable('f',y_rho.dtype, ('y_p'))
fp.long_name = 'Coriolis parameter at p-grid'
fp.units = '1/s'
fp.missing_value = 9999.

maskp = nc_grid.createVariable('mask_p',y_rho.dtype, ('y_p','x_p'))
maskp.long_name = 'mask at p-grid'
maskp.units = ''
maskp.missing_value = 9999.

damp_p = nc_grid.createVariable('damp_p',y_rho.dtype, ('y_p','x_p'))
damp_p.long_name = 'damping coefficients at p-grid'
damp_p.units = ''
damp_p.missing_value = 9999.

damp_u = nc_grid.createVariable('damp_u',y_rho.dtype, ('y_u','x_u'))
damp_u.long_name = 'damping coefficients at u-grid'
damp_u.units = ''
damp_u.missing_value = 9999.

damp_v = nc_grid.createVariable('damp_v',y_rho.dtype, ('y_v','x_v'))
damp_v.long_name = 'damping coefficients at v-grid'
damp_v.units = ''
damp_v.missing_value = 9999.
    
    
xp[:]=x_rho
yp[:]=y_rho
xu[:]=x_u
yu[:]=y_u
xv[:]=x_v
yv[:]=y_v
    
lonp[:]=lon_rho
latp[:]=lat_rho
lonu[:]=lon_u
latu[:]=lat_u
lonv[:]=lon_v
latv[:]=lat_v
fp[:]=f
maskp[:,:]=mask_p
damp_u[:,:]=damp_u_out
damp_v[:,:]=damp_v_out
damp_p[:,:]=damp_p_out

nc_grid.close()    

# line='gridtype = lonlat'
# print(line)
# line='xsize    =  '+str(nx_rho)
# print(line)
# line='ysize    =  '+str(ny_rho)
# print(line)
# line='xfirst   = '+str(lon_rho[0])
# print(line)
# line='xinc     =  '+str(dlon)
# print(line)
# line='yfirst   = '+str(lat_rho[0])
# print(line)
# line='xinc     =  '+str(dlat)
# print(line)
xsize=np.shape(lon_rho)[0]
ysize=np.shape(lat_rho)[0]
gridsize=xsize*ysize
lon_2d,lat_2d=np.meshgrid(lon_rho,lat_rho)
lon_1d=np.reshape(lon_2d,[gridsize])
lat_1d=np.reshape(lat_2d,[gridsize])

f=open(fname_gdes,"w")
line='gridtype= curvilinear\n'
f.write(line)
line='gridsize= '+str(gridsize)+'\n'
f.write(line)
line='xsize= '+str(xsize)+'\n'
f.write(line)
line='ysize= '+str(ysize)+'\n'
f.write(line)
line='\n'
f.write(line)

line='#Longitudes \n'
f.write(line)
line='xvals = \n'
f.write(line)

for i in range(0,gridsize):
    line=str(lon_1d[i])+" \n"
    f.write(line)

line='#Latitudes \n'
f.write(line)
line='yvals = \n'
f.write(line)

for i in range(0,gridsize):
    line=str(lat_1d[i])+" \n"
    f.write(line)
    
f.close()
