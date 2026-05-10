# python script for defining grid
import numpy as np
import netCDF4 as ncdf
import math
import os
import subprocess
gname="eqpac_30"
lon_w=110.0;lon_e=300.0;lat_s=-30.0;lat_n=30.0
dlon=2.5;dlat=1.0
fname_grid="grid_"+gname+".nc"
fname_gdes="des_grid_"+gname+".dat"
undef=-9999.9

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
    x_p=np.linspace(x1,x2,nx+1)
    x_u=np.linspace(x_vert[0],x_vert[nx-1],nx)
    x_v=np.linspace(x1,x2,nx+1)
    y1=1.5 * y_vert[0]-0.5 * y_vert[1]
    y2=1.5 * y_vert[ny-1] - 0.5 * y_vert[ny-2]
    y_p=np.linspace(y1,y2,ny+1)
    y_u=np.linspace(y1,y2,ny+1)
    y_v=np.linspace(y_vert[0],y_vert[ny-1],ny)
    return(x_p,y_p,x_u,y_u,x_v,y_v)

    
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

x_p,y_p,x_u,y_u,x_v,y_v=gen_cgrid(x_vert,y_vert)
lon_p,lat_p,lon_u,lat_u,lon_v,lat_v=gen_cgrid(lon_vert,lat_vert)

x_p_2d_out=np.zeros((len(y_p),len(x_p)));y_p_2d_out=np.zeros((len(y_p),len(x_p)))
x_u_2d_out=np.zeros((len(y_u),len(x_u)));y_u_2d_out=np.zeros((len(y_u),len(x_u)))
x_v_2d_out=np.zeros((len(y_v),len(x_v)));y_v_2d_out=np.zeros((len(y_v),len(x_v)))
ix=0
for iy in range(0,len(y_p)):
    x_p_2d_out[iy,ix]=-1.0*cal_dis_lon_lat(lon_vert[0],lon_p[0],lat_p[iy],lat_p[iy])
for iy in range(0,len(y_p)):
    for ix in range(1,len(x_p)):
        x_p_2d_out[iy,ix]=x_p_2d_out[iy,ix-1]+cal_dis_lon_lat(lon_p[ix-1],lon_p[ix],lat_p[iy],lat_p[iy])
iy=0
for ix in range(0,len(x_p)):
    y_p_2d_out[iy,ix]=-1.0*cal_dis_lon_lat(lon_p[ix],lon_p[ix],lat_vert[0],lat_p[0])
for iy in range(1,len(y_p)):
    for ix in range(0,len(x_p)):
        y_p_2d_out[iy,ix]=y_p_2d_out[iy-1,ix]+cal_dis_lon_lat(lon_p[ix],lon_p[ix],lat_p[iy-1],lat_p[iy])


ix=0
for iy in range(0,len(y_u)):
    x_u_2d_out[iy,ix]=-1.0*cal_dis_lon_lat(lon_vert[0],lon_u[0],lat_u[iy],lat_u[iy])
for iy in range(0,len(y_u)):
    for ix in range(1,len(x_u)):
        x_u_2d_out[iy,ix]=x_u_2d_out[iy,ix-1]+cal_dis_lon_lat(lon_u[ix-1],lon_u[ix],lat_u[iy],lat_u[iy])
iy=0
for ix in range(0,len(x_u)):
    y_u_2d_out[iy,ix]=-1.0*cal_dis_lon_lat(lon_u[ix],lon_u[ix],lat_vert[0],lat_u[0])
for iy in range(1,len(y_u)):
    for ix in range(0,len(x_u)):
        y_u_2d_out[iy,ix]=y_u_2d_out[iy-1,ix]+cal_dis_lon_lat(lon_u[ix],lon_u[ix],lat_u[iy-1],lat_u[iy])

ix=0
for iy in range(0,len(y_v)):
    x_v_2d_out[iy,ix]=-1.0*cal_dis_lon_lat(lon_vert[0],lon_v[0],lat_v[iy],lat_v[iy])
for iy in range(0,len(y_v)):
    for ix in range(1,len(x_v)):
        x_v_2d_out[iy,ix]=x_v_2d_out[iy,ix-1]+cal_dis_lon_lat(lon_v[ix-1],lon_v[ix],lat_v[iy],lat_v[iy])
iy=0
for ix in range(0,len(x_v)):
    y_v_2d_out[iy,ix]=-1.0*cal_dis_lon_lat(lon_v[ix],lon_v[ix],lat_vert[0],lat_v[0])
for iy in range(1,len(y_v)):
    for ix in range(0,len(x_v)):
        y_v_2d_out[iy,ix]=y_v_2d_out[iy-1,ix]+cal_dis_lon_lat(lon_v[ix],lon_v[ix],lat_v[iy-1],lat_v[iy])


nx_p=np.shape(x_p)[0]
ny_p=np.shape(y_p)[0]
nx_u=np.shape(x_u)[0]
ny_u=np.shape(y_u)[0]
nx_v=np.shape(x_v)[0]
ny_v=np.shape(y_v)[0]

# Create Corioris parameter
f=np.zeros((ny+1,nx+1))
for iy in range(0,ny+1):
    f[iy,:]= cal_f(lat_p[iy])

# Create damper
damp_p_out=np.ones((ny_p,nx_p))
damp_u_out=np.ones((ny_u,nx_u))
damp_v_out=np.ones((ny_v,nx_v))

mask_p=np.ones((ny_p,nx_p))
    
isexist=os.path.exists(fname_grid)
if (isexist):
    command=("rm",fname_grid)
    subprocess.check_call(command)

nc_grid=ncdf.Dataset(fname_grid,"w")
nc_grid.createDimension('x_p',nx_p)
nc_grid.createDimension('y_p',ny_p)
nc_grid.createDimension('x_u',nx_u)
nc_grid.createDimension('y_u',ny_u)
nc_grid.createDimension('x_v',nx_v)
nc_grid.createDimension('y_v',ny_v)

xp = nc_grid.createVariable('x_p',x_p.dtype, ('x_p'))
xp.long_name = 'zonal distance at p-grid'
xp.units = 'm'
xp.missing_value = undef

yp = nc_grid.createVariable('y_p',y_p.dtype, ('y_p'))
yp.long_name = 'zonal distance at p-grid'
yp.units = 'm'
yp.missing_value = undef

xu = nc_grid.createVariable('x_u',x_u.dtype, ('x_u'))
xu.long_name = 'zonal distance at u-grid'
xu.units = 'm'
xu.missing_value = undef

yu = nc_grid.createVariable('y_u',y_u.dtype, ('y_u'))
yu.long_name = 'zonal distance at u-grid'
yu.units = 'm'
yu.missing_value = undef
    
xv = nc_grid.createVariable('x_v',x_v.dtype, ('x_v'))
xv.long_name = 'zonal distance at v-grid'
xv.vnits = 'm'
xv.missing_valve = undef

yv = nc_grid.createVariable('y_v',y_v.dtype, ('y_v'))
yv.long_name = 'zonal distance at v-grid'
yv.vnits = 'm'
yv.missing_valve = undef

lonp = nc_grid.createVariable('lon_p',x_p.dtype, ('x_p'))
lonp.long_name = 'Longitude at p-grid'
lonp.units = 'degrees_east'
lonp.missing_value = undef

latp = nc_grid.createVariable('lat_p',y_p.dtype, ('y_p'))
latp.long_name = 'Latitude at p-grid'
latp.units = 'degrees_north'
latp.missing_value = undef

lonu = nc_grid.createVariable('lon_u',x_u.dtype, ('x_u'))
lonu.long_name = 'Longitude at u-grid'
lonu.units = 'degrees_east'
lonu.missing_value = undef

latu = nc_grid.createVariable('lat_u',y_u.dtype, ('y_u'))
latu.long_name = 'Latitude at u-grid'
latu.units = 'degrees_north'
latu.missing_value = undef

lonv = nc_grid.createVariable('lon_v',x_v.dtype, ('x_v'))
lonv.long_name = 'Longitude at v-grid'
lonv.units = 'degrees_east'
lonv.missing_value = undef

latv = nc_grid.createVariable('lat_v',y_v.dtype, ('y_v'))
latv.long_name = 'Latitude at v-grid'
latv.units = 'degrees_north'
latv.missing_value = undef

maskp = nc_grid.createVariable('mask_p',mask_p.dtype, ('y_p','x_p'))
maskp.long_name = 'mask at p-grid'
maskp.units = ''
maskp.missing_value = undef

masksst = nc_grid.createVariable('mask_sst',mask_p.dtype, ('y_p','x_p'))
masksst.long_name = 'mask at sst-grid'
masksst.units = ''
masksst.missing_value = undef

damp_p = nc_grid.createVariable('damp_p',y_p.dtype, ('y_p','x_p'))
damp_p.long_name = 'damping coefficients at p-grid'
damp_p.units = ''
damp_p.missing_value = undef

damp_u = nc_grid.createVariable('damp_u',y_p.dtype, ('y_u','x_u'))
damp_u.long_name = 'damping coefficients at u-grid'
damp_u.units = ''
damp_u.missing_value = undef

damp_v = nc_grid.createVariable('damp_v',y_p.dtype, ('y_v','x_v'))
damp_v.long_name = 'damping coefficients at v-grid'
damp_v.units = ''
damp_v.missing_value = undef

fp = nc_grid.createVariable('f',y_p.dtype, ('y_p','x_p'))
fp.long_name = 'Coriolis parameter at p-grid'
fp.units = '1/s'
fp.missing_value = undef

xp_2d = nc_grid.createVariable('x_p_2d',x_p.dtype, ('y_p','x_p'))
xp_2d.long_name = '2d zonal distance at p-grid'
xp_2d.units = 'm'
xp_2d.missing_value = undef

yp_2d = nc_grid.createVariable('y_p_2d',y_p.dtype, ('y_p','x_p'))
yp_2d.long_name = '2d meridional distance at p-grid'
yp_2d.units = 'm'
yp_2d.missing_value = undef

xu_2d = nc_grid.createVariable('x_u_2d',x_u.dtype, ('y_u','x_u'))
xu_2d.long_name = '2d zonal distance at u-grid'
xu_2d.units = 'm'
xu_2d.missing_value = undef

yu_2d = nc_grid.createVariable('y_u_2d',y_u.dtype, ('y_u','x_u'))
yu_2d.long_name = '2d meridional distance at u-grid'
yu_2d.units = 'm'
yu_2d.missing_value = undef

xv_2d = nc_grid.createVariable('x_v_2d',x_v.dtype, ('y_v','x_v'))
xv_2d.long_name = '2d zonal distance at v-grid'
xv_2d.units = 'm'
xv_2d.missing_value = undef

yv_2d = nc_grid.createVariable('y_v_2d',y_v.dtype, ('y_v','x_v'))
yv_2d.long_name = '2d meridional distance at v-grid'
yv_2d.units = 'm'
yv_2d.missing_value = undef
    
xp[:]=x_p
yp[:]=y_p
xu[:]=x_u
yu[:]=y_u
xv[:]=x_v
yv[:]=y_v
    
lonp[:]=lon_p
latp[:]=lat_p
lonu[:]=lon_u
latu[:]=lat_u
lonv[:]=lon_v
latv[:]=lat_v
fp[:,:]=f
maskp[:,:]=mask_p
masksst[:,:]=mask_p
damp_u[:,:]=damp_u_out
damp_v[:,:]=damp_v_out
damp_p[:,:]=damp_p_out
xp_2d[:,:]=x_p_2d_out;yp_2d[:,:]=y_p_2d_out
xu_2d[:,:]=x_u_2d_out;yu_2d[:,:]=y_u_2d_out
xv_2d[:,:]=x_v_2d_out;yv_2d[:,:]=y_v_2d_out

nc_grid.close()    
xsize=np.shape(lon_p)[0]
ysize=np.shape(lat_p)[0]
gridsize=xsize*ysize
lon_2d,lat_2d=np.meshgrid(lon_p,lat_p)
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
