import netCDF4 as ncdf
import subprocess 
import matplotlib.pyplot as plt
import numpy as np
import struct
class grid_data:
     def __init__(self,lon,lat):
        self.lon=lon
        self.lat=lat
        nlon=len(self.lon)
        nlat=len(self.lat)
        mask_rho=np.zeros((nlat,nlon))
        self.mask=mask_rho
        self.topo=mask_rho
        x,y=np.meshgrid(self.lon,self.lat)
        self.x=x
        self.y=y
        x_vert=np.empty((nlat+1,nlon+1))
        y_vert=np.empty((nlat+1,nlon+1))
        x_vert[1:-1,1:-1]=0.25*(x[1:,1:]+x[1:,:-1]+x[:-1,1:]+x[:-1,:-1])
        y_vert[1:-1,1:-1]=0.25*(y[1:,1:]+y[1:,:-1]+y[:-1,1:]+y[:-1,:-1])
        x_vert[-1,1:-1] = x_vert[-2,1:-1]
        y_vert[-1,1:-1] = y_vert[-2,1:-1] + (y[-1,0:-1]-y[-2,0:-1])
        x_vert[0,1:-1] = x_vert[1,1:-1]
        y_vert[0,1:-1] = y_vert[1,1:-1] - (y[1,0:-1]-y[0,0:-1])
        x_vert[1:-1,-1] = x_vert[1:-1,-2] + (x[0:-1,-1]-x[0:-1,-2])
        y_vert[1:-1,-1] = y_vert[1:-1,-2] 
        x_vert[1:-1,0] = x_vert[1:-1,1] - (x[0:-1,1]-x[0:-1,0])
        y_vert[1:-1,0] = y_vert[1:-1,1] 
        x_vert[0,0] = 4.0*x[0,0]-x_vert[1,0]-x_vert[0,1]-x_vert[1,1]
        x_vert[-1,0] = 4.0*x[-1,0]-x_vert[-2,0]-x_vert[-1,1]-x_vert[-2,1]
        x_vert[0,-1] = 4.0*x[0,-1]-x_vert[0,-2]-x_vert[1,-1]-x_vert[1,-2]
        x_vert[-1,-1] = 4.0*x[-1,-1]-x_vert[-2,-2]-x_vert[-2,-1]-x_vert[-1,-2]

        y_vert[0,0] = 4.0*y[0,0]-y_vert[1,0]-y_vert[0,1]-y_vert[1,1]
        y_vert[-1,0] = 4.0*y[-1,0]-y_vert[-2,0]-y_vert[-1,1]-y_vert[-2,1]
        y_vert[0,-1] = 4.0*y[0,-1]-y_vert[0,-2]-y_vert[1,-1]-y_vert[1,-2]
        y_vert[-1,-1] = 4.0*y[-1,-1]-y_vert[-2,-2]-y_vert[-2,-1]-y_vert[-1,-2]
        self.x_vert=x_vert
        self.y_vert=y_vert
        self.istrx=-1
        self.istry=-1
     def read_mask_binary(self,fname_in):
        nlon=len(self.lon)
        nlat=len(self.lat)
        f = open(fname_in, "rb")
        data_in = np.frombuffer(f.read(nlon*nlat*4),">f")
        data_in=np.reshape(data_in,(nlat,nlon))
        mask_in=np.copy(data_in)
        self.mask=mask_in
        f.close()
     def draw_mask(self):
        plt.pcolormesh(self.x_vert,self.y_vert,self.topo, edgecolors="w",cmap="jet")
        plt.pcolormesh(self.x_vert,self.y_vert,self.mask, edgecolors="w",cmap="Greys_r",alpha=0.5)
#        plt.show()
     def onclick(self,event):
        if event.dblclick!=1 :
            return
        if event.xdata == None or event.ydata == None:
            return
        else:
            xc=event.xdata
            yc=event.ydata
            abs_x=np.abs(self.lon-xc)
            abs_y=np.abs(self.lat-yc)
            ind_x=np.where(abs_x==np.min(abs_x))[0]
            ind_y=np.where(abs_y==np.min(abs_y))[0]
            mask_bef=np.copy(self.mask)
            xv=mask_bef[ind_y,ind_x]
            if (xv==0):
                self.mask[ind_y,ind_x]=1
            else:
                self.mask[ind_y,ind_x]=0
            plt.ion()
            self.draw_mask() 
            plt.draw()
     
     def onkey(self,event):
        if event.xdata == None or event.ydata == None:
            return
        else:
            xc=event.xdata
            yc=event.ydata
            abs_x=np.abs(self.lon-xc)
            abs_y=np.abs(self.lat-yc)
            ind_x=np.where(abs_x==np.min(abs_x))[0]
            ind_y=np.where(abs_y==np.min(abs_y))[0]
            if event.key == '0':
                self.istrx=ind_x[0]
                self.istry=ind_y[0]
            elif event.key == '1':
                if (self.istrx==-1):
                    return
                else:
                    self.mask[self.istry:(ind_y[0]+1),self.istrx:(ind_x[0]+1)]=0
                    plt.ion()                 
                    self.draw_mask() 
                    plt.draw()
            elif event.key == '2':
                self.mask[self.istry:(ind_y[0]+1),self.istrx:(ind_x[0]+1)]=1
                plt.ion()                 
                self.draw_mask() 
                plt.draw()

     def edit_mask(self):
        self.draw_mask() 
        plt.connect('button_press_event', self.onclick)
        plt.connect('key_press_event', self.onkey)
        plt.show()
     def save_mask_ncdf(self,fname_out):
        lonname="lon"
        latname="lat"
        nc_out=ncdf.Dataset(fname_out,"w")
        nc_out.createDimension(lonname,len(self.lon))
        nc_out.createDimension(latname,len(self.lat))

        nc_out.createVariable(lonname,self.lon.dtype,(lonname))
        nc_out.createVariable(latname,self.lat.dtype,(latname))
        nc_out.createVariable("mask",self.mask.dtype,(latname,lonname))
        nc_out.variables[lonname].long_name = 'longitude'
        nc_out.variables[lonname].units = 'degrees_east'
        nc_out.variables[latname].long_name = 'latitude'
        nc_out.variables[latname].units = 'degrees_north'
        nc_out.variables["mask"].long_name = 'Land-sea masking'
        nc_out.variables[lonname][:]=self.lon[:]
        nc_out.variables[latname][:]=self.lat[:]
        nc_out.variables["mask"][:]=self.mask[:]
        nc_out.close()
     def save_mask_binary(self,fname_out):
        f_out=open(fname_out,"wb")
        var=self.mask
        b1 = struct.pack('>'+str(len(var.ravel()))+'f',*var.ravel().astype("f4")[:] )
        f_out.write(b1)