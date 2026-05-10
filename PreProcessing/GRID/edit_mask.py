# Python script for construct LOM grid from ROMS grid
import netCDF4 as ncdf
import numpy as np
import subprocess
import argparse
from sub_grid import *

parser = argparse.ArgumentParser()
parser.add_argument("--in", dest="fname_mask", required=True)
parser.add_argument("--out", dest="fname_mask_rev", required=True)
args = parser.parse_args()

fname_mask = args.fname_mask
fname_mask_rev = args.fname_mask_rev

nc_in = ncdf.Dataset(fname_mask, 'r')
lon = nc_in.variables["lon"][:]
lat = nc_in.variables["lat"][:]
mask = nc_in.variables["mask"][:]
nc_in.close()

hgrid = grid_data(lon, lat)
hgrid.mask = mask
hgrid.edit_mask()
hgrid.save_mask_ncdf(fname_mask_rev)