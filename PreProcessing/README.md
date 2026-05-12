# LCSM PreProcessing

This directory contains preprocessing tools for the Linear Continuously
Stratified Model (LCSM).  The scripts generate the horizontal model grid,
land-sea mask, boundary damping coefficients, wind-stress forcing, and
stratification-related input files used by LCSM experiments.

The current scripts are mainly configured for the `eqpac_30` domain
(`110E-300E`, `30S-30N`, `2.5 deg x 1.0 deg`).  Many parameters and file
paths are currently hard-coded near the top of each script, so check and edit
those values before running a new experiment.

## Directory Layout

```text
PreProcessing/
├── DATA/          External source datasets used by preprocessing
├── GRID/          Horizontal grid, mask, and damping tools
├── WIND/          Wind-stress remapping and idealized wind forcing
└── STRF/
    ├── STRF_SW/   Simple shallow-water stratification file generator
    ├── STRF_TLLL/ T/S-based density and vertical-mode preprocessing
    └── OLD/       Older NetCDF helper sources
```

## Main Products

The preprocessing workflow produces NetCDF files such as:

- `GRID/grid_<domain>.nc`: LCSM C-grid geometry, longitude/latitude,
  Coriolis parameter, mask fields, and damping fields.
- `GRID/mask_<domain>.nc` and `GRID/mask_<domain>_rev.nc`: automatically
  generated and manually revised land-sea masks.
- `WIND/jra55do_uw_clm_<domain>.nc`,
  `WIND/jra55do_vw_clm_<domain>.nc`: wind-stress data remapped to the LCSM
  grid.
- `WIND/wstress_eqpatch_<domain>.nc`: idealized equatorial wind-stress patch.
- `STRF/STRF_SW/SW_strf_<domain>.nc`: one-mode shallow-water stratification
  coefficients.
- `STRF/STRF_TLLL/WOA18_strf_ann_<domain>_mode<N>.nc`: vertical-mode
  coefficients derived from temperature/salinity climatology.
- `STRF/STRF_TLLL/WOA18_cn_ann_1d_<domain>_mode<N>.nc`: horizontal mean phase
  speeds expanded back onto the model grid.

## Requirements

The tools assume the following command-line and Python/Fortran environment:

- Python 3
- Python packages: `numpy`, `netCDF4`, `matplotlib`
- CDO (`cdo`) for horizontal remapping
- Fortran compiler, currently `gfortran`
- NetCDF C/Fortran libraries
- LAPACK, used by the vertical-mode solver

The Fortran Makefile in `STRF/STRF_TLLL` contains Homebrew-style NetCDF paths:

```make
NETCDF_INCDIR=-I/opt/homebrew/Cellar/netcdf-fortran/4.6.1/include
NETCDF_LIBDIR=-L/opt/homebrew/Cellar/netcdf-fortran/4.6.1/lib ...
```

Update these paths if NetCDF is installed elsewhere.

## GRID Workflow

Run the grid tools from the `GRID` directory.

```bash
cd GRID
python define_grid.py
```

`define_grid.py` creates the base LCSM horizontal grid file
`grid_<domain>.nc`.  It writes p/u/v C-grid coordinates, 1-D and 2-D metric
coordinates, longitude/latitude, Coriolis parameter, initial mask values, and
initial damping coefficients.  It also writes a CDO grid-description file used
for remapping.

To create an initial mask from topography:

```bash
python create_mask_from_topo.py
```

This remaps `DATA/etopo5.nc` to the LCSM grid using CDO and masks points
shallower than the threshold set in the script (`hthres=-100.0` by default).

To edit the mask interactively:

```bash
python edit_mask.py --in mask_eqpac_30.nc --out mask_eqpac_30_rev.nc
```

Mask editing uses a Matplotlib window:

- Double-click a grid cell to toggle ocean/land.
- Press `0` to mark the first corner of a rectangular edit region.
- Press `1` to set the selected rectangle to land.
- Press `2` to set the selected rectangle to ocean.

To insert the revised mask into the grid file:

```bash
python apply_mask.py
```

To add a meridional sponge/damping layer near the northern and southern
boundaries:

```bash
python add_damper.py
```

The current damping script writes damping only to `damp_u`; `damp_p` and
`damp_v` are set to zero.  The damping amplitude is scaled by `1/day`.

## WIND Workflow

Run wind tools from the `WIND` directory.

```bash
cd WIND
python remap_wstess.py
```

`remap_wstess.py` remaps JRA55-do monthly climatological wind-stress files to
the LCSM grid using CDO.  The source file paths are hard-coded and should be
checked before use.

For an idealized forcing experiment:

```bash
python create_idealizedwind_eqpatch.py
```

This creates a Gaussian zonal wind-stress patch centered near the equator and
`180E`, with no meridional wind stress.

## STRF Workflow

The `STRF` directory provides two ways to create LCSM stratification input.

### Shallow-Water Coefficients

For a simple one-mode shallow-water setup:

```bash
cd STRF/STRF_SW
python create_strf_shallowwater.py
```

The current default uses:

- equivalent depth scale `H=130 m`
- phase speed `cp=2.9 m/s`
- one vertical mode

The output contains `cn`, `ohn`, and `obn` on the LCSM grid.

### T/S-Based Vertical Modes

The `STRF/STRF_TLLL` workflow derives vertical-mode coefficients from gridded
temperature and salinity data.

First, remap source temperature/salinity files to the LCSM grid if needed:

```bash
cd STRF/STRF_TLLL
python remap_ts.py
```

Check `remap_ts.py` before running; the current script contains example ORAS4
input names and an `indian` grid setting.

Build the Fortran tools:

```bash
make
```

Then run the executables with their namelist files:

```bash
./exec_get_pdens.out < filename_get_pdens_LCSMgrid.nml
./exec_do_flood.out < filename_do_flood_LCSM.nml
./exec_get_vmode_TLLL.out < filename_get_vmode_TLLL.nml
```

The steps are:

1. `exec_get_pdens.out`: compute potential density from temperature and
   salinity.
2. `exec_do_flood.out`: fill missing density values at wet model points using
   nearest valid neighboring values.
3. `exec_get_vmode_TLLL.out`: compute buoyancy frequency and vertical-mode
   quantities, including `cn`, `ohn`, `obn`, `phi`, `phidz`, and `psi_w`.

Finally, if a horizontally uniform phase-speed file is needed:

```bash
python aave_cn_4d.py
```

This computes horizontal/time mean `cn` values and writes them back to a
grid-shaped NetCDF file.

## Typical End-to-End Order

For the default `eqpac_30` setup, a typical preprocessing sequence is:

```bash
cd GRID
python define_grid.py
python create_mask_from_topo.py
python edit_mask.py --in mask_eqpac_30.nc --out mask_eqpac_30_rev.nc
python apply_mask.py
python add_damper.py

cd ../WIND
python remap_wstess.py
# or:
python create_idealizedwind_eqpatch.py

cd ../STRF/STRF_SW
python create_strf_shallowwater.py

# For T/S-derived vertical modes instead of the shallow-water setup:
cd ../STRF_TLLL
make
./exec_get_pdens.out < filename_get_pdens_LCSMgrid.nml
./exec_do_flood.out < filename_do_flood_LCSM.nml
./exec_get_vmode_TLLL.out < filename_get_vmode_TLLL.nml
python aave_cn_4d.py
```

## Notes for New Domains

When preparing a new domain, update the domain name, longitude/latitude range,
grid spacing, source file names, CDO grid-description file names, and namelist
outputs consistently across:

- `GRID/define_grid.py`
- `GRID/create_mask_from_topo.py`
- `GRID/apply_mask.py`
- `GRID/add_damper.py`
- `WIND/remap_wstess.py`
- `WIND/create_idealizedwind_eqpatch.py`
- `STRF/STRF_SW/create_strf_shallowwater.py`
- `STRF/STRF_TLLL/*.nml`
- `STRF/STRF_TLLL/remap_ts.py`
- `STRF/STRF_TLLL/aave_cn_4d.py`

Some scripts refer to `des_grid_<domain>.dat` and others to
`gdes_grid_<domain>.dat`; verify the grid-description filename before running
CDO-based steps.
