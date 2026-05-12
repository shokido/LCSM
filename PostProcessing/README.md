# LCSM PostProcessing

This directory contains a Fortran post-processing tool for converting LCSM
modal output to standard depth-level fields.  LCSM writes model results as
modal coefficients, with dimensions conceptually like `time x mode x lat x lon`
for variables such as `u`, `v`, and `p`.  The post-processing tool combines
those modal coefficients with stratification and mode-function information
(`phi`, `phidz`, and `psi_w`) to reconstruct fields on `time x lev x lat x lon`
output grids.

The main program is:

```text
convert_LCSM_to_zlev.f90
```

It builds the executable:

```text
exec_convert_LCSM_to_zlev.out
```

## What The Tool Does

For each input LCSM output file, the converter:

1. Reads the LCSM grid and mask from `fname_grid`.
2. Reads target output depths from `fname_lev`.
3. Reads modal functions from `fname_strf`:
   - `phi`
   - `phidz`
   - `psi_w`
4. Reads modal LCSM output from `fname_ocn`:
   - `p`
   - `u`
   - `v`
5. Sums the requested modal range `im1:im2`.
6. Writes reconstructed depth-level fields to `fname_out`.

The output variables are:

| Variable | Meaning | Units |
|---|---|---|
| `p` | reconstructed pressure-like field | `m^2/s^2` |
| `u` | reconstructed zonal velocity on p-grid points | `m/s` |
| `v` | reconstructed meridional velocity on p-grid points | `m/s` |
| `rho` | reconstructed density perturbation | currently blank in the code |
| `w` | reconstructed vertical velocity from horizontal divergence and `psi_w` | `m/s` |
| `mask_p` | p-grid land-sea mask copied from the grid file | none |
| `x`, `y` | Cartesian coordinates corresponding to output `lon`, `lat` | `m` |

## Requirements

- `gfortran`
- NetCDF C library
- NetCDF Fortran library
- `make`

The Makefile currently contains machine-specific Homebrew paths:

```make
NETCDF_INCDIR=-I/opt/homebrew/Cellar/netcdf-fortran/4.6.1/include
NETCDF_LIBDIR=-L/opt/homebrew/Cellar/netcdf-fortran/4.6.1/lib -L/opt/homebrew/Cellar/netcdf/4.9.2_2/lib
```

Edit `NETCDF_INCDIR` and `NETCDF_LIBDIR` if NetCDF is installed elsewhere.

## Compilation

Run `make` inside the `PostProcessing` directory:

```bash
cd PostProcessing
make
```

This compiles:

- `ncdf_read.f90`
- `ncdf_write.f90`
- `convert_LCSM_to_zlev.f90`

and links them into:

```text
exec_convert_LCSM_to_zlev.out
```

To remove compiled files:

```bash
make clean
```

## Running The Converter

The executable reads a namelist from standard input:

```bash
./exec_convert_LCSM_to_zlev.out < filenames_convert_LCSM_to_zlev.nml
```

An alternate namelist example is also present in `OUTPUTS`:

```bash
./exec_convert_LCSM_to_zlev.out < ../OUTPUTS/filenames_convert_to_z_4d.nml
```

Check paths before running.  In particular, stratification files produced by
the current preprocessing workflow are usually under:

```text
../PreProcessing/STRF/STRF_TLLL/
```

## Namelist File

The default namelist file is:

```text
filenames_convert_LCSM_to_zlev.nml
```

It contains two groups: `&param` and `&fnames`.

### `&param`

| Variable | Example value | Meaning |
|---|---:|---|
| `im1` | `1` | First vertical mode to include in the reconstruction. |
| `im2` | `5` | Last vertical mode to include in the reconstruction. |
| `nfile` | `1` | Number of input/output file triplets to process. |

The converter sums modes from `im1` through `im2`.  For example:

- `im1=1`, `im2=1`: reconstruct mode 1 only.
- `im1=1`, `im2=5`: reconstruct the sum of modes 1-5.
- `im1=6`, `im2=10`: reconstruct modes 6-10.

### `&fnames`

| Variable | Example value | Meaning |
|---|---|---|
| `fname_grid` | `../PreProcessing/GRID/grid_eqpac_30.nc` | LCSM grid file containing `mask_p`. |
| `fname_lev` | `lev_out.dat` | Text file listing target output depth levels. |
| `fname_strf(1)` | `../PreProcessing/STRF/STRF_TLLL/WOA18_strf_ann_eqpac_30_mode5.nc` | Stratification/mode-function NetCDF file. |
| `fname_ocn(1)` | `../OUTPUTS/avg_eqpac30_eqpatch_mode5_unicp.nc` | Input LCSM modal output file. |
| `fname_out(1)` | `../OUTPUTS/avg_zlev_eqpac30_eqpatch_mode5_unicp.nc` | Output depth-level NetCDF file. |

For `nfile > 1`, provide indexed entries for each file:

```fortran
fname_strf(1)="..."
fname_ocn(1)="..."
fname_out(1)="..."
fname_strf(2)="..."
fname_ocn(2)="..."
fname_out(2)="..."
```

`fname_grid` and `fname_lev` are shared across all processed files.

## Target Depth File

`lev_out.dat` defines the vertical levels of the output file.  The first line
is the number of output levels.  Each following line is one depth level in
meters.

Current example:

```text
20
0.0
5.0
10.0
...
500.0
```

The converter linearly interpolates the mode-function fields from the
stratification file's `lev` coordinate to these target levels.

## Expected Input Files

### Grid File

`fname_grid` must contain:

| Variable | Purpose |
|---|---|
| `mask_p` | land-sea mask on the p-grid |

### Stratification / Mode-Function File

`fname_strf(*)` must contain:

| Variable / dimension | Purpose |
|---|---|
| `lon`, `lat` | horizontal coordinates |
| `lev` | mode-function vertical coordinate |
| `mode` | vertical mode dimension |
| `phi(lon, lat, lev, mode)` | modal structure for `p`, `u`, and `v` reconstruction |
| `phidz(lon, lat, lev, mode)` | vertical derivative used for `rho` reconstruction |
| `psi_w(lon, lat, lev, mode)` | vertical structure used for `w` reconstruction |

These are produced by the `STRF_TLLL` preprocessing workflow.

### LCSM Modal Output File

`fname_ocn(*)` must contain:

| Variable / dimension | Purpose |
|---|---|
| `x_p`, `y_p`, `lon_p`, `lat_p` | p-grid coordinates |
| `x_u` | u-grid x coordinate, used for `dudx` |
| `y_v` | v-grid y coordinate, used for `dvdy` |
| `time` | time coordinate copied to the output |
| `p(x_p, y_p, mode, time)` | modal pressure coefficient |
| `u(x_u, y_u, mode, time)` | modal zonal velocity coefficient |
| `v(x_v, y_v, mode, time)` | modal meridional velocity coefficient |

The code trims the outer halo points and writes interior fields with output
dimensions:

```text
lon x lat x lev x time
```

Conceptually, this corresponds to `time x lev x lat x lon` when viewed in
common analysis tools such as Python/xarray.

## Reconstruction Notes

- `p` is reconstructed by multiplying modal `p` by `phi` and `rho0`.
- `rho` is reconstructed from modal `p`, `phidz`, `rho0`, and `g`.
- `u` and `v` are first averaged from staggered grid points to p-grid points,
  then multiplied by `phi`.
- `w` is computed from `-(dudx + dvdy) * psi_w`.
- Missing values use `9999.0`.
- The constants used in the converter are `rho0=1024.0 kg/m^3` and
  `g=9.8 m/s^2`.

## Example Workflow

```bash
cd PostProcessing

# Edit NetCDF paths in Makefile if needed.
make

# Edit filenames_convert_LCSM_to_zlev.nml for your grid, mode-function file,
# LCSM output file, output path, and mode range.
./exec_convert_LCSM_to_zlev.out < filenames_convert_LCSM_to_zlev.nml
```

After completion, the output file can be opened as a standard depth-level
NetCDF dataset containing `p`, `u`, `v`, `rho`, and `w`.
