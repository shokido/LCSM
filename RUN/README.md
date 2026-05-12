# LCSM RUN Namelists

This directory contains example namelist files for running the LCSM solver.
The table below documents the variables used in
`test_eqpac30_jra55do_mode5_varcp.nml`, which is an equatorial Pacific
(`eqpac_30`) experiment forced by JRA55-do climatological wind stress and
using five spatially varying vertical modes from `STRF_TLLL`.

## Running an Experiment

From the repository root, build the solver in `CODES`, then run it with a
namelist from `RUN`:

```bash
cd CODES
make

cd ../RUN
../CODES/exec_solver_lcsm_dyn.out < test_eqpac30_jra55do_mode5_varcp.nml
```

The namelist is read from standard input.  The order of namelist groups should
follow the order used in the file, because the main program reads them
sequentially.

## Example Namelist Summary

`test_eqpac30_jra55do_mode5_varcp.nml` configures:

- simulation period: `2015-01-01 00:00:00` to `2015-01-31 00:00:00`
- time step: `3600 s`
- grid: `../PreProcessing/GRID/grid_eqpac_30.nc`
- wind forcing: JRA55-do climatological `uw` and `vw`
- vertical-mode inputs: `cn` and `obn` from
  `WOA18_strf_ann_eqpac_30_mode5.nc`
- output interval: every 5 days for history and average files
- restart input: disabled
- restart output: `../OUTPUTS/restart_eqpac30_jra55do_mode5_unicp.nc`

## Time And Calendar Conventions

Dates use integer formats:

| Format | Meaning | Example |
|---|---|---|
| `YYYYMMDD` | calendar date | `20150101` |
| `HHMMSS` | time of day | `000000` |

Output interval flags follow the calendar utility used by the solver:

| Flag | Unit |
|---:|---|
| `-10000` | seconds |
| `-100` | minutes |
| `-1` | hours |
| `1` | days |
| `100` | months |
| `10000` | years |

In the current example, `out_hist_flag=1` and `out_avg_flag=1`, so
`out_hist_int=5` and `out_avg_int=5` mean 5 days.

## Namelist Variables

### `&date`

| Variable | Example value | Meaning | Units / notes |
|---|---:|---|---|
| `dt` | `3600.0` | Model time step. | seconds |
| `start_yymmdd` | `20150101` | Start date. | `YYYYMMDD` |
| `start_hhmmss` | `000000` | Start time. | `HHMMSS` |
| `end_yymmdd` | `20150131` | End date. | `YYYYMMDD` |
| `end_hhmmss` | `000000` | End time. | `HHMMSS` |

The total number of model steps is computed from the start/end timestamps in
seconds and divided by `dt`.

### `&grid`

| Variable | Example value | Meaning | Units / notes |
|---|---|---|---|
| `fname_in_grid` | `../PreProcessing/GRID/grid_eqpac_30.nc` | Input LCSM grid file. | Must contain p/u/v coordinates, masks, Coriolis parameter, and damping fields. |

### `&taux_param_ocn`

Zonal wind-stress input settings.

| Variable | Example value | Meaning | Units / notes |
|---|---:|---|---|
| `nfile_ocn_taux` | `1` | Number of zonal wind-stress files. | Determines the size of `fnames_ocn_taux`. |
| `timename_ocn_taux` | `"time"` | Time coordinate variable name in the NetCDF input. |  |
| `varname_ocn_taux` | `"uw"` | Zonal wind-stress variable name. | Expected on the p-grid. |
| `Lcycle_ocn_taux` | `"T"` | Use cyclic time interpolation. | `"T"` enables day-of-year cycling; other values use ordinary time interpolation. |
| `Tcycle_ocn_taux` | `365.0` | Length of the forcing cycle. | days |

### `&taux_io_ocn`

| Variable | Example value | Meaning | Units / notes |
|---|---|---|---|
| `fnames_ocn_taux(1)` | `../PreProcessing/WIND/jra55do_uw_clm_eqpac_30.nc` | Zonal wind-stress file. | Variable selected by `varname_ocn_taux`. |

### `&tauy_param_ocn`

Meridional wind-stress input settings.

| Variable | Example value | Meaning | Units / notes |
|---|---:|---|---|
| `nfile_ocn_tauy` | `1` | Number of meridional wind-stress files. | Determines the size of `fnames_ocn_tauy`. |
| `timename_ocn_tauy` | `"time"` | Time coordinate variable name in the NetCDF input. |  |
| `varname_ocn_tauy` | `"vw"` | Meridional wind-stress variable name. | Expected on the p-grid. |
| `Lcycle_ocn_tauy` | `"T"` | Use cyclic time interpolation. | `"T"` enables day-of-year cycling; other values use ordinary time interpolation. |
| `Tcycle_ocn_tauy` | `365.0` | Length of the forcing cycle. | days |

### `&tauy_io_ocn`

| Variable | Example value | Meaning | Units / notes |
|---|---|---|---|
| `fnames_ocn_tauy(1)` | `../PreProcessing/WIND/jra55do_vw_clm_eqpac_30.nc` | Meridional wind-stress file. | Variable selected by `varname_ocn_tauy`. |

### `&cn_param_ocn`

Phase-speed input settings for baroclinic modes.

| Variable | Example value | Meaning | Units / notes |
|---|---:|---|---|
| `nfile_ocn_cn` | `1` | Number of phase-speed files. | Determines the size of `fnames_ocn_cn`. |
| `timename_ocn_cn` | `"time"` | Time coordinate variable name in the NetCDF input. |  |
| `varname_ocn_cn` | `"cn"` | Phase-speed variable name. | Typically `m/s`; read as a 4-D field `(x, y, mode, time)`. |
| `Lcycle_ocn_cn` | `"T"` | Use cyclic time interpolation. | `"T"` enables day-of-year cycling. |
| `Tcycle_ocn_cn` | `365.0` | Length of the cycle. | days |

### `&cn_io_ocn`

| Variable | Example value | Meaning | Units / notes |
|---|---|---|---|
| `fnames_ocn_cn(1)` | `../PreProcessing/STRF/STRF_TLLL/WOA18_strf_ann_eqpac_30_mode5.nc` | NetCDF file containing `cn`. | The solver reads the `mode` dimension from this file. |

### `&obn_param_ocn`

Wind-projection coefficient input settings.  In the solver comments this is
used as the inverse wind-trapping coefficient for the modal forcing terms.

| Variable | Example value | Meaning | Units / notes |
|---|---:|---|---|
| `nfile_ocn_obn` | `1` | Number of `obn` files. | Determines the size of `fnames_ocn_obn`. |
| `timename_ocn_obn` | `"time"` | Time coordinate variable name in the NetCDF input. |  |
| `varname_ocn_obn` | `"obn"` | Wind-projection coefficient variable name. | Read as a 4-D field `(x, y, mode, time)`. |
| `Lcycle_ocn_obn` | `"T"` | Use cyclic time interpolation. | `"T"` enables day-of-year cycling. |
| `Tcycle_ocn_obn` | `365.0` | Length of the cycle. | days |

### `&obn_io_ocn`

| Variable | Example value | Meaning | Units / notes |
|---|---|---|---|
| `fnames_ocn_obn(1)` | `../PreProcessing/STRF/STRF_TLLL/WOA18_strf_ann_eqpac_30_mode5.nc` | NetCDF file containing `obn`. | Usually the same file as `cn`. |

### `&init`

| Variable | Example value | Meaning | Units / notes |
|---|---|---|---|
| `in_rst_flag` | `"F"` | Whether to initialize from a restart file. | `"T"` reads `fname_in_rst`; otherwise the model starts from zero initial fields. |
| `fname_in_rst` | `"restart_eqpac30.nc"` | Input restart file name. | Used only when `in_rst_flag="T"`. |

### `&output_hist`

History output contains instantaneous snapshots of `u`, `v`, `p`, `uw`, and
`vw`.

| Variable | Example value | Meaning | Units / notes |
|---|---:|---|---|
| `out_hist_flag` | `1` | Unit flag for the history interval. | `1` means days. |
| `out_hist_int` | `5` | History output interval. | 5 days in this example. |
| `fname_out_hist` | `../OUTPUTS/hist_eqpac30_jra55do_mode5_varcp.nc` | History output file. | Created at the start of the run. |

### `&output_avg`

Average output contains interval means of `u`, `v`, `p`, `uw`, and `vw`.

| Variable | Example value | Meaning | Units / notes |
|---|---:|---|---|
| `out_avg_flag` | `1` | Unit flag for the averaging/output interval. | `1` means days. |
| `out_avg_int` | `5` | Averaging/output interval. | 5-day averages in this example. |
| `fname_out_avg` | `../OUTPUTS/avg_eqpac30_jra55do_mode5_varcp.nc` | Average output file. | Created at the start of the run. |

### `&output_rst`

| Variable | Example value | Meaning | Units / notes |
|---|---|---|---|
| `out_rst_flag` | `"T"` | Whether to write a restart file at the end of the run. | `"T"` enables restart output. |
| `fname_out_rst` | `../OUTPUTS/restart_eqpac30_jra55do_mode5_varcp.nc` | Output restart file. | Written after the time loop when `out_rst_flag="T"`. |

### `&param_ocn`

Ocean model parameters are stored in the derived type `oset`.

| Variable | Example value | Meaning | Units / notes |
|---|---:|---|---|
| `oset%slip_ind` | `2.0` | Slip/no-slip control used by mask and boundary-condition routines. | The source comments state `0` gives slip-like `du/dx=0`; the boundary routine computes `gamma2 = 1 - 2*slip_ind`. |
| `oset%nu_h` | `2.0e4` | Horizontal viscosity. | `m^2/s` |
| `oset%A` | `2.0e-7` | Vertical mixing/damping parameter in the modal equations. | Used as `A / cn^2` in the modal momentum and pressure equations. |

Other `oset` fields have defaults in `CODES/run_types.f90`, including
`rho0=1024.0` and closed boundary-condition strings for p/u/v at each side.
They can also be set in `&param_ocn` if needed.

## Input File Shape Expectations

The solver reads 2-D time-dependent forcing fields with
`read_data_TLL_p`:

| Input type | Variables in this example | Expected dimensions |
|---|---|---|
| Zonal wind stress | `uw` | `(x, y, time)` |
| Meridional wind stress | `vw` | `(x, y, time)` |

The solver reads 3-D modal fields with `read_data_TLLL_p`:

| Input type | Variables in this example | Expected dimensions |
|---|---|---|
| Phase speed | `cn` | `(x, y, mode, time)` |
| Wind-projection coefficient | `obn` | `(x, y, mode, time)` |

The `time` variable must include a NetCDF `units` attribute in a form like
`days since YYYY-MM-DD HH:MM:SS`, because the solver converts input time to
days relative to the model start date.
