## Makefile of Linearly continuosly stratified mode
#==========================
#Set compiler
FC  = ifort
#==========================
#Set compile option
NETCDF_INCDIR=/opt/local/build/netcdf4/netcdf-c-4.3.3.1_ifort_gcc/include
NETCDF_LIBDIR=/opt/local/build/netcdf4/netcdf-c-4.3.3.1_ifort_gcc/lib
FFLAGS += -I$(NETCDF_INCDIR)
LDFLAGS += -L$(NETCDF_LIBDIR) -lnetcdf -lnetcdff 
LDFLAGS += -fpp -O2 -qopenmp
#==========================
# You don't have to edit the following part
#==========================
TARGET=../RUN/exec_lcsm_vs.out
OBJ_ncdf  = ncdf_write.o
OBJ = run_param.o
OBJ_io  = calendar_sub.o \
input_files.o \
mod_hist.o \
mod_grid.o \
mod_avg.o \
mod_rst.o \
mod_wind.o \
mod_strf.o 
OBJ_ocn  = lcsm_param.o \
 lcsm_array.o \
 ocn_dyn.o
OBJ_main  = lcsm_main_vs.o

MODS  = ncdf_write.mod \
        calendar_sub.mod \
        input_files.mod \
        mod_hist.mod \
        mod_grid.mod \
        mod_avg.mod \
        mod_rst.mod \
        mod_wind.mod \
        mod_strf.mod \
        lcsm_param.mod \
	lcsm_array.mod \
	ocn_dyn.mod  \

all: $(TARGET)


$(TARGET): $(OBJ_ncdf) $(OBJ) $(OBJ_io) $(OBJ_ocn) $(OBJ_main) 
	$(FC) -o $@ $(OBJ_ncdf) $(OBJ) $(OBJ_io) $(OBJ_ocn)  $(OBJ_main) ${LDFLAGS} ${FFLAGS}

#Objects--------------------------------------
ncdf_write.o:ncdf_write.f90
	$(FC) -c $< ${LDFLAGS} ${FFLAGS}
calendar_sub.o:calendar_sub.f90
	$(FC) -c $< ${LDFLAGS} ${FFLAGS}
input_files.o:input_files.f90 calendar_sub.f90
	$(FC) -c $< ${LDFLAGS} ${FFLAGS}
mod_grid.o:mod_grid.f90 input_files.f90
	$(FC) -c $< ${LDFLAGS} ${FFLAGS}
mod_hist.o:mod_hist.f90
	$(FC) -c $< ${LDFLAGS} ${FFLAGS}
mod_avg.o:mod_avg.f90
	$(FC) -c $< ${LDFLAGS} ${FFLAGS}
mod_rst.o:mod_rst.f90
	$(FC) -c $< ${LDFLAGS} ${FFLAGS}
mod_wind.o:mod_wind.f90
	$(FC) -c $< ${LDFLAGS} ${FFLAGS}
mod_strf.o:mod_strf.f90 input_files.f90
	$(FC) -c $< ${LDFLAGS} ${FFLAGS}
run_param.o:run_param.f90
	$(FC) -c $< ${LDFLAGS} ${FFLAGS}
lcsm_param.o:lcsm_param.f90 run_param.f90
	$(FC) -c $< ${LDFLAGS} ${FFLAGS}
lcsm_array.o:lcsm_array.f90
	$(FC) -c $< ${LDFLAGS} ${FFLAGS}
ocn_dyn.o:ocn_dyn.f90
	$(FC) -c $< ${LDFLAGS} ${FFLAGS}
lcsm_main_vs.o:lcsm_main_vs.f90 lcsm_param.f90 run_param.f90
	$(FC) -c $< ${LDFLAGS} ${FFLAGS}
clean:
	rm -f $(TARGET_grid) $(TARGET) $(OBJ) $(OBJ_io) $(OBJ_grid) $(OBJ_ocn) $(OBJ_atm) $(OBJ_ncdf) $(MODS)
