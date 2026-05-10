## Makefile of T/S files
#==========================
#Set compiler
FC  = gfortran
NETCDF_INCDIR=-I/opt/homebrew/Cellar/netcdf-fortran/4.6.1/include -I/opt/homebrew/Cellar/netcdf-fortran/4.6.1/include
NETCDF_LIBDIR=-L/opt/homebrew/Cellar/netcdf-fortran/4.6.1/lib -L/opt/homebrew/Cellar/netcdf/4.9.2_2/lib
# NETCDF_INCDIR=/opt/local/build/netcdf4/netcdf-c-4.3.3.1_ifort_gcc/include
# NETCDF_LIBDIR=/opt/local/build/netcdf4/netcdf-c-4.3.3.1_ifort_gcc/lib
FFLAGS += $(NETCDF_INCDIR) 
LDFLAGS += $(NETCDF_LIBDIR) -lnetcdf -lnetcdff -llapack

TARGET_DENS=exec_get_pdens.out
TARGET_FLOOD=exec_do_flood.out
TARGET_DECOMP=exec_get_vmode_TLLL.out
OBJ_NCDF  = ncdf_write.o \
ncdf_read.o
OBJ_LAPACK = lapsub.o
OBJ_DENS  = get_pdens_LCSMgrid.o
OBJ_FLOOD  = do_flood_LCSM.o
OBJ_DECOMP=get_vmode_TLLL.o
MODES= ncdf_write.mod \
ncdf_read.mod \
lapsub.mod
all: $(TARGET_DENS) $(TARGET_FLOOD) $(TARGET_DECOMP)

$(TARGET_DENS): $(OBJ_NCDF) $(OBJ_DENS)
	$(FC) -o $@ $(OBJ_NCDF) $(OBJ_DENS) ${LDFLAGS} ${FFLAGS}
$(TARGET_FLOOD): $(OBJ_NCDF) $(OBJ_FLOOD)
	$(FC) -o $@ $(OBJ_NCDF) $(OBJ_FLOOD) ${LDFLAGS} ${FFLAGS}
$(TARGET_DECOMP): $(OBJ_NCDF) $(OBJ_LAPACK)  $(OBJ_DECOMP)
	$(FC) -o $@ $(OBJ_NCDF) $(OBJ_LAPACK) $(OBJ_DECOMP) ${LDFLAGS} ${FFLAGS}

#Objects--------------------------------------
ncdf_read.o:ncdf_read.f90
	$(FC) -c $< ${LDFLAGS} ${FFLAGS}
ncdf_write.o:ncdf_write.f90
	$(FC) -c $< ${LDFLAGS} ${FFLAGS}
lapsub.o:lapsub.f90
	$(FC) -c $< ${LDFLAGS} ${FFLAGS}
get_pdens_LCSMgrid.o: get_pdens_LCSMgrid.f90
	$(FC) -c $< ${LDFLAGS} ${FFLAGS}
do_flood_LCSM.o:do_flood_LCSM.f90
	$(FC) -c $< ${LDFLAGS} ${FFLAGS}
get_vmode_TLLL.o:get_vmode_TLLL.f90
	$(FC) -c $< ${LDFLAGS} ${FFLAGS}
clean:
	rm -f $(TARGET_DENS) $(TARGET_FLOOD) $(TARGET_DECOMP) $(OBJ_NCDF) $(OBJ_LAPACK) $(OBJ_DENS) $(OBJ_FLOOD) $(OBJ_DECOMP) $(MODS)
