## Makefile of T/S files
#==========================
#Set compiler
FC  = gfortran
NETCDF_INCDIR=-I/opt/homebrew/Cellar/netcdf-fortran/4.6.1/include -I/opt/homebrew/Cellar/netcdf-fortran/4.6.1/include
NETCDF_LIBDIR=-L/opt/homebrew/Cellar/netcdf-fortran/4.6.1/lib -L/opt/homebrew/Cellar/netcdf/4.9.2_2/lib
FFLAGS += $(NETCDF_INCDIR)
LDFLAGS += $(NETCDF_LIBDIR) -lnetcdf -lnetcdff

TARGET_CONV=exec_convert_LCSM_to_zlev.out
OBJ_NCDF  = ncdf_write.o \
ncdf_read.o
OBJ_CONV  = convert_LCSM_to_zlev.o

MODS= ncdf_read.mod \
ncdf_write.mod

all: $(TARGET_CONV)

$(TARGET_CONV): $(OBJ_NCDF) $(OBJ_CONV)
	$(FC) -o $@ $(OBJ_NCDF) $(OBJ_CONV) ${LDFLAGS} ${FFLAGS}

#Objects--------------------------------------
ncdf_read.o:ncdf_read.f90
	$(FC) -c $< ${LDFLAGS} ${FFLAGS}
ncdf_write.o:ncdf_write.f90
	$(FC) -c $< ${LDFLAGS} ${FFLAGS}
convert_LCSM_to_zlev.o:convert_LCSM_to_zlev.f90
	$(FC) -c $< ${LDFLAGS} ${FFLAGS}
clean:
	rm -f $(TARGET_CONV) $(OBJ_NCDF) $(OBJ_CONV) $(MODS)
