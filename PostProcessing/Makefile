## Makefile of T/S files
#==========================
#Set compiler
FC  = ifort
NETCDF_INCDIR=/opt/local/build/netcdf4/netcdf-c-4.3.3.1_ifort_gcc/include
NETCDF_LIBDIR=/opt/local/build/netcdf4/netcdf-c-4.3.3.1_ifort_gcc/lib
FFLAGS += -I$(NETCDF_INCDIR) 
LDFLAGS += -L$(NETCDF_LIBDIR) -lnetcdf -lnetcdff

TARGET_CONV=exec_convert_to_z_4d.out
OBJ_NCDF  = ncdf_write.o \
ncdf_read.o
OBJ_CONV  = convert_to_z_4d.o

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
convert_to_z_4d.o:convert_to_z_4d.f90
	$(FC) -c $< ${LDFLAGS} ${FFLAGS}
clean:
	rm -f $(TARGET_CONV) $(OBJ_NCDF) $(OBJ_CONV) $(MODS)
