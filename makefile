F77 = /usr/bin/f77
FFLAGS = -fno-second-underscore
INCLUDE = /usr/local/netcdf-3.6.2/fortran
LIBS = /usr/local/netcdf-3.6.2/libsrc/.libs/libnetcdf.a
TARGET = conv2netcdf
SRC = write_netcdf.f

all: $(SRC)
	$(F77) $(FFLAGS) -I$(INCLUDE) $(SRC) $(LIBS) -o $(TARGET)
