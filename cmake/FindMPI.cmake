# set(CMAKE_FIND_LIBRARY_SUFFIXES .a ${CMAKE_FIND_LIBRARY_SUFFIXES})

set(OPENMPI_ROOT ${PROJECT_SOURCE_DIR}/ext/openmpi-5.0.6)

find_path(
  MPI_Fortran_INCLUDE_PATH
  NAMES mpif.h
  PATHS ${OPENMPI_ROOT}/include
)

find_library(
  MPI_Fortran_LIBRARIES
  NAMES mpi_mpifh
  PATHS ${OPENMPI_ROOT}/lib
  NO_DEFAULT_PATH
)

if(MPI_Fortran_INCLUDE_PATH AND MPI_Fortran_LIBRARIES)
  set(MPI_FOUND TRUE)
  set(MPI_Fortran_LIBRARIES ${MPI_Fortran_LIBRARIES})
  set(MPI_Fortran_INCLUDE_PATH ${MPI_Fortran_INCLUDE_PATH})
else()
  set(MPI_FOUND FALSE)
endif()
