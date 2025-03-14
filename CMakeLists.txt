cmake_minimum_required(VERSION 3.14.0)

# REF: https://github.com/fortran-lang/stdlib/blob/master/CMakeLists.txt
# 	   https://github.com/nasa/CFL3D

project(solver_fvm_3d
        LANGUAGES Fortran
        DESCRIPTION "Finite Volume Method 3D CFD Solver")

# Set compile command
# makefile:
#   .f90.o:
#           $(FTN) $(CPP) $(FFLAGS) -c $*.f90
#   $(EXEC): $(FOBJ_MAIN) $(DIST_LIB) $(EXT_LIB)
#           $(FTN) $(CPP) $(LFLAG) -o $(EXEC) *.o $(LLIBS)
#
#   FTN = gfortran
#   CPP = $(CPPFLAG) $(CPPOPT)
#   CPPFLAG = -cpp -P
#   CPPOPT = -DTEST
#   FFLAGS = $(FFLAGS) $(PREC)
#   LFLAG = $(FFLAGS) $(PREC)
#   FFLAGS = -O2 -w -march=native
#   PREC = -fdefault-real-8
#   LFLAG = -march=native -ffpe-trap=zero
#   EXEC = $(EXE_SOLVER)
#
# TODO: add preprocessor command
# choose compile release or debug
set(CMAKE_Fortran_COMPILER gfortran)

set(CMAKE_EXE_LINKER_FLAGS "-march=native -ffpe-trap=zero")

message(STATUS "CMAKE_BUILD_TYPE: ${CMAKE_BUILD_TYPE}")

if(CMAKE_BUILD_TYPE MATCHES Release)
    # default: -O3
    set(
        CMAKE_Fortran_FLAGS_INIT
    )
    set(
        CMAKE_Fortran_FLAGS_RELEASE_INIT
        "-w"
        "-march=native"
        "-fdefault-real-8"
        "-fallow-argument-mismatch"
    )
    set(
        CMAKE_Fortran_FLAGS_DEBUG_INIT
    )
    set(EXEC solver)

else(CMAKE_BUILD_TYPE MATCHES Debug)
    set(
        CMAKE_Fortran_FLAGS_INIT
    )
    set(
        CMAKE_Fortran_FLAGS_RELEASE_INIT
    )
    # default: -g
    set(
        CMAKE_Fortran_FLAGS_DEBUG_INIT
        "-O0"
        "-Wall"
        "-Wextra"
        "-Wimplicit-procedure"
    )
    set(EXEC solver_debug)

endif()
string(REPLACE ";" " " CMAKE_Fortran_FLAGS_INIT "${CMAKE_Fortran_FLAGS_INIT}")
string(REPLACE ";" " " CMAKE_Fortran_FLAGS_RELEASE_INIT "${CMAKE_Fortran_FLAGS_RELEASE_INIT}")
string(REPLACE ";" " " CMAKE_Fortran_FLAGS_DEBUG_INIT "${CMAKE_Fortran_FLAGS_DEBUG_INIT}")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${CMAKE_Fortran_FLAGS_INIT}")
set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} ${CMAKE_Fortran_FLAGS_RELEASE_INIT}")
set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${CMAKE_Fortran_FLAGS_DEBUG_INIT}")
message(STATUS "CMAKE_Fortran_FLAGS: ${CMAKE_Fortran_FLAGS}")
message(STATUS "CMAKE_Fortran_FLAGS_RELEASE: ${CMAKE_Fortran_FLAGS_RELEASE}")
message(STATUS "CMAKE_Fortran_FLAGS_DEBUG: ${CMAKE_Fortran_FLAGS_DEBUG}")

# set module files path
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)

# import external libraries
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)
## HDF5 library
# find_package(HDF5 REQUIRED)
# if(HDF5_FOUND)
#     include_directories(${HDF5_INCLUDE_DIR})
#     set(LIBEXT ${LIBEXT} ${HDF5_LIBRARY})
#     message(STATUS "HDF5 library: ${HDF5_LIBRARY}")
#     message(STATUS "HDF5 include: ${HDF5_INCLUDE_DIR}")
# else()
#     message(FATAL_ERROR "HDF5 library not found")
# endif()
## CGNS library
find_package(CGNS REQUIRED)
if(CGNS_FOUND)
    include_directories(${CGNS_INCLUDE_DIR})
    message(STATUS "CGNS library: ${CGNS_LIBRARY}")
    message(STATUS "CGNS include: ${CGNS_INCLUDE_DIR}")
else()
    message(FATAL_ERROR "CGNS library not found")
endif()
## OpenMPI library
## https://cmake.org/cmake/help/latest/module/FindMPI.html
find_package(MPI REQUIRED)
if(MPI_FOUND)
    include_directories(${MPI_Fortran_INCLUDE_PATH})
    message(STATUS "OpenMPI library: ${MPI_Fortran_LIBRARIES}")
    message(STATUS "OpenMPI include: ${MPI_Fortran_INCLUDE_PATH}")
else()
    message(FATAL_ERROR "OpenMPI library not found")
endif()
## Metis library
find_package(GKLIB REQUIRED)
if(GKLIB_FOUND)
    include_directories(${GKLIB_INCLUDE_PATH})
    message(STATUS "GKLIB library: ${GKLIB_LIBRARY}")
    message(STATUS "GKLIB include: ${GKLIB_INCLUDE_PATH}")
else()
    message(FATAL_ERROR "GKLib library not found")
endif()
find_package(METIS REQUIRED)
if(METIS_FOUND)
    include_directories(${METIS_INCLUDE_PATH})
    message(STATUS "METIS library: ${METIS_LIBRARY}")
    message(STATUS "METIS include: ${METIS_INCLUDE_PATH}")
else()
    message(FATAL_ERROR "METIS library not found")
endif()

# Read version form file
file(STRINGS "${PROJECT_SOURCE_DIR}/VERSION" PROJECT_VERSION)
string(REPLACE "." ";" VERSION_LIST ${PROJECT_VERSION})
list(GET VERSION_LIST 0 PROJECT_VERSION_MAJOR)
list(GET VERSION_LIST 1 PROJECT_VERSION_MINOR)
list(GET VERSION_LIST 2 PROJECT_VERSION_PATCH)
unset(VERSION_LIST)

# test framework
# include(CTest)

# Add subdirectories
add_subdirectory(${PROJECT_SOURCE_DIR}/src)

# Install executable
install(TARGETS ${EXEC}
        RUNTIME DESTINATION "${PROJECT_SOURCE_DIR}/bin"
)
