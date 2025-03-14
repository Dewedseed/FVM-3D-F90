
message(STATUS "Configure LIBCORE: ${LIBCORE}")

set(
  coreFiles
  kind_parameter.f90
  physical_constants.f90
  parallel_mpi.f90
  # parallel_openmp.f90
  global_devlog.f90
  global_file.f90
  global_error.f90
  global_terminal.f90
  global_uuid.f90
  container_linklist.f90
)

add_library(${LIBCORE} ${coreFiles})

target_link_libraries(${LIBCORE} PRIVATE
                      ${MPI_Fortran_LIBRARIES}
)
