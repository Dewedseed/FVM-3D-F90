
message(STATUS "Configure LIBBASE: ${LIBBASE}")

set(
  baseFiles
  interface_stdlib_array.f90
  interface_stdlib_logger.f90
  interface_stdlib_math.f90
  interface_stdlib_optval.f90
  interface_stdlib_string.f90
  interface_cgns.f90
  interface_cgns_write.f90
  interface_metis_c.f90
  interface_metis5_c.f90
  interface_metis.f90
  defs_fluid_props.f90
  defs_face_geom.f90
  defs_block_geom.f90
  defs_ghost_cell.f90
  defs_block_data.f90
  method_partition.f90
  method_argparse.f90
  method_namelist.f90
  method_gridgen.f90
  method_gridfile.f90
)

add_library(${LIBBASE} ${baseFiles})

message(STATUS "METIS_LIBRARY: ${METIS_LIBRARY}")
message(STATUS "GKLIB_LIBRARY: ${GKLIB_LIBRARY}")

target_link_libraries(${LIBBASE} PRIVATE
                      ${LIBCORE}
                      ${LIBSTD}
                      ${LIBDEFS}
                      ${CGNS_LIBRARY}
                      ${METIS_LIBRARY}
                      ${GKLIB_LIBRARY}
)
