
message(STATUS "Configure LIBDEFS: ${LIBDEFS}")

set(
  defsFiles
  type_defs_bc.f90
  type_defs_bcdata.f90
  type_defs_dimension.f90
  type_defs_equation.f90
  type_defs_gas.f90
  type_defs_variables.f90
  type_defs_file.f90
  type_defs_scheme.f90
  global_type_defs.f90
)

add_library(${LIBDEFS} ${defsFiles})

# target_link_libraries(${LIBTYPEDEFS} PRIVATE
#                       # ${LIBCORE}
#                       # ${LIBSTD}
#                       # ${LIBBASE}
#                       # ${LIBSOLVE}
# )
