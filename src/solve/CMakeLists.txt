
message(STATUS "Configure LIBSOLVE: ${LIBSOLVE}")

set(
  solveFiles
  global_class.f90
  class_solver_info.f90
  class_case.f90
  class_equation.f90
  class_iteration.f90
  class_geometry.f90
  class_flow_field.f90
  class_space_discret.f90
  class_time_discret.f90
  class_solver.f90
  class_output.f90
  class_input.f90
)

add_library(${LIBSOLVE} ${solveFiles})

target_link_libraries(${LIBSOLVE} PRIVATE
                      ${LIBCORE}
                      ${LIBDEFS}
                      ${LIBBASE}
                      ${LIBSCHEME}
)
