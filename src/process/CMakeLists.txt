
message(STATUS "Configure LIBPROCESS: ${LIBPROCESS}")

set(
  processFiles
  solver_sub.f90
)

add_library(${LIBPROCESS} ${processFiles})

target_link_libraries(${LIBPROCESS} PRIVATE
                      # ${LIBCORE}
                      # ${LIBSTD}
                      # ${LIBBASE}
                      ${LIBSOLVE}
)
