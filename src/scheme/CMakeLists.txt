
message(STATUS "Configure LIBSCHEME: ${LIBSCHEME}")

set(
  schemeFiles
  scheme_Runge_Kutta.f90
  scheme_reconstruct_simple.f90
  scheme_reconstruct_MUSCL.f90
  scheme_FDS_Roe.f90
  scheme_FVS_VanLeer.f90
  scheme_WENO.f90
  scheme_AUSM.f90
  scheme_gradient_finite_diff.f90
  scheme_gradient_Green.f90
  numerical_scheme.f90
)

add_library(${LIBSCHEME} ${schemeFiles})

target_link_libraries(${LIBSCHEME} PRIVATE
                      ${LIBCORE}
                      ${LIBBASE}
)
