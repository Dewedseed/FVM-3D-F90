
# compile library core
set(LIBCORE core)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/${LIBCORE})

# compile library typedefs
set(LIBDEFS defs)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/${LIBDEFS})

## compile library std
set(LIBSTD std)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/${LIBSTD})

## compile library base
set(LIBBASE base)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/${LIBBASE})

## compile library scheme
set(LIBSCHEME scheme)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/${LIBSCHEME})

## compile library solve
set(LIBSOLVE solve)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/${LIBSOLVE})

## compile library process
set(LIBPROCESS process)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/${LIBPROCESS})

# compile executable
add_executable(${EXEC} solver_fvm_3d.f90)

target_link_libraries(${EXEC}
                      ${LIBCORE}
                      ${LIBDEFS}
                      ${LIBSTD}
                      ${LIBBASE}
                      ${LIBSOLVE}
                      ${LIBPROCESS}
                      ${MPI_Fortran_LIBRARIES}
                      ${CGNS_LIBRARY}
                      ${METIS_LIBRARY}
                      ${GKLIB_LIBRARY}
)
