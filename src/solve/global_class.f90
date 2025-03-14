!=============================================================================80
!
!> Gather global classes
!
!=============================================================================80

module global_class

  use global_error
  ! use parallel_openmp
  use parallel_mpi

  use interface_stdlib_string
  ! use interface_stdlib_optval

  use interface_cgns

  use global_file
  use global_terminal

  use method_argparse
  use method_namelist

  implicit none

  public

!...Core classes
  type(ClassError)         :: error
  ! type(ParallelOpenMP)     :: openmp
  type(ParallelMPI)        :: mpi
  type(ClassTerminal)      :: terminal

!...Base method
  type(ClassStringMethod)   :: string
  ! type(ClassOptionalMethod) :: optional

end module global_class
