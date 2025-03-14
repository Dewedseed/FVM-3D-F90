!=============================================================================80
!
!> solver
!!   3D NS eqution
!!   Finite Volume Method
!!   Structural Grid
!
!=============================================================================80

program solver_fvm_3d

  use solver_sub, only : pre_process, solve_process, post_process

  implicit none

continue

  call pre_process
  call solve_process
  call post_process

end program solver_fvm_3d
