!=============================================================================80
!
!> Subroutine of solver
!
!=============================================================================80

module solver_sub

!...Use modules
  use global_type_defs, only : TypeDefs

  use global_class,   only : ParallelMPI, mpi
  use global_class,   only : ClassTerminal, terminal
  use global_devlog,  only : ClassDevLog, devlog

  use class_equation, only : ClassPhysicalSystem
  use class_solver,   only : ClassSolver
  use class_output,   only : ClassOutput
  use class_input,    only : ClassInput

  implicit none

!...Define scope
  private

  public  :: pre_process
  public  :: solve_process
  public  :: post_process

!...Declare local variables
  type(ClassInput)          :: input
  type(ClassSolver)         :: solver
  type(ClassOutput)         :: output
  type(ClassPhysicalSystem) :: system

contains

!=================================== PRE_PROCESS =============================80
!
!> Solver pre-processing
!!   1. Get argument command
!!   2. Read case file
!!   3. Read mesh file
!!   4. Assembly equation system
!!   5. MPI initialization and synchronous
!!   6. Initialize solver (allocate memory)
!!   7. Dimensionless
!!   8. Output information for user
!!   9. Output intial data
!!
!
!=============================================================================80

  subroutine pre_process()
  !...Use module

  !...Declare input/output variables

  !...Declare local variables
    integer :: terminal_unit = 6

  continue

  !...Instantiate mpi component
    mpi = ParallelMPI(terminal_unit)

    call mpi%start()

  !...Open terminal
    terminal = ClassTerminal(mpi%is_host())

  !...Instantiate component
    solver = ClassSolver()
    input  = ClassInput()
    output = ClassOutput()
    system = ClassPhysicalSystem()

  !...Print Solver information
    call terminal%print()
    call terminal%print()
    call terminal%print("Solver:  " // trim(solver%info%name))
    call terminal%print("Version: " // trim(solver%info%version))

  !...Begin pre-process
    call terminal%print()
    call terminal%print("===================== PRE-PROCESS =====================")
    call terminal%print()
    call terminal%flush()

    if ( mpi%is_host() ) then
    !...Get and parse argument
      call input%parse_arguments(solver, devlog)
      call terminal%flush()

    !...Start log for developer
      call devlog%start()

    !...Get and parse case file
      call input%read_casefile(solver, output)
      call input%check_casefile(solver, output)
      call terminal%flush()

    !...Get mesh
      select case (trim(solver%geometry%type))
        case ("gridfile")
          ! call solver%geometry%gird_read()
        case ("uniform")
          call solver%geometry%grid_generate()
      end select
      call terminal%flush()

    !...Split geometry for parallel computation
      if (mpi%nprocess() > 1) then
        call solver%geometry%grid_partition()
      end if
      call terminal%flush()

    else
      call mpi%barrier()

    end if

  !...Sync
    ! call solver%geometry%sync_data_mpi()

  !...Construct physical system
    call terminal%print()
    call terminal%print(">> Construct physical system...")
    call terminal%print()

    if ( solver%flowfield%eqn_type == TypeDefs%Equation%Euler ) then
      call system%add_equation("mass")
      call system%add_equation("momentumX", "inviscid")
      call system%add_equation("momentumY", "inviscid")
      call system%add_equation("momentumZ", "inviscid")
      call system%add_equation("energy",    "inviscid")
    else if ( solver%flowfield%eqn_type == TypeDefs%Equation%NS ) then
      call system%add_equation("mass")
      call system%add_equation("momentumX")
      call system%add_equation("momentumY")
      call system%add_equation("momentumZ")
      call system%add_equation("energy")
    end if

  !...Allocate memory
    call solver%flowfield%allocate_memory(system, solver%geometry)
    call solver%iteration%allocate_memory(system)

  !...Dimensionless
    if (.not. solver%case%dimensionless) call solver%flowfield%dimensionless()

  !...Initialize
    call solver%flowfield%initial()

  !...Output initial flow field
    call output%initial(solver%case, solver%geometry)
    call output%flowfield(flowfield=solver%flowfield)

  !...End Process
    call terminal%print()
    call terminal%flush()

    return

  end subroutine pre_process

!================================= SOLVE_PROCESS =============================80
!
!> Solve process
!! 1. Solve equation system
!! 2. MPI synchronous
!! 3. Output flow field
!! 4. Record calculation time
!
!=============================================================================80

  subroutine solve_process()
  !...Use module

  !...Declare input/output variables

  !...Declare local variables

  continue

  !...Begin solve-process
    call terminal%print()
    call terminal%print("===================== SOLVE-PROCESS ===================")
    call terminal%print()

  !...Start time
    call output%start_time()
    call output%residual(solver%iteration)

  !...Solve
    do while (.not. solver%iteration%converged())
      solver%iteration%step = solver%iteration%step + 1
      call solver%timeDiscr%solve(          &
              geometry=solver%geometry,     &
              flowfield=solver%flowfield,   &
              spaceDiscr=solver%spaceDiscr, &
              iteration=solver%iteration    &
            )
      call output%residual(solver%iteration)
      call output%flowfield(flowfield=solver%flowfield, &
                            timestep=solver%iteration%step)
    end do

  !...End time
    call output%end_time()

  !...End Process
    call terminal%print()

    return

  end subroutine solve_process


!================================= POST_PROCESS ==============================80
!
!> Post-process
!! 1. Close output file
!! 2. MPI end
!! 3. Output calculation information
!
!=============================================================================80

  subroutine post_process()
  !...Use module

  !...Declare input/output variables

  !...Declare local variables

  continue

  !...Begin post-process
    call terminal%print()
    call terminal%print("===================== POST-PROCESS ====================")
    call terminal%print()

  !...Close output file
    call output%finish()

  !...Close log file
    call devlog%end()

  !...End Process
    call terminal%print("========================= END =========================")
    call terminal%print()
    call terminal%print()

  !...MPI end
    call mpi%end()

    return

  end subroutine post_process

end module solver_sub
