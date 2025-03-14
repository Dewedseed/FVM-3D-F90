!=============================================================================80
!
!> Interface to stdlib logger
!
!  https://stdlib.fortran-lang.org/page/specs/stdlib_logger.html
!
!=============================================================================80

module interface_stdlib_logger

  use stdlib_logger, only : logger_type, global_logger

  use stdlib_logger, only : success, &
                            close_failure, &
                            index_invalid_error, &
                            non_sequential_error, &
                            open_failure, &
                            read_only_error, &
                            unformatted_in_error, &
                            unopened_in_error, &
                            write_failure

  use stdlib_logger, only : debug_level, &
                            information_level, &
                            warning_level, &
                            error_level, &
                            io_error_level, &
                            text_error_level, &
                            none_level

  implicit none

!...Define scope
  private

  public  :: global_logger

  public  :: success, &
            close_failure, &
            index_invalid_error, &
            non_sequential_error, &
            open_failure, &
            read_only_error, &
            unformatted_in_error, &
            unopened_in_error, &
            write_failure

  public  :: debug_level, &
            information_level, &
            warning_level, &
            error_level, &
            io_error_level, &
            text_error_level, &
            none_level

!...Declare local variables

contains

end module interface_stdlib_logger
