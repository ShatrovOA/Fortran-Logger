program test1
use fortran_logger
use testing_subroutines
use penf
use iso_fortran_env, only : error_unit, output_unit
implicit none
  type(fortran_logger_t) :: logger
  integer(I4P) :: error

  call logger%initialize(log_level = 4, print_timestamp = .true.)

  call debug_info(logger)

  ! Calling external subroutine, which generates non-zero error code
  call errored_subroutine(error)
  call logger%check_error(check_routine = 'errored_subroutine', err = error)

  ! Let's call the same subroutine, but this time non-zero code will be fatal
  call errored_subroutine(error)
  call logger%check_error(check_routine = 'fatal_errored_subroutine', err = error, is_fatal = .true.)

  call logger%info(message = 'Finished')

  call logger%finalize()

  sync all
    
end program test1