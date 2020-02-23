program test1
use fortran_logger_m, only : fortran_logger
use testing_subroutines
use penf
use iso_fortran_env, only : error_unit, output_unit
  implicit none
  type(fortran_logger) :: logger
  integer(I4P) :: error

  call logger%initialize(log_level = 4, print_timestamp = .true.)

  call debug_info(logger)


  ! call logger%warn(routine = 'test_routine 1', message = 'you cant do that')

  ! call logger%error(routine = 'test_routine 1', message = 'error happened', is_fatal = .true.)

  ! Calling external subroutine, which generates non-zero error code
  call errored_subroutine(error)
  call logger%check_error(check_routine = 'errored_subroutine', err = error)

  ! Let's call the same subroutine, but this time non-zero code will be fatal
  call errored_subroutine(error)
  call logger%check_error(check_routine = 'fatal_errored_subroutine', err = error, is_fatal = .true.)

  ! call logger%check_alloc(routine='test_routine 1', check = ar, check_name = 'allocate check arrays')

  call logger%info(message = 'Finished')

  ! call logger%check_error(routine = 'test_routine 1', check_routine = 'MPI_Init', err = -32, is_fatal = .true.)

  call logger%finalize()

  ! close(stderr)
  ! close(stdout)

  ! call MPI_Finalize()
    
end program test1