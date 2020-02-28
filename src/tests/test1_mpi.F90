program test1
use fortran_logger_m, only : fortran_logger
use testing_subroutines
use penf
use mpi_f08
use iso_fortran_env, only : error_unit, output_unit
implicit none
  type(fortran_logger) :: logger
  integer(I4P) :: error

  ! MPI_Init must be called before logger%initialize. Otherwise runtime error will occur
  call MPI_Init()

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

  call MPI_Finalize()
        
end program test1