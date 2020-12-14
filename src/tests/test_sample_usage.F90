program sample
use fortran_logger, only: fortran_logger_t, LOGGER_DEBUG_LEVEL
use penf
use mpi_f08
implicit none
  type(fortran_logger_t) :: logger
  integer(I4P) :: i, j, k
  integer(I8P) :: error_I8P
  
  call MPI_Init()
  call logger%initialize(MPI_COMM_WORLD, log_level = LOGGER_DEBUG_LEVEL)

  call logger%info("Logger initialization finished!")
  call logger%debug("Entering loop")

  do i = 1, 200
    do j = 50, 500
      k = i + j 
    enddo
  enddo
  call logger%debug("Exiting loop")

  call errored_subroutine_I8P(error_I8P)
  call logger%check_error('errored_subroutine_I8P', error_I8P)

  call logger%debug("Ignoring error code -2 and calling errored_subroutine_I8P again..")
  call logger%ignore_error_codes([-2])
  call errored_subroutine_I8P(error_I8P)
  call logger%check_error('errored_subroutine_I8P_ignored_code', error_I8P)
  call logger%debug("logger%check_error did not print anything...")
  call logger%error('-2 is still an error, you should not ignore it..')

  call logger%finalize()
contains

  subroutine errored_subroutine_I8P(error)
    integer(I8P) :: error
    error = -2
  end subroutine errored_subroutine_I8P
end program sample