program test1
use fortran_logger_m, only : fortran_logger
use mpi_f08
use penf
use iso_fortran_env, only : error_unit, output_unit
  implicit none
  type(fortran_logger) :: logger
  integer(I4P) :: rank, stdout, stderr, ierror
  integer(I4P), allocatable :: ar(:)

  call MPI_Init()

  ierror = 0

  ! call MPI_Comm_rank(MPI_COMM_WORLD, rank)

  ! if(rank == 0) then
  !   stderr = error_unit
  !   stdout = output_unit
  !   ! allocate(ar(32))
  ! else
  !   open(newunit = stderr, status = 'scratch', action = 'write')
  !   open(newunit = stdout, status = 'scratch', action = 'write')
  ! endif


  call logger%initialize(log_level = 4, print_timestamp = .true.)

  call logger%mpi_init()

  call logger%debug(routine = 'test_routine 1', message = 'enter')

  call logger%info(routine = 'test_routine 1', message = 'doing some stuff')

  call logger%warn(routine = 'test_routine 1', message = 'you cant do that')

  ! call logger%error(routine = 'test_routine 1', message = 'error happened', is_fatal = .true.)
  ierror = -1
  call logger%check_error(check_routine = 'external subroutine', err = ierror, is_fatal = .true.)

  ! call logger%check_alloc(routine='test_routine 1', check = ar, check_name = 'allocate check arrays')

  call logger%info(message = 'Finished')

  ! call logger%check_error(routine = 'test_routine 1', check_routine = 'MPI_Init', err = -32, is_fatal = .true.)

  call logger%finalize()

  ! close(stderr)
  ! close(stdout)

  ! call MPI_Finalize()
    
end program test1