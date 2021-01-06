program run_tests
!< Program designed to test Fortran Logger
use iso_fortran_env, only : error_unit, output_unit
use fortran_logger, only: fortran_logger_t, LOGGER_DEBUG_LEVEL
use test_check_alloc_m, only: test_check_alloc
use test_ignore_error_codes_m, only: test_ignore_error_codes
use test_unit_change_m, only: test_unit_change
use test_check_json_m, only: test_check_json
use penf, only: I4P
#ifdef _MPI
  use mpi_f08
#endif
implicit none
  type(fortran_logger_t)  :: logger       !< Fortran logger class
  integer(I4P)            :: rank         !< MPI process number
  integer(I4P)            :: np           !< MPI number of processes
  integer(I4P)            :: null_unit    !< Unit connected to "/dev/null"
  integer(I4P)            :: out_unit     !< Unit used to print info and debug messages: stdout on rank = 0 and null_unit otherwise
  integer(I4P)            :: err_unit     !< Unit used to print error and warn messages: stderr on rank = 0 and null_unit otherwise
  
#ifdef _MPI
  ! MPI_Init must be called before initializing logger
  call MPI_Init()
  call MPI_Comm_rank(MPI_COMM_WORLD, rank)
  call MPI_Comm_size(MPI_COMM_WORLD, np)
#else
  rank = 0
  np = 1
#endif

  open( newunit = null_unit,    &
        file = '/dev/null',     &
        action = 'write')

  out_unit = null_unit; if(rank == 0) out_unit = output_unit
  err_unit = null_unit; if(rank == 0) err_unit = error_unit

  ! comm is the only required parameter. It must be passed when compiled with mpi support
  call logger%initialize(                                                                                                &
#ifdef _MPI
                          MPI_COMM_WORLD,                                                                                &
#endif
                          log_level = LOGGER_DEBUG_LEVEL,                                                                &
                          error_unit = err_unit, error_color_fg = 'red_intense', error_prefix = '[', error_suffix = ']', &
                          warn_unit = err_unit, warn_color_fg = 'yellow', warn_prefix = '[', warn_suffix = ']',          &
                          info_unit = out_unit, info_color_fg = 'green', info_prefix = '[', info_suffix = ']',           &
                          debug_unit = out_unit, debug_color_fg = 'cyan', debug_prefix = '[', debug_suffix = ']',        &
                          routine_prefix = '[', routine_suffix = ']', print_timestamp = .true.,                          &
                          timestamp_format = '%b %d %G %T', timestamp_prefix = '[', timestamp_suffix = ']')

  call logger%info('---------------------------------------------------------------')
  call logger%info('                   Fortran Logger Tests                        ')
  call logger%info('---------------------------------------------------------------')

  call test_check_alloc(logger)
#ifdef _MPI
  call MPI_Barrier(MPI_COMM_WORLD)
#endif
  call test_unit_change(logger, rank, np, null_unit)
#ifdef _MPI
  call MPI_Barrier(MPI_COMM_WORLD)
#endif
  call test_ignore_error_codes(logger)
#ifdef _MPI
  call MPI_Barrier(MPI_COMM_WORLD)
#endif
  call test_check_json(logger, rank)
#ifdef _MPI
  call MPI_Barrier(MPI_COMM_WORLD)
#endif

  call logger%info('---------------------------------------------------------------')
  call logger%info('                   All Tests passed!                           ')
  call logger%info('---------------------------------------------------------------')
  close(null_unit)
  call logger%finalize()

#ifdef _MPI
  call MPI_Finalize()
#endif
end program run_tests