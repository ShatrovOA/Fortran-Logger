program test_alloc
use fortran_logger
use penf
use mpi_f08
use iso_fortran_env, only: error_unit, output_unit
implicit none
  type(fortran_logger_t) :: logger
  complex(R4P), allocatable :: complex_rank_1_R4P(:)
  complex(R8P), allocatable :: complex_rank_1_R8P(:)
  complex(R16P), allocatable :: complex_rank_1_R16P(:)
  integer(I4P) :: rank
  integer(I4P) :: out_unit, err_unit, null_unit

  
  call MPI_Init()
  call MPI_Comm_rank(MPI_COMM_WORLD, rank)

  open( newunit = null_unit,     &
        status='replace',  &
          file = '/dev/null',    &
          action = 'write')

  if(rank == 0) then 
    out_unit = output_unit
    err_unit = error_unit
  else
    out_unit = null_unit
    err_unit = null_unit
  endif

  ! close(null_unit)
  call logger%initialize(MPI_COMM_WORLD, log_level = LOGGER_DEBUG_LEVEL, &
                         error_unit = err_unit, error_color_fg = 'red', error_style = 'italics_on', error_prefix = '[ ', error_suffix = ' ]', &
                         warn_unit = err_unit, warn_color_fg = 'orange', warn_style = 'italics_on', warn_prefix = '<', warn_suffix = '>', &
                         info_unit = out_unit, info_color_fg = 'green', info_style = 'framed_on', info_prefix = '{', info_suffix = '}',&
                         debug_unit = out_unit, debug_color_fg = 'yellow', debug_style = 'underline_on', debug_prefix = '--', debug_suffix = '--',&
                         print_timestamp = .true., timestamp_format = '%b %d %G %T')
  
  call logger%debug('Allocating complex_rank_1_R4P...')
  allocate(complex_rank_1_R4P(20))
  call logger%check_alloc(complex_rank_1_R4P, 'complex_rank_1_R4P')

  call logger%debug('Allocating complex_rank_1_R8P...')
  allocate(complex_rank_1_R8P(20))
  call logger%check_alloc(complex_rank_1_R8P, 'complex_rank_1_R8P')

  call logger%debug('Allocating complex_rank_1_R16P...')
  if(rank < 3) then 
    allocate(complex_rank_1_R16P(20))
  endif
  call logger%check_alloc(complex_rank_1_R16P, 'complex_rank_1_R16P')
end program test_alloc