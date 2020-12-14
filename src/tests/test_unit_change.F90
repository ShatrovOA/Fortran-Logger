program sample
use fortran_logger, only: fortran_logger_t, LOGGER_DEBUG_LEVEL
use penf
use mpi_f08
use iso_fortran_env, only: output_unit
implicit none
  type(fortran_logger_t) :: logger
  integer(I4P) :: rank,out_unit,null_unit  
  
  call MPI_Init()
  call MPI_Comm_rank(MPI_COMM_WORLD, rank)

  open( newunit = null_unit,     &
        status='replace',  &
          file = '/dev/null',    &
          action = 'write')

  if(rank == 0) then 
    out_unit = output_unit
  else
    out_unit = null_unit
  endif
  close(null_unit)

  call logger%initialize(MPI_COMM_WORLD, log_level = LOGGER_DEBUG_LEVEL, debug_unit = out_unit)

  call logger%debug("Hello from rank "//trim(str(n=rank,no_sign=.true.)))

  out_unit = output_unit
  call logger%change_unit(LOGGER_DEBUG_LEVEL, out_unit)
  call logger%debug("This Hello from rank "//trim(str(n=rank,no_sign=.true.)))

  call logger%finalize()

  call MPI_Finalize()
  
end program sample