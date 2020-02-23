module testing_subroutines
use fortran_logger_m
use penf
implicit none

contains

  subroutine debug_info(logger)
    class(fortran_logger),  intent(inout) :: logger

    call logger%debug(routine = 'debug_info', message = 'Entering subroutine')

    ! Do stuff
    call logger%info(routine = 'debug_info', message = 'Doing stuff')
    ! Do stuff

    call logger%debug(routine = 'debug_info', message = 'Exiting subroutine')

  end subroutine debug_info

  subroutine errored_subroutine(error)
    integer(I4P), intent(out) :: error

    error = 1

  end subroutine errored_subroutine


end module testing_subroutines