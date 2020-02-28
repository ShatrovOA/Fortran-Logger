module testing_subroutines
use fortran_logger_m
use penf
implicit none

contains

  subroutine debug_info(logger)
    class(fortran_logger),  intent(inout) :: logger

    call logger%debug(message = 'Entering subroutine', routine = 'debug_info')

    ! Do stuff
    call logger%info(message = 'Doing stuff', routine = 'debug_info')
    ! Do stuff

    call logger%debug(message = 'Exiting subroutine', routine = 'debug_info')

  end subroutine debug_info

  subroutine errored_subroutine(error)
    integer(I4P), intent(out) :: error

    error = -1

  end subroutine errored_subroutine


end module testing_subroutines