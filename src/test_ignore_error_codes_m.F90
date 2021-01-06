module test_ignore_error_codes_m
!< Module to test check_error and ignore_error_codes generic methods of Fortran Logger
use fortran_logger, only : fortran_logger_t
use penf, only: I1P, I2P, I4P, I8P, R4P, R8P, str
#ifdef _R16P
use penf, only: R16P
#endif
implicit none
private
public :: test_ignore_error_codes

  interface errored_subroutine
  !< Generic external subroutines that return non-zero error codes
    module procedure :: errored_subroutine_I1P
    module procedure :: errored_subroutine_I2P
    module procedure :: errored_subroutine_I4P
    module procedure :: errored_subroutine_I8P
    module procedure :: errored_subroutine_LOGICAL
  end interface errored_subroutine

contains

  subroutine test_ignore_error_codes(logger)
  !< Tester subroutine
    class(fortran_logger_t),  intent(inout) :: logger               !< Fortran logger class
    integer(I1P)                            :: error_code_I1P       !< Error code 1 byte
    integer(I2P)                            :: error_code_I2P       !< Error code 2 bytes
    integer(I4P)                            :: error_code_I4P       !< Error code 4 bytes
    integer(I8P)                            :: error_code_I8P       !< Error code 8 bytes
    logical                                 :: error_code_LOGICAL   !< Logical error flag
    character(len=:),         allocatable   :: this                 !< This subroutine name

    call logger%info('***************************************************************')
    call logger%info('               Testing "ignore error codes" methods            ')
    call logger%info('***************************************************************')

    this = 'test_ignore_error_codes'

    call errored_subroutine(logger, error_code_I1P)
    call logger%check_error('errored_subroutine_I1P', error_code_I1P, routine=this, is_fatal=.false.)
    call logger%ignore_error_codes([error_code_I1P])
    call logger%warn('Ignoring code: '//trim(str(error_code_I1P)), routine=this)
    call errored_subroutine(logger, error_code_I1P)
    call logger%check_error('errored_subroutine_I1P', error_code_I1P, routine=this, is_fatal=.false.)

    call errored_subroutine(logger, error_code_I2P)
    call logger%check_error('errored_subroutine_I2P', error_code_I2P, routine=this, is_fatal=.false.)
    call logger%warn('Ignoring code: '//trim(str(error_code_I2P)), routine=this)
    call logger%ignore_error_codes([error_code_I2P])
    call errored_subroutine(logger, error_code_I2P)
    call logger%check_error('errored_subroutine_I2P', error_code_I2P, routine=this, is_fatal=.false.)

    call errored_subroutine(logger, error_code_I4P)
    call logger%check_error('errored_subroutine_I4P', error_code_I4P, routine=this, is_fatal=.false.)
    call logger%warn('Ignoring code: '//trim(str(error_code_I4P)), routine=this)
    call logger%ignore_error_codes([error_code_I4P])
    call errored_subroutine(logger, error_code_I4P)
    call logger%check_error('errored_subroutine_I4P', error_code_I4P, routine=this, is_fatal=.false.)

    call errored_subroutine(logger, error_code_I8P)
    call logger%check_error('errored_subroutine_I8P', error_code_I8P, routine=this, is_fatal=.false.)
    call logger%warn('Ignoring code: '//trim(str(error_code_I8P)), routine=this)
    call logger%ignore_error_codes([error_code_I8P])
    call errored_subroutine(logger, error_code_I8P)
    call logger%check_error('errored_subroutine_I8P', error_code_I8P, routine=this, is_fatal=.false.)

    call errored_subroutine(logger, error_code_LOGICAL)
    call logger%check_error('errored_subroutine_LOGICAL', error_code_LOGICAL, routine=this, is_fatal=.false.)

    ! there is no way to ignore logical error code

    call logger%info('***************************************************************')
    call logger%info('                  Tests passed...                              ')
    call logger%info('***************************************************************')
  end subroutine test_ignore_error_codes

  subroutine errored_subroutine_I1P(logger, error)
  !< Dummy external subroutine that returns non-zero error code, 1 byte
    class(fortran_logger_t),  intent(inout) :: logger   !< Fortran logger class
    integer(I1P),             intent(out)   :: error    !< Error code

    call logger%debug('Entering...', routine = 'errored_subroutine_I1P')
    error = -1_I1P
    call logger%debug('Exiting...', routine = 'errored_subroutine_I1P')
  end subroutine errored_subroutine_I1P

  subroutine errored_subroutine_I2P(logger, error)
  !< Dummy external subroutine that returns non-zero error code, 2 bytes
    class(fortran_logger_t),  intent(inout) :: logger   !< Fortran logger class
    integer(I2P),             intent(out)   :: error    !< Error code

    call logger%debug('Entering...', routine = 'errored_subroutine_I2P')
    error = -2_I2P
    call logger%debug('Exiting...', routine = 'errored_subroutine_I2P')
  end subroutine errored_subroutine_I2P

  subroutine errored_subroutine_I4P(logger, error)
  !< Dummy external subroutine that returns non-zero error code, 4 bytes
    class(fortran_logger_t),  intent(inout) :: logger   !< Fortran logger class
    integer(I4P),             intent(out)   :: error    !< Error code

    call logger%debug('Entering...', routine = 'errored_subroutine_I4P')
    error = -4_I4P
    call logger%debug('Exiting...', routine = 'errored_subroutine_I4P')
  end subroutine errored_subroutine_I4P

  subroutine errored_subroutine_I8P(logger, error)
  !< Dummy external subroutine that returns non-zero error code, 8 bytes
    class(fortran_logger_t),  intent(inout) :: logger   !< Fortran logger class
    integer(I8P),             intent(out)   :: error    !< Error code

    call logger%debug('Entering...', routine = 'errored_subroutine_I8P')
    error = -8_I8P
    call logger%debug('Exiting...', routine = 'errored_subroutine_I8P')
  end subroutine errored_subroutine_I8P

  subroutine errored_subroutine_LOGICAL(logger, error)
  !< Dummy external subroutine that returns logical False status
    class(fortran_logger_t),  intent(inout) :: logger   !< Fortran logger class
    logical,                  intent(out)   :: error    !< Error code

    call logger%debug('Entering...', routine = 'errored_subroutine_LOGICAL')
    error = .false.
    call logger%debug('Exiting...', routine = 'errored_subroutine_LOGICAL')
  end subroutine errored_subroutine_LOGICAL
end module test_ignore_error_codes_m