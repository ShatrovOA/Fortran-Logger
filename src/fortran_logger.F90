!< Fortran-Logger project, definition of [[fortran_logger_t]] class.

module fortran_logger
!< Fortran-Logger project, definition of [[fortran_logger_t]] class.
use datetime_module, only: datetime
use face, only : colorize
use flap
use json_module
use penf
use logger_element_object
#ifdef _MPI
use mpi_f08
#endif
implicit none
private
public :: fortran_logger_t,                       &
          LOGGER_NULL_LEVEL,                      &
          LOGGER_ERROR_LEVEL,                     &
          LOGGER_WARN_LEVEL,                      &
          LOGGER_INFO_LEVEL,                      &
          LOGGER_DEBUG_LEVEL,                     &
          LOGGER_ERROR_ALLOCATION_FAILED,         &
          LOGGER_ERROR_JSON_VALUE_NOT_FOUND,      &
          LOGGER_ERROR_JSON_VALUE_TYPE_MISMATCH,  &
          LOGGER_ERROR_DIRECTORY_NOT_FOUND,       &
          LOGGER_ERROR_FILE_NOT_FOUND

  integer(I4P), parameter :: LOGGER_NULL_LEVEL   = 0_I4P    !< Null Level Code  
                                                            !< Logger will not produce output
  integer(I4P), parameter :: LOGGER_ERROR_LEVEL  = 1_I4P    !< Error Level Code  
                                                            !< Logger will produce output only if error occurs
  integer(I4P), parameter :: LOGGER_WARN_LEVEL   = 2_I4P    !< Warning Level Code  
                                                            !< Logger will produce Error + Warning messages
  integer(I4P), parameter :: LOGGER_INFO_LEVEL   = 3_I4P    !< Info Level Code  
                                                            !< Logger will produce Error + Warning + Info messages
  integer(I4P), parameter :: LOGGER_DEBUG_LEVEL  = 4_I4P    !< Debug Level Code  
                                                            !< Logger will produce all messages

  integer(I4P), parameter :: LOGGER_ERROR_ALLOCATION_FAILED = -999_I4P            !< Internal error, allocation failure
  integer(I4P), parameter :: LOGGER_ERROR_JSON_VALUE_NOT_FOUND = -998_I4P         !< Internal error, requested value not found in json file
  integer(I4P), parameter :: LOGGER_ERROR_JSON_VALUE_TYPE_MISMATCH = -997_I4P     !< Internal error, requested value has a different type
  integer(I4P), parameter :: LOGGER_ERROR_DIRECTORY_NOT_FOUND = -996_I4P          !< Internal error, requested directory not found in the path
  integer(I4P), parameter :: LOGGER_ERROR_FILE_NOT_FOUND = -995_I4P               !< Internal error, requested file not found in the path
  integer(I4P), parameter :: LOGGER_ERROR_CONVERTED_LOGICAL = -994_I4P            !< Internal error, converted from logical value

  character(len=*), parameter, dimension(0:7) :: JSON_TYPES = [               &   !< JSON types from json fortran library  
                                                                "Unknown",    &   !< Check it out on [GitHub](https://github.com/jacobwilliams/json-fortran)
                                                                "Null   ",    &
                                                                "Object ",    &
                                                                "Array  ",    &
                                                                "Logical",    &
                                                                "Integer",    &
                                                                "Real   ",    &
                                                                "String "     &
                                                              ]

  type :: fortran_logger_t
  !< Definition of Logger class
  private
    integer(I4P)                      :: log_level            !< Level of logging
    type(element_object)              :: logger_object(4)     !< Objects that handle messages from all levels
    type(element_object)              :: timestamp_object     !< Timestamp handle
    type(element_object)              :: routine_object       !< Handle of optional "routine" parameter in most of methods
    logical                           :: print_timestamp      !< Timestamp printer flag
    character(len=:),     allocatable :: timestamp_format     !< Timestamp format, C style
    integer(I4P),         allocatable :: ignored_codes(:)     !< Buffer that keeps all ignored codes
#ifdef _MPI
    integer(I4P), allocatable         :: gather_buf(:)        !< Buffer that aggregates all error codes
    type(MPI_Comm)                    :: comm                 !< MPI Communicator
#endif
  contains
  private
  ! Public methods
    procedure, pass(self),  public :: finalize                  !< Finalization class subroutine
    procedure, pass(self),  public :: initialize                !< Initialization class subroutine
    procedure, pass(self),  public :: error                     !< Print error message
    procedure, pass(self),  public :: warn                      !< Print warn message
    procedure, pass(self),  public :: info                      !< Print info message
    procedure, pass(self),  public :: debug                     !< Print debug message
    procedure, pass(self),  public :: change_unit               !< Runtime output unit change
    procedure, pass(self),  public :: check_json_value          !< Checks presence and type of value in json file
    procedure, pass(self),  public :: check_directory           !< Checks directory presence
    procedure, pass(self),  public :: check_file                !< Checks file presence
    generic, public :: check_error =>             &             !< Checks error code returned by external subroutine.  
                       check_error_LOGICAL,       &
                       check_error_I1P,           &
                       check_error_I2P,           &
                       check_error_I4P,           &
                       check_error_I8P
    generic, public :: check_alloc =>             &             !< Checks allocation of buffers of different types and ranks
                       check_alloc_rank1_CR4P,    &
                       check_alloc_rank1_CR8P,    &
#ifdef _R16P
                       check_alloc_rank1_CR16P,   &
#endif
                       check_alloc_rank2_CR4P,    &
                       check_alloc_rank2_CR8P,    &
#ifdef _R16P
                       check_alloc_rank2_CR16P,   &
#endif
                       check_alloc_rank3_CR4P,    &
                       check_alloc_rank3_CR8P,    &
#ifdef _R16P
                       check_alloc_rank3_CR16P,   &
#endif
                       check_alloc_rank4_CR4P,    &
                       check_alloc_rank4_CR8P,    &
#ifdef _R16P
                       check_alloc_rank4_CR16P,   &
#endif
                       check_alloc_rank1_R4P,     &
                       check_alloc_rank1_R8P,     &
#ifdef _R16P
                       check_alloc_rank1_R16P,    &
#endif
                       check_alloc_rank2_R4P,     &
                       check_alloc_rank2_R8P,     &
#ifdef _R16P
                       check_alloc_rank2_R16P,    &
#endif
                       check_alloc_rank3_R4P,     &
                       check_alloc_rank3_R8P,     &
#ifdef _R16P
                       check_alloc_rank3_R16P,    &
#endif
                       check_alloc_rank4_R4P,     &
                       check_alloc_rank4_R8P,     &
#ifdef _R16P
                       check_alloc_rank4_R16P,    &
#endif
                       check_alloc_rank1_I1P,     &
                       check_alloc_rank1_I2P,     &
                       check_alloc_rank1_I4P,     &
                       check_alloc_rank1_I8P,     &
                       check_alloc_rank2_I1P,     &
                       check_alloc_rank2_I2P,     &
                       check_alloc_rank2_I4P,     &
                       check_alloc_rank2_I8P,     &
                       check_alloc_rank3_I1P,     &
                       check_alloc_rank3_I2P,     &
                       check_alloc_rank3_I4P,     &
                       check_alloc_rank3_I8P,     &
                       check_alloc_rank4_I1P,     &
                       check_alloc_rank4_I2P,     &
                       check_alloc_rank4_I4P,     &
                       check_alloc_rank4_I8P
    generic, public :: ignore_error_codes =>      &             !< Ignore codes of various types except logical
                       ignore_error_codes_I1P,    &
                       ignore_error_codes_I2P,    &
                       ignore_error_codes_I4P,    &
                       ignore_error_codes_I8P
  ! Private methods
  ! Check error methods
    procedure, pass(self) :: check_error_LOGICAL                !< Checks logical error_code returned by external subroutine.  
    procedure, pass(self) :: check_error_I1P                    !< Checks integer I1P error_code returned by external subroutine. 
    procedure, pass(self) :: check_error_I2P                    !< Checks integer I2P error_code returned by external subroutine.  
    procedure, pass(self) :: check_error_I4P                    !< Checks integer I4P error_code returned by external subroutine.  
    procedure, pass(self) :: check_error_I8P                    !< Checks integer I8P error_code returned by external subroutine.  
  ! Check alloc methods
    procedure, pass(self) :: check_alloc_rank1_CR4P             !< Checks allocation of complex R4P buffer of rank = 1  
    procedure, pass(self) :: check_alloc_rank1_CR8P             !< Checks allocation of complex R8P buffer of rank = 1
#ifdef _R16P
    procedure, pass(self) :: check_alloc_rank1_CR16P            !< Checks allocation of complex R16P buffer of rank = 1
#endif
    procedure, pass(self) :: check_alloc_rank2_CR4P             !< Checks allocation of complex R4P buffer of rank = 2
    procedure, pass(self) :: check_alloc_rank2_CR8P             !< Checks allocation of complex R8P buffer of rank = 2
#ifdef _R16P
    procedure, pass(self) :: check_alloc_rank2_CR16P            !< Checks allocation of complex R16P buffer of rank = 2
#endif
    procedure, pass(self) :: check_alloc_rank3_CR4P             !< Checks allocation of complex R4P buffer of rank = 3
    procedure, pass(self) :: check_alloc_rank3_CR8P             !< Checks allocation of complex R8P buffer of rank = 3
#ifdef _R16P
    procedure, pass(self) :: check_alloc_rank3_CR16P            !< Checks allocation of complex R16P buffer of rank = 3
#endif
    procedure, pass(self) :: check_alloc_rank4_CR4P             !< Checks allocation of complex R4P buffer of rank = 4
    procedure, pass(self) :: check_alloc_rank4_CR8P             !< Checks allocation of complex R8P buffer of rank = 4
#ifdef _R16P
    procedure, pass(self) :: check_alloc_rank4_CR16P            !< Checks allocation of complex R16P buffer of rank = 4
#endif
    procedure, pass(self) :: check_alloc_rank1_R4P              !< Checks allocation of real R4P buffer of rank = 1
    procedure, pass(self) :: check_alloc_rank1_R8P              !< Checks allocation of real R8P buffer of rank = 1
#ifdef _R16P
    procedure, pass(self) :: check_alloc_rank1_R16P             !< Checks allocation of real R16P buffer of rank = 1
#endif
    procedure, pass(self) :: check_alloc_rank2_R4P              !< Checks allocation of real R4P buffer of rank = 2
    procedure, pass(self) :: check_alloc_rank2_R8P              !< Checks allocation of real R8P buffer of rank = 2
#ifdef _R16P
    procedure, pass(self) :: check_alloc_rank2_R16P             !< Checks allocation of real R16P buffer of rank = 2
#endif
    procedure, pass(self) :: check_alloc_rank3_R4P              !< Checks allocation of real R4P buffer of rank = 3
    procedure, pass(self) :: check_alloc_rank3_R8P              !< Checks allocation of real R8P buffer of rank = 3
#ifdef _R16P
    procedure, pass(self) :: check_alloc_rank3_R16P             !< Checks allocation of real R16P buffer of rank = 3
#endif
    procedure, pass(self) :: check_alloc_rank4_R4P              !< Checks allocation of real R4P buffer of rank = 4
    procedure, pass(self) :: check_alloc_rank4_R8P              !< Checks allocation of real R8P buffer of rank = 4
#ifdef _R16P
    procedure, pass(self) :: check_alloc_rank4_R16P             !< Checks allocation of real R16P buffer of rank = 4
#endif
    procedure, pass(self) :: check_alloc_rank1_I1P              !< Checks allocation of integer I1P buffer of rank = 1
    procedure, pass(self) :: check_alloc_rank1_I2P              !< Checks allocation of integer I2P buffer of rank = 1
    procedure, pass(self) :: check_alloc_rank1_I4P              !< Checks allocation of integer I4P buffer of rank = 1
    procedure, pass(self) :: check_alloc_rank1_I8P              !< Checks allocation of integer I8P buffer of rank = 1
    procedure, pass(self) :: check_alloc_rank2_I1P              !< Checks allocation of integer I1P buffer of rank = 2
    procedure, pass(self) :: check_alloc_rank2_I2P              !< Checks allocation of integer I2P buffer of rank = 2
    procedure, pass(self) :: check_alloc_rank2_I4P              !< Checks allocation of integer I4P buffer of rank = 2
    procedure, pass(self) :: check_alloc_rank2_I8P              !< Checks allocation of integer I8P buffer of rank = 2
    procedure, pass(self) :: check_alloc_rank3_I1P              !< Checks allocation of integer I1P buffer of rank = 3
    procedure, pass(self) :: check_alloc_rank3_I2P              !< Checks allocation of integer I2P buffer of rank = 3
    procedure, pass(self) :: check_alloc_rank3_I4P              !< Checks allocation of integer I4P buffer of rank = 3
    procedure, pass(self) :: check_alloc_rank3_I8P              !< Checks allocation of integer I8P buffer of rank = 3
    procedure, pass(self) :: check_alloc_rank4_I1P              !< Checks allocation of integer I1P buffer of rank = 4
    procedure, pass(self) :: check_alloc_rank4_I2P              !< Checks allocation of integer I2P buffer of rank = 4
    procedure, pass(self) :: check_alloc_rank4_I4P              !< Checks allocation of integer I4P buffer of rank = 4
    procedure, pass(self) :: check_alloc_rank4_I8P              !< Checks allocation of integer I8P buffer of rank = 4
  ! Ignoring error codes methods
    procedure, pass(self) :: ignore_error_codes_I1P             !< Ignore codes type I1P 
    procedure, pass(self) :: ignore_error_codes_I2P             !< Ignore codes type I2P 
    procedure, pass(self) :: ignore_error_codes_I4P             !< Ignore codes type I4P 
    procedure, pass(self) :: ignore_error_codes_I8P             !< Ignore codes type I8P 
  ! Other private methods
    procedure, pass(self) :: gather_error_codes                 !< Gather error codes from all processes
    procedure, pass(self) :: print                              !< Print messages to specified unit
  endtype fortran_logger_t

contains 
!-------------------------------------------------------------------------------------
  subroutine finalize(self)
!-------------------------------------------------------------------------------------
!< Finalization class subroutine
!-------------------------------------------------------------------------------------
    class(fortran_logger_t), intent(inout) :: self  !< Logger
    integer(I4P)                           :: i     !< Counter

    self%log_level = LOGGER_DEBUG_LEVEL
    do i = LOGGER_ERROR_LEVEL, LOGGER_DEBUG_LEVEL
      call self%logger_object(i)%finalize()
    enddo
    call self%timestamp_object%finalize()
    self%print_timestamp = .true.
    if(allocated(self%timestamp_format)) deallocate(self%timestamp_format)
    if(allocated(self%ignored_codes))    deallocate(self%ignored_codes)
#ifdef _MPI
    self%comm = MPI_COMM_NULL
    if(allocated(self%gather_buf)) deallocate(self%gather_buf)
#endif
  end subroutine finalize

!-------------------------------------------------------------------------------------
  subroutine initialize(self,                                                                                   &
#ifdef _MPI
                        comm,                                                                                   &
#endif
                        log_level,                                                                              &
                        error_unit, error_color_fg, error_color_bg, error_style, error_prefix, error_suffix,    &
                        warn_unit, warn_color_fg, warn_color_bg, warn_style, warn_prefix, warn_suffix,          &
                        info_unit, info_color_fg, info_color_bg, info_style, info_prefix, info_suffix,          &
                        debug_unit, debug_color_fg, debug_color_bg, debug_style, debug_prefix, debug_suffix,    &
                        routine_prefix, routine_suffix, print_timestamp, timestamp_format, timestamp_color_fg,  &
                        timestamp_color_bg, timestamp_style, timestamp_prefix, timestamp_suffix)
!-------------------------------------------------------------------------------------
!< Initialization class subroutine  
!< If built with MPI support then MPI_Init must be called before calling this method
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),  intent(inout)         :: self                 !< Logger
#ifdef _MPI
    type(MPI_Comm),           intent(in)            :: comm                 !< MPI Communicator
#endif
    integer(I4P),             intent(in), optional  :: log_level            !< Level of logging
    integer(I4P),             intent(in), optional  :: error_unit           !< Unit used to print ERROR messages
    character(len=*),         intent(in), optional  :: error_color_fg       !< ERROR foreground color
    character(len=*),         intent(in), optional  :: error_color_bg       !< ERROR background color
    character(len=*),         intent(in), optional  :: error_style          !< ERROR style
    character(len=*),         intent(in), optional  :: error_prefix         !< ERROR prefix
    character(len=*),         intent(in), optional  :: error_suffix         !< ERROR suffix
    integer(I4P),             intent(in), optional  :: warn_unit            !< Unit used to print WARN messages
    character(len=*),         intent(in), optional  :: warn_color_fg        !< WARN foreground color
    character(len=*),         intent(in), optional  :: warn_color_bg        !< WARN background color
    character(len=*),         intent(in), optional  :: warn_style           !< WARN style
    character(len=*),         intent(in), optional  :: warn_prefix          !< WARN prefix
    character(len=*),         intent(in), optional  :: warn_suffix          !< WARN suffix
    integer(I4P),             intent(in), optional  :: info_unit            !< Unit used to print INFO messages
    character(len=*),         intent(in), optional  :: info_color_fg        !< INFO foreground color
    character(len=*),         intent(in), optional  :: info_color_bg        !< INFO background color
    character(len=*),         intent(in), optional  :: info_style           !< INFO style
    character(len=*),         intent(in), optional  :: info_prefix          !< INFO prefix
    character(len=*),         intent(in), optional  :: info_suffix          !< INFO suffix
    integer(I4P),             intent(in), optional  :: debug_unit           !< Unit used to print DEBUG messages
    character(len=*),         intent(in), optional  :: debug_color_fg       !< DEBUG foreground color
    character(len=*),         intent(in), optional  :: debug_color_bg       !< DEBUG background color
    character(len=*),         intent(in), optional  :: debug_style          !< DEBUG style
    character(len=*),         intent(in), optional  :: debug_prefix         !< DEBUG prefix
    character(len=*),         intent(in), optional  :: debug_suffix         !< DEBUG suffix
    character(len=*),         intent(in), optional  :: routine_prefix       !< Routine prefix. routine is optional agrument in most of logger methods
    character(len=*),         intent(in), optional  :: routine_suffix       !< Routine suffix. routine is optional agrument in most of logger methods
    logical,                  intent(in), optional  :: print_timestamp      !< Display current timestamp with all messages. Default is .true.
    character(len=*),         intent(in), optional  :: timestamp_format     !< Timestamp format, C style
    character(len=*),         intent(in), optional  :: timestamp_color_fg   !< Timestamp foreground color
    character(len=*),         intent(in), optional  :: timestamp_color_bg   !< Timestamp background color
    character(len=*),         intent(in), optional  :: timestamp_style      !< Timestamp style
    character(len=*),         intent(in), optional  :: timestamp_prefix     !< Timestamp prefix
    character(len=*),         intent(in), optional  :: timestamp_suffix     !< Timestamp suffix
    type(command_line_interface)                    :: cli                  !< CLI parser
    integer(I4P)                                    :: null_unit            !< Unit used to hide CLI output messages
    integer(I4P)                                    :: temp_level           !< Temporal level value
    character(len=:),         allocatable           :: this                 !< This method name
#ifdef _MPI
    integer(I4P)                                    :: np                   !< MPI number of processes
#endif

    call self%finalize()
    if(present(log_level)) self%log_level = log_level
    call self%logger_object(LOGGER_ERROR_LEVEL)%initialize(out_unit = error_unit, string = 'ERROR',               &
                                                           color_fg = error_color_fg, color_bg = error_color_bg,  &
                                                           style = error_style,                                   &
                                                           prefix = error_prefix, suffix = error_suffix)
    call self%logger_object(LOGGER_WARN_LEVEL)%initialize( out_unit = warn_unit, string = 'WARN',                 &
                                                           color_fg = warn_color_fg, color_bg = warn_color_bg,    &
                                                           style = warn_style,                                    &
                                                           prefix = warn_prefix, suffix = warn_suffix)
    call self%logger_object(LOGGER_INFO_LEVEL)%initialize( out_unit = info_unit, string = 'INFO',                 &
                                                           color_fg = info_color_fg, color_bg = info_color_bg,    &
                                                           style = info_style,                                    &
                                                           prefix = info_prefix, suffix = info_suffix)
    call self%logger_object(LOGGER_DEBUG_LEVEL)%initialize(out_unit = debug_unit, string = 'DEBUG',               &
                                                           color_fg = debug_color_fg, color_bg = debug_color_bg,  &
                                                           style = debug_style,                                   &
                                                           prefix = debug_prefix, suffix = debug_suffix)

    call self%routine_object%initialize(prefix = routine_prefix, suffix = routine_suffix)
    if(present(print_timestamp)) self%print_timestamp = print_timestamp
    self%timestamp_format = '%c'; if(present(timestamp_format)) self%timestamp_format = timestamp_format
    call self%timestamp_object%initialize(color_fg = timestamp_color_fg, color_bg = timestamp_color_bg,           &
                                          style = timestamp_style,                                                &
                                          prefix = timestamp_prefix, suffix = timestamp_suffix)
#ifdef _MPI
    self%comm = comm
    call MPI_Comm_size(self%comm, np)
    allocate(self%gather_buf(0:np - 1), source = 0_I4P)
#endif

    open( newunit = null_unit,    &
          status = 'scratch',     &
          action = 'write')

    call cli%init(ignore_unknown_clas = .true.,   &
                  usage_lun = null_unit,          &
                  error_lun  = null_unit,         &
                  version_lun = null_unit,        &
                  disable_hv = .true.)
    call cli%add( switch = '--logger_level',      &
                  switch_ab = '-log_lev',         &
                  required = .false.,             &
                  def = '4',                      &
                  act='store')
    call cli%parse()
    if(cli%is_passed(switch = '-log_lev')) then 
      call cli%get(val = self%log_level, switch = '-log_lev')
    endif
    call cli%free()
    close(null_unit)
    if(self%log_level > LOGGER_DEBUG_LEVEL .or. self%log_level < LOGGER_NULL_LEVEL) then
      temp_level = self%log_level
      self%log_level = LOGGER_DEBUG_LEVEL
      this = 'fortran_logger_t.initialize'
      call self%error(message = 'Wrong value of log_level provided: '//trim(str(n = temp_level)), routine = this)
      call self%error(message = 'Assuming log_level = 4 (LOGGER_DEBUG_LEVEL)', routine = this)
    endif
  end subroutine initialize

!-------------------------------------------------------------------------------------
  subroutine ignore_error_codes_I1P(self, error_codes) 
!-------------------------------------------------------------------------------------
!< Some error codes, returned by external subroutines might be unimportant warnings.  
!< This subroutine takes array of codes integer type I1P, which will be be ignored by logger.  
!< No message will be displayed if one the codes occurs during execution, even if error is fatal
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< integer(I1P) :: status, ignored_code
!< call logger%initialize()
!< ignored_code = 33_I1P
!< call logger%ignore_error_codes([ignored_code])
!< call external_subroutine(.., status)
!< call logger%check_error('external_subroutine', status)
!< ! If status /= 0 AND ignored_code == status, then no message will be displayed
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),  intent(inout) :: self                   !< Logger
    integer(I1P),             intent(in)    :: error_codes(:)         !< Array of error codes that will be ignored by logger  
    integer(I4P),             allocatable   :: casted_error_codes(:)  !< Error codes casted to I4P

    allocate(casted_error_codes(size(error_codes, dim = 1)), source = int(error_codes, I4P))
    call self%ignore_error_codes(casted_error_codes)
    deallocate(casted_error_codes)
  end subroutine ignore_error_codes_I1P

!-------------------------------------------------------------------------------------
  subroutine ignore_error_codes_I2P(self, error_codes)
!-------------------------------------------------------------------------------------
!< Some error codes, returned by external subroutines might be unimportant warnings.  
!< This subroutine takes array of codes integer type I2P, which will be be ignored by logger.  
!< No message will be displayed if one the codes occurs during execution, even if error is fatal
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< integer(I2P) :: status, ignored_code
!< call logger%initialize()
!< ignored_code = 33_I2P
!< call logger%ignore_error_codes([ignored_code])
!< call external_subroutine(.., status)
!< call logger%check_error('external_subroutine', status)
!< ! If status /= 0 AND ignored_code == status, then no message will be displayed
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),  intent(inout) :: self                   !< Logger
    integer(I2P),             intent(in)    :: error_codes(:)         !< Array of error codes that will be ignored by logger
    integer(I4P),             allocatable   :: casted_error_codes(:)  !< Error codes casted to I4P

    allocate(casted_error_codes(size(error_codes, dim = 1)), source = int(error_codes, I4P))
    call self%ignore_error_codes(casted_error_codes)
    deallocate(casted_error_codes)
  end subroutine ignore_error_codes_I2P

!-------------------------------------------------------------------------------------
  subroutine ignore_error_codes_I4P(self, error_codes) 
!-------------------------------------------------------------------------------------
!< Some error codes, returned by external subroutines might be unimportant warnings.  
!< This subroutine takes array of codes integer type I4P, which will be be ignored by logger.  
!< No message will be displayed if one the codes occurs during execution, even if error is fatal
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< integer(I4P) :: status, ignored_code
!< call logger%initialize()
!< ignored_code = 33_I4P
!< call logger%ignore_error_codes([ignored_code])
!< call external_subroutine(.., status)
!< call logger%check_error('external_subroutine', status)
!< ! If status /= 0 AND ignored_code == status, then no message will be displayed
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),  intent(inout) :: self                       !< Logger
    integer(I4P),             intent(in)    :: error_codes(:)             !< Array of error codes that will be ignored by logger
    integer(I4P)                            :: new_ignored_codes_count    !< Size of error_codes
    integer(I4P)                            :: old_ignored_codes_count    !< Size of logger ignored_codes
    integer(I4P)                            :: total_ignored_codes_count  !< new_ignored_codes_count + old_ignored_codes_count
    integer(I4P),             allocatable   :: ignored_codes(:)           !< Temporal buffer

    new_ignored_codes_count = size(error_codes, dim = 1)
    if(allocated(self%ignored_codes)) then 
      old_ignored_codes_count = size(self%ignored_codes, dim = 1)
      allocate(ignored_codes(old_ignored_codes_count), source = self%ignored_codes)
      total_ignored_codes_count = new_ignored_codes_count + old_ignored_codes_count
      deallocate(self%ignored_codes)
      allocate(self%ignored_codes(total_ignored_codes_count))
      self%ignored_codes(:old_ignored_codes_count) = ignored_codes(:)
      self%ignored_codes(old_ignored_codes_count + 1:) = error_codes(:)
      deallocate(ignored_codes)
    else
      allocate(self%ignored_codes(new_ignored_codes_count))
      self%ignored_codes(:) = error_codes(:)
    endif
  end subroutine ignore_error_codes_I4P

!-------------------------------------------------------------------------------------
  subroutine ignore_error_codes_I8P(self, error_codes) 
!-------------------------------------------------------------------------------------
!< Some error codes, returned by external subroutines might be unimportant warnings.  
!< This subroutine takes array of codes integer type I8P, which will be be ignored by logger.  
!< No message will be displayed if one the codes occurs during execution, even if error is fatal
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< integer(I8P) :: status, ignored_code
!< call logger%initialize()
!< ignored_code = 33_I8P
!< call logger%ignore_error_codes([ignored_code])
!< call external_subroutine(.., status)
!< call logger%check_error('external_subroutine', status)
!< ! If status /= 0 AND ignored_code == status, then no message will be displayed
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),  intent(inout) :: self                   !< Logger
    integer(I8P),             intent(in)    :: error_codes(:)         !< Array of error codes that will be ignored by logger
    integer(I4P),             allocatable   :: casted_error_codes(:)  !< Error codes casted to I4P

    allocate(casted_error_codes(size(error_codes, dim = 1)), source = int(error_codes, I4P))
    call self%ignore_error_codes(casted_error_codes)
    deallocate(casted_error_codes)
  end subroutine ignore_error_codes_I8P

!-------------------------------------------------------------------------------------
  subroutine change_unit(self, log_level, new_unit)
!-------------------------------------------------------------------------------------
!< Will change output unit for the specified logging level
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),  intent(inout) :: self       !< Logger
    integer(I4P),             intent(in)    :: log_level  !< Logging level
    integer(I4P),             intent(in)    :: new_unit   !< New output unit for specified log_level
    character(len=:),         allocatable   :: this       !< This method name

    if(log_level > LOGGER_DEBUG_LEVEL .or. self%log_level < LOGGER_ERROR_LEVEL) then
      this = 'fortran_logger_t.change_unit'
      call self%error("Wrong value of log_level provided: "//trim(str(n = log_level,no_sign = .true.)), routine = this)
      call self%error("Unit will not be changed...", routine = this)
    else 
      call self%logger_object(log_level)%update_unit(new_unit)
    endif
  end subroutine change_unit

!-------------------------------------------------------------------------------------
  subroutine debug(self, message, routine, file, line)
!-------------------------------------------------------------------------------------
!< Will print debug message if current logging level = [[LOGGER_DEBUG_LEVEL]]
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),  intent(inout)         :: self     !< Logger
    character(len=*),         intent(in)            :: message  !< Message to be printed
    character(len=*),         intent(in), optional  :: routine  !< Internal subroutine name
    character(len=*),         intent(in), optional  :: file     !< Source file name where debug was called, e.g. \_\_FILE\_\_
    integer(I4P),             intent(in), optional  :: line     !< Line where debug was called, e.g. \_\_LINE\_\_

    if(self%log_level == LOGGER_DEBUG_LEVEL) then
      call self%print(LOGGER_DEBUG_LEVEL, message, routine=routine, file=file, line=line)
    endif
  end subroutine debug

!-------------------------------------------------------------------------------------
  subroutine info(self, message, routine, file, line)
!-------------------------------------------------------------------------------------
!< Will print info message if current logging level >= [[LOGGER_INFO_LEVEL]]
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),  intent(inout)         :: self     !< Logger
    character(len=*),         intent(in)            :: message  !< Message to be printed
    character(len=*),         intent(in), optional  :: routine  !< Internal subroutine name
    character(len=*),         intent(in), optional  :: file     !< Source file name where info was called, e.g. \_\_FILE\_\_
    integer(I4P),             intent(in), optional  :: line     !< Line where info was called, e.g. \_\_LINE\_\_

    if(self%log_level >= LOGGER_INFO_LEVEL) then
      call self%print(LOGGER_INFO_LEVEL, message, routine=routine, file=file, line=line)
    endif
  end subroutine info

!-------------------------------------------------------------------------------------
  subroutine warn(self, message, routine, file, line)
!-------------------------------------------------------------------------------------
!< Will print warn message if current logging level >= [[LOGGER_WARN_LEVEL]]
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),  intent(inout)         :: self     !< Logger
    character(len=*),         intent(in)            :: message  !< Message to be printed
    character(len=*),         intent(in), optional  :: routine  !< Internal subroutine name
    character(len=*),         intent(in), optional  :: file     !< Source file name where warn was called, e.g. \_\_FILE\_\_
    integer(I4P),             intent(in), optional  :: line     !< Line where warn was called, e.g. \_\_LINE\_\_

    if(self%log_level >= LOGGER_WARN_LEVEL) then
      call self%print(LOGGER_WARN_LEVEL, message, routine=routine, file=file, line=line)
    endif
  end subroutine warn

!-------------------------------------------------------------------------------------
#ifdef _MPI
  subroutine error(self, message, routine, file, line, error_code, errored_rank, is_fatal)
#else
  subroutine error(self, message, routine, file, line, error_code, is_fatal)
#endif
!-------------------------------------------------------------------------------------
!< Will print error message if current logging level >= [[LOGGER_ERROR_LEVEL]].  
!< If is_fatal = .true., program will stop execution.
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),  intent(inout)         :: self         !< Logger
    character(len=*),         intent(in)            :: message      !< Message to be printed
    character(len=*),         intent(in), optional  :: routine      !< Internal subroutine name
    character(len=*),         intent(in), optional  :: file         !< Source file name where error was called, e.g. \_\_FILE\_\_
    integer(I4P),             intent(in), optional  :: line         !< Line where error was called, e.g. \_\_LINE\_\_
    integer(I4P),             intent(in), optional  :: error_code   !< Internal subroutine returned error code
#ifdef _MPI
    integer(I4P),             intent(in), optional  :: errored_rank !< Rank where error occured
#endif
    logical,                  intent(in), optional  :: is_fatal     !< Fatal error. Default is .false.
    logical                                         :: fatal_error  !< Fatal error. Default is .false.

    if(present(error_code)) then 
      if(allocated(self%ignored_codes)) then
        ! Ignoring specified error codes
          if(any(error_code == self%ignored_codes)) return
        endif
    endif
    if(self%log_level >= LOGGER_ERROR_LEVEL) then
      fatal_error = .false.; if(present(is_fatal)) fatal_error = is_fatal
#ifdef _MPI
      call self%print(LOGGER_ERROR_LEVEL, message, routine=routine, file=file, line=line, error_code=error_code, rank=errored_rank)
#else
      call self%print(LOGGER_ERROR_LEVEL, message, routine=routine, file=file, line=line, error_code=error_code)
#endif
      if(fatal_error) then
        call self%print(LOGGER_ERROR_LEVEL, 'This error is fatal. Program will stop executing now...', routine = routine)
        call self%finalize()
#ifdef _MPI
        call MPI_Finalize()
#endif
        error stop 'Fortran logger aborted execution of program due to fatal error occured'
      endif
    endif

  end subroutine error

!-------------------------------------------------------------------------------------
  subroutine gather_error_codes(self, message, error_code, routine, file, line, is_fatal, gathered_code)
!-------------------------------------------------------------------------------------
!< Gather error codes from all processes
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),  intent(inout)           :: self                 !< Logger
    character(len=*),         intent(in)              :: message              !< Message to be printed
    class(*),                 intent(in)              :: error_code           !< Internal subroutine returned error code
    character(len=*),         intent(in),   optional  :: routine              !< Internal subroutine name
    character(len=*),         intent(in),   optional  :: file                 !< Source file name where error might have occured, e.g. \_\_FILE\_\_
    integer(I4P),             intent(in),   optional  :: line                 !< Line where error might have occured, e.g. \_\_LINE\_\_
    logical,                  intent(in),   optional  :: is_fatal             !< Fatal error. Default is .false.
    integer(I4P),             intent(out),  optional  :: gathered_code        !< Aggregated error code similar on all processes.
    character(len=500)                                :: internal_message     !< Aggregated message to be printed
    integer(I4P)                                      :: positive_error_code  !< Absolute value of error_code.
    integer(I4P)                                      :: gathered_error_code  !< Aggregated error code similar on all processes.
#ifdef _MPI
    integer(I4P)                                      :: msg_length           !< Length of the message
    integer(I4P)                                      :: errored_rank         !< Rank where error occured
#endif

    internal_message = message
    select type(error_code)
    type is (integer(I1P))
      positive_error_code = int(abs(error_code), I4P)
      gathered_error_code = int(error_code, I4P)
    type is (integer(I2P))
      positive_error_code = int(abs(error_code), I4P)
      gathered_error_code = int(error_code, I4P)
    type is (integer(I4P))
      positive_error_code = abs(error_code)
      gathered_error_code = error_code
    type is (integer(I8P))
      positive_error_code = int(abs(error_code), I4P)
      gathered_error_code = int(error_code, I4P)
    endselect
#ifdef _MPI
    call MPI_Allgather(positive_error_code, 1, MPI_INTEGER, self%gather_buf, 1, MPI_INTEGER, self%comm)
    errored_rank = maxloc(self%gather_buf, dim = 1) - 1
    call MPI_Bcast(gathered_error_code, 1, MPI_INTEGER, errored_rank, self%comm)
    msg_length = len(internal_message)
    call MPI_Bcast(internal_message, msg_length, MPI_CHARACTER, errored_rank, self%comm)
#endif
    if(gathered_error_code /= 0_I4P) then 
#ifdef _MPI
      call self%error(trim(internal_message), routine=routine, error_code=gathered_error_code, errored_rank=errored_rank, is_fatal=is_fatal, file=file, line=line)
#else
      call self%error(trim(internal_message), routine=routine, error_code=gathered_error_code, is_fatal=is_fatal, file=file, line=line)
#endif
    endif
    if(present(gathered_code)) gathered_code = gathered_error_code
  end subroutine gather_error_codes

!-------------------------------------------------------------------------------------
  subroutine check_json_value(self, json, json_path, expected_type, routine, is_fatal, file, line, error)
!-------------------------------------------------------------------------------------
!< Checks presence and type of value in json file
!< 
!<```fortran
!< use json_module, only: json_file, json_string
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< type(json_file)  :: json
!< character(len=:), allocatable :: json_path 
!< integer(I4P) :: error
!< 
!< call logger%initialize()
!< call json%load_file('my_config.json')
!< json_path = 'logger.test.check_json_value'
!< call logger%check_json_value(json, json_path, json_string, error = error)
!< if(error == 0) call json%get(json_path,success)
!< call json%destroy()
!< call logger%finalize()
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),  intent(inout)           :: self             !< Logger
    class(json_file),         intent(inout)           :: json             !< JSON handle
    character(len=*),         intent(in)              :: json_path        !< Path to variable
    integer(I4P),             intent(in)              :: expected_type    !< Expected variable datatype
    character(len=*),         intent(in),   optional  :: routine          !< Internal subroutine name
    logical,                  intent(in),   optional  :: is_fatal         !< Fatal error. Default is .false.
    character(len=*),         intent(in),   optional  :: file             !< Source file name where json is being checked, e.g. \_\_FILE\_\_
    integer(I4P),             intent(in),   optional  :: line             !< Line where json is being checked, e.g. \_\_LINE\_\_
    integer(I4P),             intent(out),  optional  :: error            !< Error code
    character(len=:),                    allocatable  :: message          !< Error message
    integer(I4P)                                      :: recieved_type    !< Actual datatype found in JSON path
    integer(I4P)                                      :: error_code       !< Internal error code
    integer(I4P)                                      :: gathered_code    !< Aggregated internal error code
    logical                                           :: is_value_found   !< Is value found in JSON path flag
    
    
    message = ''
    error_code = 0_I4P
    call json%info(json_path, found = is_value_found, var_type = recieved_type)
    if(.not. is_value_found) then
      message = 'JSON Path "'//json_path//'"'//" doesn't exist;"
      error_code = LOGGER_ERROR_JSON_VALUE_NOT_FOUND
    elseif(expected_type /= recieved_type) then
      message = 'JSON Path "'//json_path//'": '//'expected datatype is '//colorize(trim(JSON_TYPES(expected_type)), color_fg='cyan')//     &
                      ', but found '//colorize(trim(JSON_TYPES(recieved_type)), color_fg='red')//' instead;'
      error_code = LOGGER_ERROR_JSON_VALUE_TYPE_MISMATCH
    endif
    call self%gather_error_codes(message, error_code, routine=routine, file=file, line=line, is_fatal=is_fatal, gathered_code=gathered_code)
    if(present(error)) error = gathered_code
  end subroutine check_json_value

!-------------------------------------------------------------------------------------
  subroutine check_directory(self, dir_path, is_create, routine, file, line, is_fatal, error)
!-------------------------------------------------------------------------------------
!< Checks directory presence in the specified path  
!< If directory doesn't exist, logger can create it.  
!< Attention! This subroutine is collective. All MPI Processes must call it.
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),  intent(inout)           :: self           !< Logger
    character(len = *),       intent(in)              :: dir_path       !< Path to directory
    logical,                  intent(in)              :: is_create      !< Create directory if it doesn't exist
    character(len = *),       intent(in),   optional  :: routine        !< Internal subroutine name
    character(len=*),         intent(in),   optional  :: file           !< Source file name where directory is being checked, e.g. \_\_FILE\_\_
    integer(I4P),             intent(in),   optional  :: line           !< Line where directory is being checked, e.g. \_\_LINE\_\_
    logical,                  intent(in),   optional  :: is_fatal       !< Fatal error. Default is .false.
    integer(I4P),             intent(out),  optional  :: error          !< Error code
    integer(I4P)                                      :: error_code     !< Internal error code
    integer(I4P)                                      :: gathered_code  !< Aggregated internal error code
    logical                                           :: exist          !< Is directory found flag

#ifdef __INTEL_COMPILER
    inquire(directory = dir_path, exist = exist)
#else
    inquire(file = dir_path, exist = exist)
#endif
    error_code = 0_I4P; if(.not. exist) error_code = LOGGER_ERROR_DIRECTORY_NOT_FOUND
    call self%gather_error_codes('Directory "'//dir_path//'"'//" doesn't exist;", error_code, routine=routine, file=file, line=line, &
                                  is_fatal=is_fatal, gathered_code=gathered_code)
    if(.not. exist .and. is_create) then
      ! TODO next info message should be generated by correct MPI rank
      call self%info('Creating directory "'//dir_path//'"', routine = routine)
      call execute_command_line('mkdir -p '//dir_path)
    endif
    if(present(error)) error = gathered_code
  end subroutine check_directory

!-------------------------------------------------------------------------------------
  subroutine check_file(self, file_path, routine, file, line, is_fatal, error)
!-------------------------------------------------------------------------------------
!< Checks file presence in the specified path
!< Attention! This subroutine is collective. All MPI Processes must call it.
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),  intent(inout)           :: self           !< Logger
    character(len = *),       intent(in)              :: file_path      !< Path to directory
    character(len = *),       intent(in),   optional  :: routine        !< Internal subroutine name
    character(len=*),         intent(in),   optional  :: file           !< Source file name where directory is being checked, e.g. \_\_FILE\_\_
    integer(I4P),             intent(in),   optional  :: line           !< Line where directory is being checked, e.g. \_\_LINE\_\_
    logical,                  intent(in),   optional  :: is_fatal       !< Fatal error. Default is .false.
    integer(I4P),             intent(out),  optional  :: error          !< Error code
    integer(I4P)                                      :: error_code     !< Internal error code
    integer(I4P)                                      :: gathered_code  !< Aggregated internal error code
    logical                                           :: exist          !< Is file found flag

    inquire(file = file_path, exist = exist)
    error_code = 0_I4P; if(.not. exist) error_code = LOGGER_ERROR_FILE_NOT_FOUND
    call self%gather_error_codes('File "'//file_path//'"'//" doesn't exist;", error_code, routine=routine, file=file, line=line, &
                                  is_fatal=is_fatal, gathered_code=gathered_code)
    if(present(error)) error = gathered_code
  end subroutine check_file

!-------------------------------------------------------------------------------------
  subroutine check_error_LOGICAL(self, check_routine, error_code, routine, file, line, is_fatal)
!-------------------------------------------------------------------------------------
!< Checks logical error_code returned by external subroutine.  
!< If error occurs and is_fatal = .true., program will stop execution.  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< logical :: status
!< call logger%initialize()
!< call external_subroutine(.., status)
!< call logger%check_error('external_subroutine', status)
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),  intent(inout)         :: self                 !< Logger
    character(len=*),         intent(in)            :: check_routine        !< Name of the external subroutine, e.g. MPI_Bcast
    logical,                  intent(in)            :: error_code           !< Error code returned by external subroutine  
    character(len = *),       intent(in), optional  :: routine              !< Internal subroutine name
    character(len=*),         intent(in), optional  :: file                 !< Source file name where check_error is being called, e.g. \_\_FILE\_\_
    integer(I4P),             intent(in), optional  :: line                 !< Line where check_error is being called, e.g. \_\_LINE\_\_
    logical,                  intent(in), optional  :: is_fatal             !< Fatal error. Default is .false.  
                                                                            !< Default is .false.
    integer(I4P)                                    :: converted_error_code !< Error code converted to I4P

    converted_error_code = LOGGER_ERROR_CONVERTED_LOGICAL; if(error_code) converted_error_code = 0_I4P
    call self%check_error(check_routine, converted_error_code, routine=routine, file=file, line=line, is_fatal=is_fatal)
  end subroutine check_error_LOGICAL

!-------------------------------------------------------------------------------------
  subroutine check_error_I1P(self, check_routine, error_code, routine, file, line, is_fatal)
!-------------------------------------------------------------------------------------
!< Checks integer I1P error_code returned by external subroutine.  
!< If error occurs and is_fatal = .true., program will stop execution.  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< integer(I1P) :: status
!< call logger%initialize()
!< call external_subroutine(.., status)
!< call logger%check_error('external_subroutine', status)
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),  intent(inout)         :: self           !< Logger
    character(len=*),         intent(in)            :: check_routine  !< Name of the external subroutine, e.g. MPI_Bcast
    integer(I1P),             intent(in)            :: error_code     !< Error code returned by external subroutine
    character(len=*),         intent(in), optional  :: routine        !< Internal subroutine name
    character(len=*),         intent(in), optional  :: file           !< Source file name where check_error is being called, e.g. \_\_FILE\_\_
    integer(I4P),             intent(in), optional  :: line           !< Line where check_error is being called, e.g. \_\_LINE\_\_
    logical,                  intent(in), optional  :: is_fatal       !< Flag to stop execution is error occurs  
                                                                      !< Default is .false.

    call self%gather_error_codes('Subroutine "'//check_routine//'" returned', error_code, routine=routine, file=file, line=line, is_fatal=is_fatal)
  end subroutine check_error_I1P

!-------------------------------------------------------------------------------------
  subroutine check_error_I2P(self, check_routine, error_code, routine, file, line, is_fatal)
!-------------------------------------------------------------------------------------
!< Checks integer I2P error_code returned by external subroutine.  
!< If error occurs and is_fatal = .true., program will stop execution.  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< integer(I2P) :: status
!< call logger%initialize()
!< call external_subroutine(.., status)
!< call logger%check_error('external_subroutine', status)
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),  intent(inout)         :: self           !< Logger
    character(len=*),         intent(in)            :: check_routine  !< Name of the external subroutine, e.g. MPI_Bcast
    integer(I2P),             intent(in)            :: error_code     !< Error code returned by external subroutine
    character(len=*),         intent(in), optional  :: routine        !< Internal subroutine name
    character(len=*),         intent(in), optional  :: file           !< Source file name where check_error is being called, e.g. \_\_FILE\_\_
    integer(I4P),             intent(in), optional  :: line           !< Line where check_error is being called, e.g. \_\_LINE\_\_
    logical,                  intent(in), optional  :: is_fatal       !< Flag to stop execution is error occurs  
                                                                      !< Default is .false.

    call self%gather_error_codes('Subroutine "'//check_routine//'" returned', error_code, routine=routine, file=file, line=line, is_fatal=is_fatal)
  end subroutine check_error_I2P

!-------------------------------------------------------------------------------------
  subroutine check_error_I4P(self, check_routine, error_code, routine, file, line, is_fatal)
!-------------------------------------------------------------------------------------
!< Checks integer I4P error_code returned by external subroutine.  
!< If error occurs and is_fatal = .true., program will stop execution.  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< integer(I4P) :: status
!< call logger%initialize()
!< call external_subroutine(.., status)
!< call logger%check_error('external_subroutine', status)
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),  intent(inout)         :: self           !< Logger
    character(len=*),         intent(in)            :: check_routine  !< Name of the external subroutine, e.g. MPI_Bcast
    integer(I4P),             intent(in)            :: error_code     !< Error code returned by external subroutine
    character(len=*),         intent(in), optional  :: routine        !< Internal subroutine name
    character(len=*),         intent(in), optional  :: file           !< Source file name where check_error is being called, e.g. \_\_FILE\_\_
    integer(I4P),             intent(in), optional  :: line           !< Line where check_error is being called, e.g. \_\_LINE\_\_
    logical,                  intent(in), optional  :: is_fatal       !< Flag to stop execution is error occurs  
                                                                      !< Default is .false.

    call self%gather_error_codes('Subroutine "'//check_routine//'" returned', error_code, routine=routine, file=file, line=line, is_fatal=is_fatal)
  end subroutine check_error_I4P

!-------------------------------------------------------------------------------------
  subroutine check_error_I8P(self, check_routine, error_code, routine, file, line, is_fatal)
!-------------------------------------------------------------------------------------
!< Checks integer I8P error_code returned by external subroutine.  
!< If error occurs and is_fatal = .true., program will stop execution.  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< integer(I8P) :: status
!< call logger%initialize()
!< call external_subroutine(.., status)
!< call logger%check_error('external_subroutine', status)
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),  intent(inout)         :: self           !< Logger
    character(len=*),         intent(in)            :: check_routine  !< Name of the external subroutine, e.g. MPI_Bcast
    integer(I8P),             intent(in)            :: error_code     !< Error code returned by external subroutine
    character(len=*),         intent(in), optional  :: routine        !< Internal subroutine name
    character(len=*),         intent(in), optional  :: file           !< Source file name where check_error is being called, e.g. \_\_FILE\_\_
    integer(I4P),             intent(in), optional  :: line           !< Line where check_error is being called, e.g. \_\_LINE\_\_
    logical,                  intent(in), optional  :: is_fatal       !< Flag to stop execution is error occurs  
                                                                      !< Default is .false.
    
    call self%gather_error_codes('Subroutine "'//check_routine//'" returned', error_code, routine=routine, file=file, line=line, is_fatal=is_fatal)
  end subroutine check_error_I8P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank1_CR4P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of complex R4P buffer of rank = 1  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< complex(R4P), allocatable :: my_buffer(:)
!< call logger%initialize()
!< allocate(my_buffer(10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self         !< Logger
    complex(R4P),    allocatable, intent(in)            :: buffer(:)    !< Buffer to be checked
    character(len=*),             intent(in)            :: buffer_name  !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine      !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file         !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line         !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code   !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank1_CR4P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank1_CR8P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of complex R8P buffer of rank = 1  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< complex(R8P), allocatable :: my_buffer(:)
!< call logger%initialize()
!< allocate(my_buffer(10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self         !< Logger
    complex(R8P),    allocatable, intent(in)            :: buffer(:)    !< Buffer to be checked
    character(len=*),             intent(in)            :: buffer_name  !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine      !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file         !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line         !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code   !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank1_CR8P

#ifdef _R16P
!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank1_CR16P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of complex R16P buffer of rank = 1  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< complex(R16P), allocatable :: my_buffer(:)
!< call logger%initialize()
!< allocate(my_buffer(10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self         !< Logger
    complex(R16P),   allocatable, intent(in)            :: buffer(:)    !< Buffer to be checked
    character(len=*),             intent(in)            :: buffer_name  !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine      !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file         !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line         !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code   !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank1_CR16P
#endif

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank2_CR4P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of complex R4P buffer of rank = 2  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< complex(R4P), allocatable :: my_buffer(:,:)
!< call logger%initialize()
!< allocate(my_buffer(10,10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self         !< Logger
    complex(R4P),    allocatable, intent(in)            :: buffer(:,:)  !< Buffer to be checked
    character(len=*),             intent(in)            :: buffer_name  !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine      !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file         !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line         !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code   !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank2_CR4P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank2_CR8P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of complex R8P buffer of rank = 2  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< complex(R8P), allocatable :: my_buffer(:,:)
!< call logger%initialize()
!< allocate(my_buffer(10,10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self         !< Logger
    complex(R8P),    allocatable, intent(in)            :: buffer(:,:)  !< Buffer to be checked
    character(len=*),             intent(in)            :: buffer_name  !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine      !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file         !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line         !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code   !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank2_CR8P

#ifdef _R16P
!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank2_CR16P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of complex R16P buffer of rank = 2  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< complex(R16P), allocatable :: my_buffer(:,:)
!< call logger%initialize()
!< allocate(my_buffer(10,10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self         !< Logger
    complex(R16P),   allocatable, intent(in)            :: buffer(:,:)  !< Buffer to be checked
    character(len=*),             intent(in)            :: buffer_name  !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine      !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file         !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line         !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code   !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank2_CR16P
#endif

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank3_CR4P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of complex R4P buffer of rank = 3  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< complex(R4P), allocatable :: my_buffer(:,:,:)
!< call logger%initialize()
!< allocate(my_buffer(10,10,10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self           !< Logger
    complex(R4P),    allocatable, intent(in)            :: buffer(:,:,:)  !< Buffer to be checked
    character(len = *),           intent(in)            :: buffer_name    !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine        !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file           !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line           !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code     !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank3_CR4P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank3_CR8P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of complex R8P buffer of rank = 3  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< complex(R8P), allocatable :: my_buffer(:,:,:)
!< call logger%initialize()
!< allocate(my_buffer(10,10,10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self           !< Logger
    complex(R8P),    allocatable, intent(in)            :: buffer(:,:,:)  !< Buffer to be checked
    character(len = *),           intent(in)            :: buffer_name    !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine        !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file           !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line           !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code     !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank3_CR8P

#ifdef _R16P
!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank3_CR16P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of complex R16P buffer of rank = 3  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< complex(R16P), allocatable :: my_buffer(:,:,:)
!< call logger%initialize()
!< allocate(my_buffer(10,10,10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self           !< Logger
    complex(R16P),   allocatable, intent(in)            :: buffer(:,:,:)  !< Buffer to be checked
    character(len = *),           intent(in)            :: buffer_name    !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine        !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file           !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line           !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code     !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank3_CR16P
#endif

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank4_CR4P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of complex R4P buffer of rank = 4  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< complex(R4P), allocatable :: my_buffer(:,:,:,:)
!< call logger%initialize()
!< allocate(my_buffer(10,10,10,10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self             !< Logger
    complex(R4P),    allocatable, intent(in)            :: buffer(:,:,:,:)  !< Buffer to be checked
    character(len = *),           intent(in)            :: buffer_name      !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine          !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file             !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line             !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code       !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank4_CR4P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank4_CR8P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of complex R8P buffer of rank = 4  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< complex(R8P), allocatable :: my_buffer(:,:,:,:)
!< call logger%initialize()
!< allocate(my_buffer(10,10,10,10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self             !< Logger
    complex(R8P),    allocatable, intent(in)            :: buffer(:,:,:,:)  !< Buffer to be checked
    character(len = *),           intent(in)            :: buffer_name      !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine          !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file             !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line             !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code       !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank4_CR8P

#ifdef _R16P
!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank4_CR16P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of complex R16P buffer of rank = 4  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< complex(R16P), allocatable :: my_buffer(:,:,:,:)
!< call logger%initialize()
!< allocate(my_buffer(10,10,10,10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self             !< Logger
    complex(R16P),   allocatable, intent(in)            :: buffer(:,:,:,:)  !< Buffer to be checked
    character(len = *),           intent(in)            :: buffer_name      !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine          !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file             !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line             !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code       !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank4_CR16P
#endif

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank1_R4P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of real R4P buffer of rank = 1  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< real(R4P), allocatable :: my_buffer(:)
!< call logger%initialize()
!< allocate(my_buffer(10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self         !< Logger
    real(R4P),       allocatable, intent(in)            :: buffer(:)    !< Buffer to be checked
    character(len=*),             intent(in)            :: buffer_name  !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine      !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file         !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line         !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code   !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank1_R4P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank1_R8P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of real R8P buffer of rank = 1  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< real(R8P), allocatable :: my_buffer(:)
!< call logger%initialize()
!< allocate(my_buffer(10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self         !< Logger
    real(R8P),       allocatable, intent(in)            :: buffer(:)    !< Buffer to be checked
    character(len=*),             intent(in)            :: buffer_name  !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine      !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file         !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line         !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code   !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank1_R8P

#ifdef _R16P
!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank1_R16P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of real R16P buffer of rank = 1  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< real(R16P), allocatable :: my_buffer(:)
!< call logger%initialize()
!< allocate(my_buffer(10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self         !< Logger
    real(R16P),      allocatable, intent(in)            :: buffer(:)    !< Buffer to be checked
    character(len=*),             intent(in)            :: buffer_name  !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine      !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file         !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line         !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code   !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank1_R16P
#endif

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank2_R4P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of real R4P buffer of rank = 2  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< real(R4P), allocatable :: my_buffer(:,:)
!< call logger%initialize()
!< allocate(my_buffer(10,10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self         !< Logger
    real(R4P),       allocatable, intent(in)            :: buffer(:,:)  !< Buffer to be checked
    character(len=*),             intent(in)            :: buffer_name  !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine      !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file         !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line         !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code   !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank2_R4P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank2_R8P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of real R8P buffer of rank = 2  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< real(R8P), allocatable :: my_buffer(:,:)
!< call logger%initialize()
!< allocate(my_buffer(10,10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self         !< Logger
    real(R8P),       allocatable, intent(in)            :: buffer(:,:)  !< Buffer to be checked
    character(len=*),             intent(in)            :: buffer_name  !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine      !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file         !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line         !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code   !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank2_R8P

#ifdef _R16P
!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank2_R16P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of real R16P buffer of rank = 2  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< real(R16P), allocatable :: my_buffer(:,:)
!< call logger%initialize()
!< allocate(my_buffer(10,10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self         !< Logger
    real(R16P),      allocatable, intent(in)            :: buffer(:,:)  !< Buffer to be checked
    character(len=*),             intent(in)            :: buffer_name  !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine      !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file         !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line         !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code   !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank2_R16P
#endif

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank3_R4P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of real R4P buffer of rank = 3  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< real(R4P), allocatable :: my_buffer(:,:,:)
!< call logger%initialize()
!< allocate(my_buffer(10,10,10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self           !< Logger
    real(R4P),       allocatable, intent(in)            :: buffer(:,:,:)  !< Buffer to be checked
    character(len = *),           intent(in)            :: buffer_name    !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine        !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file           !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line           !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code     !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank3_R4P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank3_R8P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of real R8P buffer of rank = 3  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< real(R8P), allocatable :: my_buffer(:,:,:)
!< call logger%initialize()
!< allocate(my_buffer(10,10,10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self           !< Logger
    real(R8P),       allocatable, intent(in)            :: buffer(:,:,:)  !< Buffer to be checked
    character(len = *),           intent(in)            :: buffer_name    !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine        !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file           !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line           !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code     !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank3_R8P

#ifdef _R16P
!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank3_R16P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of real R16P buffer of rank = 3  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< real(R16P), allocatable :: my_buffer(:,:,:)
!< call logger%initialize()
!< allocate(my_buffer(10,10,10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self           !< Logger
    real(R16P),      allocatable, intent(in)            :: buffer(:,:,:)  !< Buffer to be checked
    character(len = *),           intent(in)            :: buffer_name    !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine        !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file           !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line           !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code     !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank3_R16P
#endif

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank4_R4P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of real R4P buffer of rank = 4  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< real(R4P), allocatable :: my_buffer(:,:,:,:)
!< call logger%initialize()
!< allocate(my_buffer(10,10,10,10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self             !< Logger
    real(R4P),       allocatable, intent(in)            :: buffer(:,:,:,:)  !< Buffer to be checked
    character(len = *),           intent(in)            :: buffer_name      !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine          !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file             !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line             !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code       !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank4_R4P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank4_R8P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of real R8P buffer of rank = 4  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< real(R8P), allocatable :: my_buffer(:,:,:,:)
!< call logger%initialize()
!< allocate(my_buffer(10,10,10,10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self             !< Logger
    real(R8P),       allocatable, intent(in)            :: buffer(:,:,:,:)  !< Buffer to be checked
    character(len = *),           intent(in)            :: buffer_name      !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine          !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file             !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line             !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code       !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank4_R8P

#ifdef _R16P
!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank4_R16P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of real R16P buffer of rank = 4  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< real(R16P), allocatable :: my_buffer(:,:,:,:)
!< call logger%initialize()
!< allocate(my_buffer(10,10,10,10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self             !< Logger
    real(R16P),      allocatable, intent(in)            :: buffer(:,:,:,:)  !< Buffer to be checked
    character(len = *),           intent(in)            :: buffer_name      !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine          !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file             !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line             !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code       !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank4_R16P
#endif

!-------------------------------------------------------------------------------------
subroutine check_alloc_rank1_I1P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of integer I1P buffer of rank = 1  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< integer(I1P), allocatable :: my_buffer(:)
!< call logger%initialize()
!< allocate(my_buffer(10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self         !< Logger
    integer(I1P),    allocatable, intent(in)            :: buffer(:)    !< Buffer to be checked
    character(len=*),             intent(in)            :: buffer_name  !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine      !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file         !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line         !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code   !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank1_I1P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank1_I2P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of integer I2P buffer of rank = 1  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< integer(I2P), allocatable :: my_buffer(:)
!< call logger%initialize()
!< allocate(my_buffer(10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self         !< Logger
    integer(I2P),    allocatable, intent(in)            :: buffer(:)    !< Buffer to be checked
    character(len=*),             intent(in)            :: buffer_name  !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine      !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file         !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line         !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code   !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank1_I2P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank1_I4P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of integer I4P buffer of rank = 1  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< integer(I4P), allocatable :: my_buffer(:)
!< call logger%initialize()
!< allocate(my_buffer(10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self         !< Logger
    integer(I4P),    allocatable, intent(in)            :: buffer(:)    !< Buffer to be checked
    character(len=*),             intent(in)            :: buffer_name  !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine      !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file         !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line         !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code   !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank1_I4P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank1_I8P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of integer I8P buffer of rank = 1  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< integer(I8P), allocatable :: my_buffer(:)
!< call logger%initialize()
!< allocate(my_buffer(10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self         !< Logger
    integer(I8P),    allocatable, intent(in)            :: buffer(:)    !< Buffer to be checked
    character(len=*),             intent(in)            :: buffer_name  !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine      !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file         !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line         !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code   !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank1_I8P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank2_I1P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of integer I1P buffer of rank = 2  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< integer(I1P), allocatable :: my_buffer(:,:)
!< call logger%initialize()
!< allocate(my_buffer(10,10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self         !< Logger
    integer(I1P),    allocatable, intent(in)            :: buffer(:,:)  !< Buffer to be checked
    character(len=*),             intent(in)            :: buffer_name  !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine      !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file         !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line         !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code   !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank2_I1P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank2_I2P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of integer I2P buffer of rank = 2  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< integer(I2P), allocatable :: my_buffer(:,:)
!< call logger%initialize()
!< allocate(my_buffer(10,10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self         !< Logger
    integer(I2P),    allocatable, intent(in)            :: buffer(:,:)  !< Buffer to be checked
    character(len=*),             intent(in)            :: buffer_name  !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine      !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file         !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line         !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code   !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank2_I2P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank2_I4P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of integer I4P buffer of rank = 2  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< integer(I4P), allocatable :: my_buffer(:,:)
!< call logger%initialize()
!< allocate(my_buffer(10,10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self         !< Logger
    integer(I4P),    allocatable, intent(in)            :: buffer(:,:)  !< Buffer to be checked
    character(len=*),             intent(in)            :: buffer_name  !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine      !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file         !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line         !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code   !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank2_I4P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank2_I8P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of integer I8P buffer of rank = 2  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< integer(I8P), allocatable :: my_buffer(:,:)
!< call logger%initialize()
!< allocate(my_buffer(10,10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self         !< Logger
    integer(I8P),    allocatable, intent(in)            :: buffer(:,:)  !< Buffer to be checked
    character(len=*),             intent(in)            :: buffer_name  !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine      !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file         !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line         !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code   !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank2_I8P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank3_I1P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of integer I1P buffer of rank = 3  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< integer(I1P), allocatable :: my_buffer(:,:,:)
!< call logger%initialize()
!< allocate(my_buffer(10,10,10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self           !< Logger
    integer(I1P),    allocatable, intent(in)            :: buffer(:,:,:)  !< Buffer to be checked
    character(len = *),           intent(in)            :: buffer_name    !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine        !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file           !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line           !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code     !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank3_I1P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank3_I2P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of integer I2P buffer of rank = 3  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< integer(I2P), allocatable :: my_buffer(:,:,:)
!< call logger%initialize()
!< allocate(my_buffer(10,10,10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self           !< Logger
    integer(I2P),    allocatable, intent(in)            :: buffer(:,:,:)  !< Buffer to be checked
    character(len = *),           intent(in)            :: buffer_name    !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine        !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file           !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line           !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code     !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank3_I2P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank3_I4P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of integer I4P buffer of rank = 3  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< integer(I4P), allocatable :: my_buffer(:,:,:)
!< call logger%initialize()
!< allocate(my_buffer(10,10,10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self           !< Logger
    integer(I4P),    allocatable, intent(in)            :: buffer(:,:,:)  !< Buffer to be checked
    character(len = *),           intent(in)            :: buffer_name    !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine        !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file           !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line           !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code     !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank3_I4P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank3_I8P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of integer I8P buffer of rank = 3  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< integer(I8P), allocatable :: my_buffer(:,:,:)
!< call logger%initialize()
!< allocate(my_buffer(10,10,10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self           !< Logger
    integer(I8P),    allocatable, intent(in)            :: buffer(:,:,:)  !< Buffer to be checked
    character(len = *),           intent(in)            :: buffer_name    !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine        !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file           !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line           !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code     !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank3_I8P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank4_I1P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of integer I1P buffer of rank = 4  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< integer(I1P), allocatable :: my_buffer(:,:,:,:)
!< call logger%initialize()
!< allocate(my_buffer(10,10,10,10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self             !< Logger
    integer(I1P),    allocatable, intent(in)            :: buffer(:,:,:,:)  !< Buffer to be checked
    character(len = *),           intent(in)            :: buffer_name      !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine          !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file             !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line             !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code       !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank4_I1P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank4_I2P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of integer I2P buffer of rank = 4  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< integer(I2P), allocatable :: my_buffer(:,:,:,:)
!< call logger%initialize()
!< allocate(my_buffer(10,10,10,10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self             !< Logger
    integer(I2P),    allocatable, intent(in)            :: buffer(:,:,:,:)  !< Buffer to be checked
    character(len = *),           intent(in)            :: buffer_name      !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine          !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file             !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line             !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code       !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank4_I2P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank4_I4P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of integer I4P buffer of rank = 4  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< integer(I4P), allocatable :: my_buffer(:,:,:,:)
!< call logger%initialize()
!< allocate(my_buffer(10,10,10,10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self             !< Logger
    integer(I4P),    allocatable, intent(in)            :: buffer(:,:,:,:)  !< Buffer to be checked
    character(len = *),           intent(in)            :: buffer_name      !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine          !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file             !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line             !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code       !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank4_I4P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank4_I8P(self, buffer, buffer_name, routine, file, line)
!-------------------------------------------------------------------------------------
!< Checks allocation of integer I8P buffer of rank = 4  
!< Program will stop execution if buffer was not allocated  
!< Attention! This subroutine is collective. All MPI Processes must call it. 
!< 
!<```fortran
!< use fortran_logger
!< use penf
!< type(fortran_logger_t) :: logger
!< integer(I8P), allocatable :: my_buffer(:,:,:,:)
!< call logger%initialize()
!< allocate(my_buffer(10,10,10,10))
!< call logger%check_alloc(my_buffer, 'my_buffer')
!<```
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),      intent(inout)         :: self             !< Logger
    integer(I8P),    allocatable, intent(in)            :: buffer(:,:,:,:)  !< Buffer to be checked
    character(len = *),           intent(in)            :: buffer_name      !< Name of the Buffer
    character(len=*),             intent(in), optional  :: routine          !< Internal subroutine name
    character(len=*),             intent(in), optional  :: file             !< Source file name where check_alloc is being called, e.g. \_\_FILE\_\_
    integer(I4P),                 intent(in), optional  :: line             !< Line where check_alloc is being called, e.g. \_\_LINE\_\_
    integer(I4P)                                        :: error_code       !< logger error code

    error_code = 0_I4P; if(.not. allocated(buffer)) error_code = LOGGER_ERROR_ALLOCATION_FAILED
    call self%gather_error_codes('Allocation of "'//buffer_name//'" failed;', error_code, routine=routine, file=file, line=line, is_fatal=.true.)
  end subroutine check_alloc_rank4_I8P

!-------------------------------------------------------------------------------------
#ifdef _MPI
  subroutine print(self, level, message, routine, line, file, error_code, rank)
#else
  subroutine print(self, level, message, routine, line, file, error_code)
#endif
!-------------------------------------------------------------------------------------
!< Logger printer
!-------------------------------------------------------------------------------------
    class(fortran_logger_t), intent(inout)        :: self         !< Logger
    integer(I4P),            intent(in)           :: level        !< Level of the message
    character(len=*),        intent(in)           :: message      !< Message to be printed
    character(len=*),        intent(in), optional :: routine      !< Internal subroutine name
    character(len=*),        intent(in), optional :: file         !< Source file name
    integer(I4P),            intent(in), optional :: line         !< Source file line
    integer(I4P),            intent(in), optional :: error_code   !< Occured error code
#ifdef _MPI
    integer(I4P),            intent(in), optional :: rank         !< MPI rank
#endif
    character(len=:),        allocatable          :: buffer       !< Message buffer
    type(datetime)                                :: dtime        !< datetime object

    buffer = ''
    if(self%print_timestamp) then
      dtime = dtime%now()
      buffer = buffer//self%timestamp_object%output(dtime%strftime(self%timestamp_format))
    endif

    buffer = buffer//self%logger_object(level)%output()
    if(present(routine)) buffer = buffer//self%routine_object%output(routine)

    buffer = buffer//'  '//message
    if(present(error_code)) buffer = buffer//' error code '//trim(str(error_code))
#ifdef _MPI
    if(present(rank)) buffer = buffer//' on rank '//trim(str(n=rank, no_sign=.true.))
#endif
    if(present(file).or.present(line)) then
      buffer = buffer//' ('
      if(present(file)) buffer = buffer//'file: "'//file//'"'
      if(present(line)) then
        if(present(file)) buffer = buffer//', '
        buffer = buffer//'line: '//trim(str(line, no_sign=.true.))
      endif
      buffer = buffer//')'
    endif

    write(self%logger_object(level)%get_unit(), '(a)') buffer
    flush(self%logger_object(level)%get_unit())
  end subroutine print

end module fortran_logger