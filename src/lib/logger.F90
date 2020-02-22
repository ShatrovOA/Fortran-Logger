module fortran_logger_m
use iso_fortran_env, only: stderr => error_unit, stdout => output_unit
use penf
use face, only : colorize
use flap
use datetime_module, only: datetime
#ifdef _MPI
use mpi_f08, only : MPI_Comm, MPI_Comm_size, MPI_Allgather, MPI_INTEGER, MPI_COMM_WORLD
#endif
implicit none
private

  character(len = 5), parameter :: info_msg  = 'INFO '
  character(len = 5), parameter :: error_msg = 'ERROR'
  character(len = 5), parameter :: warn_msg  = 'WARN '
  character(len = 5), parameter :: debug_msg = 'DEBUG'

  type, public :: fortran_logger

    logical :: print_timestamp = .true.
    logical :: use_log_file = .false.

    character(len = :), allocatable :: debug_color
    character(len = :), allocatable :: info_color
    character(len = :), allocatable :: warn_color
    character(len = :), allocatable :: error_color
    
    character(len = :), allocatable :: log_file

    character(len = :), allocatable :: c_info_msg
    character(len = :), allocatable :: c_error_msg
    character(len = :), allocatable :: c_warn_msg
    character(len = :), allocatable :: c_debug_msg

    character(len = :), allocatable :: timestamp_format


    integer(I4P) :: debug_unit = stdout
    integer(I4P) :: info_unit = stdout
    integer(I4P) :: error_unit  = stderr
    integer(I4P) :: null_unit
    integer(I4P) :: logger_unit

    integer(I4P) :: log_level = 0

#ifdef _MPI
    integer(I4P) :: np
    integer(I4P), allocatable :: tmp(:)
    type(MPI_Comm) :: comm
#endif

    integer(I4P) :: ierror = 0
  contains
    private
    procedure, pass(self),  public :: initialize
#ifdef _MPI
    procedure, pass(self),  public :: mpi_init
#endif
    procedure, pass(self),  public :: debug
    procedure, pass(self),  public :: info
    procedure, pass(self),  public :: warn
    procedure, pass(self),  public :: check_error
    procedure, pass(self),  public :: error
    procedure, pass(self),  public :: finalize
    generic, public ::  check_alloc =>           &
                        check_alloc_rank1_cR8P,  &
                        check_alloc_rank1_cR4P,  &
                        check_alloc_rank1_R8P,   &
                        check_alloc_rank1_R4P,   &
                        check_alloc_rank1_I8P,   &
                        check_alloc_rank1_I4P,   &
                        check_alloc_rank1_I2P,   &
                        check_alloc_rank1_I1P,   &
                        check_alloc_rank2_cR8P,  &
                        check_alloc_rank2_cR4P,  &
                        check_alloc_rank2_R8P,   &
                        check_alloc_rank2_R4P,   &
                        check_alloc_rank2_I8P,   &
                        check_alloc_rank2_I4P,   &
                        check_alloc_rank2_I2P,   &
                        check_alloc_rank2_I1P,   &
                        check_alloc_rank3_cR8P,  &
                        check_alloc_rank3_cR4P,  &
                        check_alloc_rank3_R8P,   &
                        check_alloc_rank3_R4P,   &
                        check_alloc_rank3_I8P,   &
                        check_alloc_rank3_I4P,   &
                        check_alloc_rank3_I2P,   &
                        check_alloc_rank3_I1P,   &
                        check_alloc_rank4_cR8P,  &
                        check_alloc_rank4_cR4P,  &
                        check_alloc_rank4_R8P,   &
                        check_alloc_rank4_R4P,   &
                        check_alloc_rank4_I8P,   &
                        check_alloc_rank4_I4P,   &
                        check_alloc_rank4_I2P,   &
                        check_alloc_rank4_I1P   
    procedure, pass(self) :: check_alloc_rank1_R8P
    procedure, pass(self) :: check_alloc_rank1_R4P
    procedure, pass(self) :: check_alloc_rank1_cR8P
    procedure, pass(self) :: check_alloc_rank1_cR4P
    procedure, pass(self) :: check_alloc_rank1_I8P
    procedure, pass(self) :: check_alloc_rank1_I4P
    procedure, pass(self) :: check_alloc_rank1_I2P
    procedure, pass(self) :: check_alloc_rank1_I1P
    procedure, pass(self) :: check_alloc_rank2_R8P
    procedure, pass(self) :: check_alloc_rank2_R4P
    procedure, pass(self) :: check_alloc_rank2_cR8P
    procedure, pass(self) :: check_alloc_rank2_cR4P
    procedure, pass(self) :: check_alloc_rank2_I8P
    procedure, pass(self) :: check_alloc_rank2_I4P
    procedure, pass(self) :: check_alloc_rank2_I2P
    procedure, pass(self) :: check_alloc_rank2_I1P
    procedure, pass(self) :: check_alloc_rank3_R8P
    procedure, pass(self) :: check_alloc_rank3_R4P
    procedure, pass(self) :: check_alloc_rank3_cR8P
    procedure, pass(self) :: check_alloc_rank3_cR4P
    procedure, pass(self) :: check_alloc_rank3_I8P
    procedure, pass(self) :: check_alloc_rank3_I4P
    procedure, pass(self) :: check_alloc_rank3_I2P
    procedure, pass(self) :: check_alloc_rank3_I1P
    procedure, pass(self) :: check_alloc_rank4_R8P
    procedure, pass(self) :: check_alloc_rank4_R4P
    procedure, pass(self) :: check_alloc_rank4_cR8P
    procedure, pass(self) :: check_alloc_rank4_cR4P
    procedure, pass(self) :: check_alloc_rank4_I8P
    procedure, pass(self) :: check_alloc_rank4_I4P
    procedure, pass(self) :: check_alloc_rank4_I2P
    procedure, pass(self) :: check_alloc_rank4_I1P
    procedure, pass(self) :: set_default_values
    procedure, pass(self) :: open_scratch_file
    procedure, pass(self) :: close_scratch_file
    procedure, pass(self) :: write_message
    procedure, pass(self) :: gather
    procedure, pass(self) :: checkerr
  end type fortran_logger

  interface get_passed_value
    module procedure :: get_integer
    module procedure :: get_string
  end interface get_passed_value
    
contains

  subroutine initialize(self, log_level, info_color, error_color, debug_color, warn_color,    &
                                         info_unit, error_unit, debug_unit, warn_unit,        &
                                         print_timestamp, timestamp_format, log_file          )
    class(fortran_logger),        intent(inout) :: self
    integer(I4P),       optional, intent(in)    :: log_level
    character(len = *), optional, intent(in)    :: info_color
    character(len = *), optional, intent(in)    :: error_color
    character(len = *), optional, intent(in)    :: debug_color
    character(len = *), optional, intent(in)    :: warn_color
    integer(I4P),       optional, intent(in)    :: info_unit
    integer(I4P),       optional, intent(in)    :: error_unit
    integer(I4P),       optional, intent(in)    :: debug_unit
    integer(I4P),       optional, intent(in)    :: warn_unit
    logical,            optional, intent(in)    :: print_timestamp
    character(len = *), optional, intent(in)    :: timestamp_format
    character(len = *), optional, intent(in)    :: log_file    
    type(command_line_interface)    :: cli !< CLI
    character(len = :), allocatable :: logger_file

    call self%set_default_values()

    if(present(log_level))    self%log_level = log_level
    if(present(info_color))   self%info_color   = info_color
    if(present(error_color))  self%error_color  = error_color
    if(present(debug_color)) self%debug_color  = debug_color
    if(present(warn_color))  self%warn_color  = warn_color
    if(present(info_unit)) self%info_unit = info_unit
    if(present(error_unit)) self%error_unit = error_unit
    if(present(debug_unit)) self%debug_unit = debug_unit
    if(present(warn_unit)) self%warn_unit = warn_unit
    if(present(print_timestamp)) self%print_timestamp = print_timestamp
    if(present(timestamp_format)) self%timestamp_format = timestamp_format
    if(present(log_file)) logger_file = log_file

    self%c_info_msg  = colorize(info_msg,  color_fg = self%info_color )
    self%c_error_msg = colorize(error_msg, color_fg = self%error_color)
    self%c_debug_msg = colorize(debug_msg, color_fg = self%debug_color)
    self%c_warn_msg  = colorize(warn_msg,  color_fg = self%warn_color )

    call self%open_scratch_file()

    call cli%init(ignore_unknown_clas = .true.,   &
                  usage_lun = self%null_unit,     &
                  error_lun  = self%null_unit,    &
                  version_lun = self%null_unit    )
    call cli%add( switch = '--logger',            &
                  switch_ab = '-log',             &
                  required = .false.,             &
                  def = '0',                      &
                  act='store'                     )
    call cli%add( switch = '--logger_file',       &
                  switch_ab = '-log_file',        &
                  required = .false.,             &
                  def = 'log.log',                &
                  act='store'                     )

    call cli%parse(error = self%ierror)

    call get_passed_value(cli = cli, switch = '-log',   val = self%log_level, ierror = self%ierror)
    call get_passed_value(cli = cli, switch = '-log_file', val = logger_file, ierror = self%ierror)

    call cli%free()

    if(allocated(logger_file)) then
      self%use_log_file = .true.
      open(newunit = self%logger_unit,            &
            file = logger_file,                   &
            action = 'write',                     &
            status='replace',                     &
            iostat = self%ierror                  )
    endif

  end subroutine initialize

#ifdef _MPI
  subroutine mpi_init(self, comm)
    class(fortran_logger),    intent(inout) :: self
    type(MPI_Comm), optional, intent(in)    :: comm

    if(present(comm)) then
      self%comm = comm
    else
      self%comm = MPI_COMM_WORLD
    endif

    call MPI_Comm_size(self%comm, self%np, self%ierror)

    allocate(self%tmp(0:self%np - 1))

  end subroutine mpi_init

  subroutine gather(self, err, pos)
    class(fortran_logger),  intent(inout) :: self
    integer(I4P),           intent(inout) :: err
    integer(I4P),           intent(out)   :: pos

    call MPI_Allgather(err, 1, MPI_INTEGER, self%tmp, 1, MPI_INTEGER, self%comm, self%ierror)
    err = maxval(self%tmp, dim = 1)
    pos = maxloc(self%tmp, dim = 1) - 1
    
  end subroutine gather
#endif

  subroutine get_integer(cli, switch, val, ierror)
    class(command_line_interface),  intent(inout) :: cli
    character(len = *),             intent(in)    :: switch
    integer(I4P),                   intent(out)   :: val
    integer(I4P),                   intent(out)   :: ierror

    if(cli%is_passed(switch = switch)) then
      call cli%get(switch = switch,   val = val,  error = ierror)
    endif

  end subroutine get_integer

  subroutine get_string(cli, switch, val, ierror)
    class(command_line_interface),    intent(inout) :: cli
    character(len = *),               intent(in)    :: switch
    character(len = :), allocatable,  intent(out)   :: val
    integer(I4P),                     intent(out)   :: ierror
    character(len = 500) :: string

    if(cli%is_passed(switch = switch)) then
      call cli%get(switch = switch,   val = string,  error = ierror)
      val = trim(string)
    endif

  end subroutine get_string

  subroutine finalize(self)
    class(fortran_logger),  intent(inout) :: self

    call self%close_scratch_file()

    if(self%use_log_file) close(self%logger_unit)

  end subroutine finalize

  function current_timestamp() result(timestamp)
    character(len = :), allocatable :: timestamp
    integer(I4P) :: ts(8)
    type(datetime) :: dtime
    character(3) :: day_of_week, month
    

    dtime = dtime%now()
    ! day_of_week = dtime%isoweekdayShort()

    timestamp = dtime%strftime('%c')

    ! timestamp = day_of_week//', '//months(ts(2))//' '//trim(str(n = ts(3),no_sign=.true.))//', '//     &
    !                                 trim(str(n = ts(5),no_sign=.true.))//':'//                         &
    !                                 trim(str(n = ts(6),no_sign=.true.))//':'//                         &
    !                                 trim(str(n = ts(7),no_sign=.true.))


  end function current_timestamp

  subroutine set_default_values(self)
    class(fortran_logger), intent(inout) :: self

    self%info_color   = 'GREEN'
    self%error_color  = 'RED_INTENSE'
    self%debug_color  = 'CYAN'
    self%warn_color  = 'YELLOW'
    self%timestamp_format = '%c'

  end subroutine set_default_values

  subroutine debug(self, routine, message)
    class(fortran_logger),        intent(inout) :: self
    character(len = *),           intent(in)    :: message
    character(len = *), optional, intent(in)    :: routine

    if(self%log_level == 4) then
      call self%write_message(self%debug_unit, prefix = self%c_debug_msg, message = message, routine = routine)
      if(self%use_log_file)  call self%write_message(self%logger_unit, prefix = debug_msg, message = message)
    endif
  
  end subroutine debug

  subroutine info(self, routine, message)
    class(fortran_logger),        intent(inout) :: self
    character(len = *),           intent(in)    :: message
    character(len = *), optional, intent(in)    :: routine

    if(self%log_level >= 3) then
      call self%write_message(self%info_unit, prefix = self%c_info_msg, message = message, routine = routine)
    endif
  
  end subroutine info

  subroutine warn(self, routine, message)
    class(fortran_logger),        intent(inout) :: self
    character(len = *),           intent(in)    :: message
    character(len = *), optional, intent(in)    :: routine

    if(self%log_level >= 2) then
      call self%write_message(self%error_unit, prefix = self%c_warn_msg, message = message, routine = routine)
    endif
  
  end subroutine warn

  subroutine check_error(self, check_routine, err, routine, is_fatal)
    class(fortran_logger),        intent(inout) :: self
    character(len = *),           intent(in)    :: check_routine
    integer(I4P),                 intent(in)    :: err
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    logical,            optional, intent(in)    :: is_fatal     !< Fatal error. Default is .false.

    call self%checkerr(message = 'Subroutine '//check_routine//' returned error code:',  &
                       err = err, routine = routine, is_fatal = is_fatal                  )

  end subroutine check_error

  subroutine checkerr(self, message, err, routine, is_fatal)
    class(fortran_logger),        intent(inout) :: self
    character(len = *),           intent(in)    :: message
    integer(I4P),                 intent(in)    :: err
    character(len = *), optional, intent(in)    :: routine  !< Tracing routine
    logical,            optional, intent(in)    :: is_fatal !< Fatal error. Default is .false.
    integer(I4P) :: ierr, pos

    ierr = err

#ifdef _MPI
    call self%gather(ierr, pos)
#endif

    if(ierr /= 0) then
      call self%error(message = message, routine = routine, err = ierr, rank = pos, is_fatal = is_fatal)
    endif

  end subroutine checkerr

  subroutine check_alloc_rank1_cR8P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self         !< Logger
    complex(R8P),    allocatable, intent(in)    :: check(:)     !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank1_cR8P

  subroutine check_alloc_rank1_cR4P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self         !< Logger
    complex(R4P),    allocatable, intent(in)    :: check(:)     !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank1_cR4P

  subroutine check_alloc_rank1_R8P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self         !< Logger
    real(R8P),       allocatable, intent(in)    :: check(:)     !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank1_R8P

  subroutine check_alloc_rank1_R4P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self         !< Logger
    real(R4P),       allocatable, intent(in)    :: check(:)     !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank1_R4P

  subroutine check_alloc_rank1_I8P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self         !< Logger
    integer(I8P),    allocatable, intent(in)    :: check(:)     !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank1_I8P

  subroutine check_alloc_rank1_I4P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self         !< Logger
    integer(I4P),    allocatable, intent(in)    :: check(:)     !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank1_I4P

  subroutine check_alloc_rank1_I2P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self         !< Logger
    integer(I2P),    allocatable, intent(in)    :: check(:)     !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank1_I2P

  subroutine check_alloc_rank1_I1P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self         !< Logger
    integer(I1P),    allocatable, intent(in)    :: check(:)     !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank1_I1P

  subroutine check_alloc_rank2_R8P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self         !< Logger
    real(R8P),       allocatable, intent(in)    :: check(:,:)   !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank2_R8P

  subroutine check_alloc_rank2_R4P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self         !< Logger
    real(R4P),       allocatable, intent(in)    :: check(:,:)   !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank2_R4P

  subroutine check_alloc_rank2_cR8P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self         !< Logger
    complex(R8P),    allocatable, intent(in)    :: check(:,:)   !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank2_cR8P

  subroutine check_alloc_rank2_cR4P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self         !< Logger
    complex(R4P),    allocatable, intent(in)    :: check(:,:)   !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank2_cR4P

  subroutine check_alloc_rank2_I8P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self         !< Logger
    integer(I8P),    allocatable, intent(in)    :: check(:,:)   !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank2_I8P

  subroutine check_alloc_rank2_I4P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self         !< Logger
    integer(I4P),    allocatable, intent(in)    :: check(:,:)   !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank2_I4P

  subroutine check_alloc_rank2_I2P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self         !< Logger
    integer(I2P),    allocatable, intent(in)    :: check(:,:)   !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank2_I2P

  subroutine check_alloc_rank2_I1P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self         !< Logger
    integer(I1P),    allocatable, intent(in)    :: check(:,:)   !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank2_I1P

  subroutine check_alloc_rank3_R8P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self         !< Logger
    real(R8P),       allocatable, intent(in)    :: check(:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank3_R8P

  subroutine check_alloc_rank3_R4P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self         !< Logger
    real(R4P),       allocatable, intent(in)    :: check(:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank3_R4P

  subroutine check_alloc_rank3_cR8P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self         !< Logger
    complex(R8P),    allocatable, intent(in)    :: check(:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank3_cR8P

  subroutine check_alloc_rank3_cR4P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self         !< Logger
    complex(R4P),    allocatable, intent(in)    :: check(:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank3_cR4P

  subroutine check_alloc_rank3_I8P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self         !< Logger
    integer(I8P),    allocatable, intent(in)    :: check(:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank3_I8P

  subroutine check_alloc_rank3_I4P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self         !< Logger
    integer(I4P),    allocatable, intent(in)    :: check(:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank3_I4P

  subroutine check_alloc_rank3_I2P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self         !< Logger
    integer(I2P),    allocatable, intent(in)    :: check(:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank3_I2P

  subroutine check_alloc_rank3_I1P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self         !< Logger
    integer(I1P),    allocatable, intent(in)    :: check(:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank3_I1P

  subroutine check_alloc_rank4_R8P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self           !< Logger
    real(R8P),       allocatable, intent(in)    :: check(:,:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name     !< Name of the array
    character(len = *), optional, intent(in)    :: routine        !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank4_R8P

  subroutine check_alloc_rank4_R4P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self           !< Logger
    real(R4P),       allocatable, intent(in)    :: check(:,:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name     !< Name of the array
    character(len = *), optional, intent(in)    :: routine        !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank4_R4P

  subroutine check_alloc_rank4_cR8P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self           !< Logger
    complex(R8P),    allocatable, intent(in)    :: check(:,:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name     !< Name of the array
    character(len = *), optional, intent(in)    :: routine        !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank4_cR8P

  subroutine check_alloc_rank4_cR4P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self           !< Logger
    complex(R4P),    allocatable, intent(in)    :: check(:,:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name     !< Name of the array
    character(len = *), optional, intent(in)    :: routine        !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank4_cR4P

  subroutine check_alloc_rank4_I8P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self           !< Logger
    integer(I8P),    allocatable, intent(in)    :: check(:,:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name     !< Name of the array
    character(len = *), optional, intent(in)    :: routine        !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank4_I8P

  subroutine check_alloc_rank4_I4P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self           !< Logger
    integer(I4P),    allocatable, intent(in)    :: check(:,:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name     !< Name of the array
    character(len = *), optional, intent(in)    :: routine        !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank4_I4P

  subroutine check_alloc_rank4_I2P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self           !< Logger
    integer(I2P),    allocatable, intent(in)    :: check(:,:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name     !< Name of the array
    character(len = *), optional, intent(in)    :: routine        !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank4_I2P

  subroutine check_alloc_rank4_I1P(self, check, check_name, routine)
    class(fortran_logger),        intent(inout) :: self           !< Logger
    integer(I1P),    allocatable, intent(in)    :: check(:,:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name     !< Name of the array
    character(len = *), optional, intent(in)    :: routine        !< Tracing routine
    integer(I4P) :: err, pos

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank4_I1P

  subroutine error(self, message, routine, err, rank, is_fatal)
    class(fortran_logger),        intent(inout) :: self
    character(len = *),           intent(in)    :: message
    character(len = *), optional, intent(in)    :: routine
    integer(I4P),       optional, intent(in)    :: err
    integer(I4P),       optional, intent(in)    :: rank
    logical,            optional, intent(in)    :: is_fatal !< Fatal error. Default is .false.
    logical :: fatal_error

    fatal_error = .false.
    if(present(is_fatal)) fatal_error = is_fatal

    if(self%log_level >= 1) then
      call self%write_message(self%error_unit, prefix = self%c_error_msg, message = message, routine = routine, &
      err = err, rank = rank)
      if(fatal_error) then
        call self%write_message(self%error_unit, prefix = self%c_error_msg, routine = routine,      &
                                message = 'This error is fatal. Program will stop executing now...' )
#ifdef _MPI
        call MPI_Finalize(self%ierror)
#endif
        stop
      endif
    endif

  end subroutine error

  subroutine write_message(self, unit, prefix, message, routine, err, rank)
    class(fortran_logger),        intent(inout) :: self
    integer(I4P),                 intent(in)    :: unit
    character(len = *),           intent(in)    :: prefix
    character(len = *),           intent(in)    :: message
    character(len = *), optional, intent(in)    :: routine
    integer(I4P),       optional, intent(in)    :: err
    integer(I4P),       optional, intent(in)    :: rank
    character(len = :), allocatable :: timestamp, subrout, msg

    if(self%print_timestamp) then
      timestamp = current_timestamp()
    else
      timestamp = ''
    endif

    if(present(routine)) then
      subrout = '['//routine//']'
    else
      subrout = ''
    endif

    msg = message
    if(present(err)) msg = msg//' '//trim(str(n = err))
    if(present(rank)) msg = msg//' on rank '//trim(str(n = rank, no_sign = .true.))

    write(unit, '(a)') timestamp//'  '//prefix//' -- '//subrout//' '//msg
    
  end subroutine write_message

  subroutine open_scratch_file(self)
    class(fortran_logger), intent(inout) :: self

    open( newunit = self%null_unit,   &
          action = 'write',           &
          status = 'scratch',         &
          iostat = self%ierror        )

  end subroutine open_scratch_file

  subroutine close_scratch_file(self)
    class(fortran_logger), intent(inout) :: self

    close(self%null_unit)

  end subroutine close_scratch_file
    
end module fortran_logger_m