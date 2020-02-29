! gfortran  -o test  src/lib/logger.F90 src/tests/testing_subroutines.f90 src/tests/test1.F90 -I /Users/os250016/MyProjects/FOSS/FOSS/include -L /Users/os250016/MyProjects/FOSS/FOSS/lib -lpenf -lface -lflap -ldatetime -J .
! mpifort -D_MPI -o test  src/lib/logger.F90 src/tests/testing_subroutines.f90 src/tests/test1_mpi.F90 -I /Users/os250016/MyProjects/FOSS/FOSS/include -L /Users/os250016/MyProjects/FOSS/FOSS/lib -lpenf -lface -lflap -ldatetime -J .

module fortran_logger
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

  integer(I4P), parameter :: error_level  = 1
  integer(I4P), parameter :: warn_level   = 2
  integer(I4P), parameter :: info_level   = 3
  integer(I4P), parameter :: debug_level  = 4

  type :: str_handle
    character(len = :), allocatable :: msg
  end type str_handle

  type, public :: fortran_logger_t
    integer(I4P) :: log_level = 0
    logical :: print_timestamp = .false.
    logical :: use_log_file = .false.
    logical :: is_mpi_init = .false.
    logical :: is_setup = .false.

    type(str_handle) :: pref(4)
    type(str_handle) :: c_pref(4)
    type(str_handle) :: colors(4)
    character(len = :), allocatable :: log_file
    character(len = :), allocatable :: timestamp_format

    integer(I4P) :: out_units(4) = [stderr, stderr, stdout, stdout]
    integer(I4P) :: n_sim_units = 1
    integer(I4P) :: sim_units(2)
    integer(I4P) :: null_unit
    integer(I4P) :: logger_unit
    integer(I4P) :: rank = 0
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
    ! procedure, pass(self),  public :: mpi_init
    procedure, pass(self) :: gather
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
    procedure, pass(self) :: setup
    procedure, pass(self) :: checkerr
    procedure, pass(self) :: current_timestamp
  end type fortran_logger_t

  interface get_passed_value
    module procedure :: get_integer
    module procedure :: get_string
  end interface get_passed_value
    
contains

!-------------------------------------------------------------------------------------
  subroutine initialize(self, log_level,                                             &
                        error_color, warn_color, info_color, debug_color,            &
                        error_unit, warn_unit, info_unit, debug_unit,                &
                        print_timestamp, timestamp_format, log_file                  )
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self
    integer(I4P),       optional, intent(in)    :: log_level
    character(len = *), optional, intent(in)    :: error_color
    character(len = *), optional, intent(in)    :: warn_color
    character(len = *), optional, intent(in)    :: info_color
    character(len = *), optional, intent(in)    :: debug_color
    integer(I4P),       optional, intent(in)    :: error_unit
    integer(I4P),       optional, intent(in)    :: warn_unit
    integer(I4P),       optional, intent(in)    :: info_unit
    integer(I4P),       optional, intent(in)    :: debug_unit
    logical,            optional, intent(in)    :: print_timestamp
    character(len = *), optional, intent(in)    :: timestamp_format
    character(len = *), optional, intent(in)    :: log_file
    type(command_line_interface)    :: cli !< CLI
    character(len = :), allocatable :: logger_file
    integer(I4P) :: i

    call self%set_default_values()

    self%pref(error_level)%msg  = 'ERROR'
    self%pref(warn_level)%msg   = 'WARN '
    self%pref(info_level)%msg   = 'INFO '
    self%pref(debug_level)%msg  = 'DEBUG'

    if(present(log_level))        self%log_level        = log_level
    if(present(error_color))      self%colors(1)%msg    = error_color
    if(present(warn_color))       self%colors(2)%msg    = warn_color
    if(present(info_color))       self%colors(3)%msg    = info_color
    if(present(debug_color))      self%colors(4)%msg    = debug_color
    if(present(error_unit))       self%out_units(1)     = error_unit
    if(present(warn_unit))        self%out_units(2)     = warn_unit
    if(present(info_unit))        self%out_units(3)     = info_unit
    if(present(debug_unit))       self%out_units(4)     = debug_unit
    if(present(print_timestamp))  self%print_timestamp  = print_timestamp
    if(present(timestamp_format)) self%timestamp_format = timestamp_format
    if(present(log_file))         self%log_file         = log_file

    do concurrent(i = 1:4)
      self%c_pref(i)%msg  = colorize(string = self%pref(i)%msg , color_fg = self%colors(i)%msg )
    end do

#ifdef _MPI
    self%comm = MPI_COMM_WORLD

    call MPI_Comm_size(self%comm, self%np, self%ierror)

    call MPI_Comm_rank(self%comm, self%rank, self%ierror)

    allocate(self%tmp(0:self%np - 1))
#endif


    call self%open_scratch_file()

    call cli%init(ignore_unknown_clas = .true.,   &
                  usage_lun = self%null_unit,     &
                  error_lun  = self%null_unit,    &
                  version_lun = self%null_unit,   &
                  disable_hv = .true.             )
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

    call get_passed_value(cli = cli, switch = '-log_file', val = self%log_file, ierror = self%ierror)
    if(allocated(self%log_file)) then
      self%use_log_file = .true.
      self%n_sim_units = 2
    endif
    call self%check_error(check_routine = 'get_passed_value', err = self%ierror, routine = 'init', is_fatal = .false.)

    call get_passed_value(cli = cli, switch = '-log',   val = self%log_level, ierror = self%ierror)
    call self%check_error(check_routine = 'get_passed_value', err = self%ierror, routine = 'init', is_fatal = .false.)
    if(self%log_level > 4) then
      call self%warn(message = 'Value of log_level '//trim(str(n = self%log_level,no_sign = .true.))//' > 4 = debug')
      call self%warn(message = 'Assuming log_level = 4')
      self%log_level = 4
    endif
    if(self%log_level < 0) then
      i = self%log_level
      self%log_level = warn_level
      call self%warn(message = 'Value of log_level '//trim(str(n = i))//' < 0')
      call self%warn(message = 'Assuming log_level = 0')
      self%log_level = 0
    endif

    call cli%free()

  end subroutine initialize

#ifdef _MPI
! !-------------------------------------------------------------------------------------
!   subroutine mpi_init(self, comm)
! !-------------------------------------------------------------------------------------
! !< Initialize MPI structure
! !-------------------------------------------------------------------------------------
!     class(fortran_logger_t),    intent(inout) :: self
!     type(MPI_Comm), optional, intent(in)    :: comm

!     if(present(comm)) then
!       self%comm = comm
!     else
!       self%comm = MPI_COMM_WORLD
!     endif

!     call MPI_Comm_size(self%comm, self%np, self%ierror)

!     call MPI_Comm_rank(self%comm, self%rank, self%ierror)

!     allocate(self%tmp(0:self%np - 1))

!   end subroutine mpi_init

!-------------------------------------------------------------------------------------
  subroutine gather(self, err, pos)
!-------------------------------------------------------------------------------------
!< Gather all error codes, find nonzero error code and rank
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),  intent(inout) :: self
    integer(I4P),           intent(inout) :: err
    integer(I4P),           intent(out)   :: pos

    call MPI_Allgather(abs(err), 1, MPI_INTEGER, self%tmp, 1, MPI_INTEGER, self%comm, self%ierror)
    err = maxval(self%tmp, dim = 1)
    pos = maxloc(self%tmp, dim = 1) - 1
    
  end subroutine gather
#endif

  subroutine setup(self)
    class(fortran_logger_t),  intent(inout) :: self

    if(self%use_log_file) then
      if(self%rank == 0) then
        open( newunit = self%logger_unit,           &
              file = self%log_file,                 &
              action = 'write',                     &
              status='replace',                     &
              iostat = self%ierror                  )
      endif
      self%sim_units(2) = self%logger_unit
    endif

    if(self%rank /= 0) then
      self%out_units(:) = self%null_unit
      self%logger_unit = self%null_unit
    endif

    self%is_setup = .true.

  end subroutine setup

!-------------------------------------------------------------------------------------
  subroutine get_integer(cli, switch, val, ierror)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(command_line_interface),  intent(inout) :: cli
    character(len = *),             intent(in)    :: switch
    integer(I4P),                   intent(out)   :: val
    integer(I4P),                   intent(out)   :: ierror

    if(cli%is_passed(switch = switch)) then
      call cli%get(switch = switch,   val = val,  error = ierror)
    else
      ierror = 0
    endif

  end subroutine get_integer

!-------------------------------------------------------------------------------------
  subroutine get_string(cli, switch, val, ierror)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(command_line_interface),    intent(inout) :: cli
    character(len = *),               intent(in)    :: switch
    character(len = :), allocatable,  intent(out)   :: val
    integer(I4P),                     intent(out)   :: ierror
    character(len = 100) :: string

    if(cli%is_passed(switch = switch)) then
      call cli%get(switch = switch,   val = string,  error = ierror)
      val = trim(string)
    else
      ierror = 0
    endif

  end subroutine get_string

!-------------------------------------------------------------------------------------
  subroutine finalize(self)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),  intent(inout) :: self

    ! call self%close_scratch_file()
    if(self%is_setup) then
      if(self%use_log_file .and. self%rank == 0) close(self%logger_unit)
      call self%close_scratch_file()
    endif
    call self%set_default_values()

    self%is_setup = .false.

  end subroutine finalize

!-------------------------------------------------------------------------------------
  function current_timestamp(self) result(timestamp)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t), intent(inout) :: self
    character(len = :), allocatable :: timestamp
    type(datetime) :: dtime

    if(self%print_timestamp) then
      dtime = dtime%now()
      timestamp = dtime%strftime(self%timestamp_format)//'  '
    else
      timestamp = ''
    endif

  end function current_timestamp

!-------------------------------------------------------------------------------------
  subroutine set_default_values(self)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t), intent(inout) :: self

    self%colors(1)%msg  = 'RED_INTENSE'
    self%colors(2)%msg  = 'YELLOW'
    self%colors(3)%msg  = 'GREEN'
    self%colors(4)%msg  = 'CYAN'

    self%timestamp_format = '%c'

  end subroutine set_default_values

!-------------------------------------------------------------------------------------
  subroutine debug(self, message, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self
    character(len = *),           intent(in)    :: message
    character(len = *), optional, intent(in)    :: routine

    if(self%log_level == debug_level) then
      if(.not. self%is_setup) call self%setup()
      call self%write_message(level = debug_level, message = message, routine = routine)
    endif
  
  end subroutine debug

!-------------------------------------------------------------------------------------
  subroutine info(self, message, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self
    character(len = *),           intent(in)    :: message
    character(len = *), optional, intent(in)    :: routine

    if(self%log_level >= info_level) then
      if(.not. self%is_setup) call self%setup()
      call self%write_message(level = info_level, message = message, routine = routine)
    endif
  
  end subroutine info

!-------------------------------------------------------------------------------------
  subroutine warn(self, message, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self
    character(len = *),           intent(in)    :: message
    character(len = *), optional, intent(in)    :: routine

    if(self%log_level >= warn_level) then
      if(.not. self%is_setup) call self%setup()
      call self%write_message(level = warn_level, message = message, routine = routine)
    endif
  
  end subroutine warn

!-------------------------------------------------------------------------------------
  subroutine check_error(self, check_routine, err, routine, is_fatal)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self
    character(len = *),           intent(in)    :: check_routine
    integer(I4P),                 intent(in)    :: err
    character(len = *), optional, intent(in)    :: routine        !< Tracing routine
    logical,            optional, intent(in)    :: is_fatal       !< Fatal error. Default is .false.


    call self%checkerr(message = 'Subroutine '//check_routine//' returned error code: '//trim(str(n=err)),  &
                       err = err, routine = routine, is_fatal = is_fatal                                    )

  end subroutine check_error

!-------------------------------------------------------------------------------------
  subroutine checkerr(self, message, err, routine, is_fatal)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self
    character(len = *),           intent(in)    :: message
    integer(I4P),                 intent(in)    :: err
    character(len = *), optional, intent(in)    :: routine  !< Tracing routine
    logical,            optional, intent(in)    :: is_fatal !< Fatal error. Default is .false.
    integer(I4P) :: ierr, pos

    ierr = err

#ifdef _MPI
    call self%gather(ierr, pos)
#endif

    if(.not. self%is_setup) call self%setup()

    if(ierr /= 0) then
      call self%error(message = message, routine = routine, err = ierr, rank = pos, is_fatal = is_fatal)
    endif

  end subroutine checkerr

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank1_cR8P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self         !< Logger
    complex(R8P),    allocatable, intent(in)    :: check(:)     !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank1_cR8P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank1_cR4P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self         !< Logger
    complex(R4P),    allocatable, intent(in)    :: check(:)     !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank1_cR4P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank1_R8P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self         !< Logger
    real(R8P),       allocatable, intent(in)    :: check(:)     !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank1_R8P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank1_R4P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self         !< Logger
    real(R4P),       allocatable, intent(in)    :: check(:)     !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank1_R4P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank1_I8P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self         !< Logger
    integer(I8P),    allocatable, intent(in)    :: check(:)     !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank1_I8P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank1_I4P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self         !< Logger
    integer(I4P),    allocatable, intent(in)    :: check(:)     !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank1_I4P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank1_I2P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self         !< Logger
    integer(I2P),    allocatable, intent(in)    :: check(:)     !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank1_I2P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank1_I1P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self         !< Logger
    integer(I1P),    allocatable, intent(in)    :: check(:)     !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank1_I1P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank2_R8P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self         !< Logger
    real(R8P),       allocatable, intent(in)    :: check(:,:)   !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank2_R8P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank2_R4P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self         !< Logger
    real(R4P),       allocatable, intent(in)    :: check(:,:)   !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank2_R4P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank2_cR8P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self         !< Logger
    complex(R8P),    allocatable, intent(in)    :: check(:,:)   !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank2_cR8P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank2_cR4P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self         !< Logger
    complex(R4P),    allocatable, intent(in)    :: check(:,:)   !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank2_cR4P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank2_I8P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self         !< Logger
    integer(I8P),    allocatable, intent(in)    :: check(:,:)   !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank2_I8P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank2_I4P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self         !< Logger
    integer(I4P),    allocatable, intent(in)    :: check(:,:)   !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank2_I4P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank2_I2P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self         !< Logger
    integer(I2P),    allocatable, intent(in)    :: check(:,:)   !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank2_I2P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank2_I1P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self         !< Logger
    integer(I1P),    allocatable, intent(in)    :: check(:,:)   !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank2_I1P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank3_R8P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self         !< Logger
    real(R8P),       allocatable, intent(in)    :: check(:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank3_R8P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank3_R4P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self         !< Logger
    real(R4P),       allocatable, intent(in)    :: check(:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank3_R4P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank3_cR8P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self         !< Logger
    complex(R8P),    allocatable, intent(in)    :: check(:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank3_cR8P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank3_cR4P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self         !< Logger
    complex(R4P),    allocatable, intent(in)    :: check(:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank3_cR4P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank3_I8P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self         !< Logger
    integer(I8P),    allocatable, intent(in)    :: check(:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank3_I8P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank3_I4P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self         !< Logger
    integer(I4P),    allocatable, intent(in)    :: check(:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank3_I4P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank3_I2P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self         !< Logger
    integer(I2P),    allocatable, intent(in)    :: check(:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank3_I2P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank3_I1P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self         !< Logger
    integer(I1P),    allocatable, intent(in)    :: check(:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name   !< Name of the array
    character(len = *), optional, intent(in)    :: routine      !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank3_I1P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank4_R8P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self           !< Logger
    real(R8P),       allocatable, intent(in)    :: check(:,:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name     !< Name of the array
    character(len = *), optional, intent(in)    :: routine        !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank4_R8P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank4_R4P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self           !< Logger
    real(R4P),       allocatable, intent(in)    :: check(:,:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name     !< Name of the array
    character(len = *), optional, intent(in)    :: routine        !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank4_R4P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank4_cR8P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self           !< Logger
    complex(R8P),    allocatable, intent(in)    :: check(:,:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name     !< Name of the array
    character(len = *), optional, intent(in)    :: routine        !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank4_cR8P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank4_cR4P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self           !< Logger
    complex(R4P),    allocatable, intent(in)    :: check(:,:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name     !< Name of the array
    character(len = *), optional, intent(in)    :: routine        !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank4_cR4P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank4_I8P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self           !< Logger
    integer(I8P),    allocatable, intent(in)    :: check(:,:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name     !< Name of the array
    character(len = *), optional, intent(in)    :: routine        !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank4_I8P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank4_I4P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self           !< Logger
    integer(I4P),    allocatable, intent(in)    :: check(:,:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name     !< Name of the array
    character(len = *), optional, intent(in)    :: routine        !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank4_I4P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank4_I2P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self           !< Logger
    integer(I2P),    allocatable, intent(in)    :: check(:,:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name     !< Name of the array
    character(len = *), optional, intent(in)    :: routine        !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank4_I2P

!-------------------------------------------------------------------------------------
  subroutine check_alloc_rank4_I1P(self, check, check_name, routine)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self           !< Logger
    integer(I1P),    allocatable, intent(in)    :: check(:,:,:,:) !< Array to be checked
    character(len = *),           intent(in)    :: check_name     !< Name of the array
    character(len = *), optional, intent(in)    :: routine        !< Tracing routine
    integer(I4P) :: err

    err = 0
    if(.not. allocated(check)) err = 1

    call self%checkerr(message = 'Allocation of '//check_name//' failed', err = err, routine = routine, is_fatal = .true.)

  end subroutine check_alloc_rank4_I1P

!-------------------------------------------------------------------------------------
  subroutine error(self, message, routine, err, rank, is_fatal)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self
    character(len = *),           intent(in)    :: message
    character(len = *), optional, intent(in)    :: routine
    integer(I4P),       optional, intent(in)    :: err
    integer(I4P),       optional, intent(in)    :: rank
    logical,            optional, intent(in)    :: is_fatal !< Fatal error. Default is .false.
    logical :: fatal_error

    if(self%log_level >= error_level) then
      if(.not. self%is_setup) call self%setup()
      fatal_error = .false.
      if(present(is_fatal)) fatal_error = is_fatal
      call self%write_message(level = error_level, message = message, routine = routine   &

#ifdef _MPI
                                                                        , rank = rank     &
#endif                                                                              
                                                                                          )
      if(fatal_error) then
        call self%write_message(level = error_level, routine = routine,                             &
                                message = 'This error is fatal. Program will stop executing now...' )
#ifdef _MPI
        call MPI_Finalize(self%ierror)
#endif
        stop
      endif
    endif

  end subroutine error

!-------------------------------------------------------------------------------------
  subroutine write_message(self, level, message, routine                      &

#ifdef _MPI
                                                                , rank               &
#endif
                                                                                     )
!-------------------------------------------------------------------------------------
!< Printer
!-------------------------------------------------------------------------------------
    class(fortran_logger_t),        intent(inout) :: self
    integer(I4P),                 intent(in)    :: level
    character(len = *),           intent(in)    :: message
    character(len = *), optional, intent(in)    :: routine
#ifdef _MPI
    integer(I4P),       optional, intent(in)    :: rank
#endif
    type(str_handle) :: msg(2)
    integer(I4P) :: i
    character(len = :), allocatable :: tmp

    self%sim_units(1) = self%out_units(level)

    tmp = ''

    if(present(routine)) then
      tmp = '['//routine//'] '
    endif

    tmp = tmp//message

#ifdef _MPI
    if(present(rank)) tmp = tmp//' on rank '//trim(str(n = rank, no_sign = .true.))
#endif

    msg(1)%msg = self%current_timestamp()//self%c_pref(level)%msg//' -- '//tmp
    msg(2)%msg = self%current_timestamp()//self%pref(level)%msg//' -- '//tmp

    do i = 1, self%n_sim_units
      write(self%sim_units(i), '(a)') msg(i)%msg
    end do
    
  end subroutine write_message

!-------------------------------------------------------------------------------------
  subroutine open_scratch_file(self)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t), intent(inout) :: self

    open( newunit = self%null_unit,   &
          action = 'write',           &
          status = 'scratch',         &
          iostat = self%ierror        )

  end subroutine open_scratch_file

!-------------------------------------------------------------------------------------
  subroutine close_scratch_file(self)
!-------------------------------------------------------------------------------------
!< 
!-------------------------------------------------------------------------------------
    class(fortran_logger_t), intent(inout) :: self

    close(self%null_unit)

  end subroutine close_scratch_file
    
end module fortran_logger