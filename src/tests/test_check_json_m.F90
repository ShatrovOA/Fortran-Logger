module test_check_json_m
!< Module to test check_json_value, check_directory and check_file of Fortran Logger
use json_module, only: json_file, JCK => json_CK, json_string, json_integer, json_logical, json_real, json_array
use penf
use fortran_logger, only : fortran_logger_t
implicit none
private
public :: test_check_json

contains

  subroutine test_check_json(logger, rank)
  !< Tester subroutine
    class(fortran_logger_t),  intent(inout) :: logger             !< Fortran logger class
    integer(I4P),             intent(in)    :: rank               !< MPI processes number
    type(json_file)                         :: json               !< JSON file
    character(len=:),         allocatable   :: this               !< This subroutine name
    character(len=:),         allocatable   :: dirname            !< Name of directory with JSON file
    character(len=:),         allocatable   :: filename           !< Name of JSON file
    character(len=:),         allocatable   :: json_path          !< Path inside JSON
    integer(I4P)                            :: error_code         !< Error code
    integer(I4P)                            :: integer_value      !< Integer value taken from JSON
    real(R8P)                               :: float_value        !< Float value taken from JSON
    integer(I4P),             allocatable   :: integer_array(:)   !< Array of Integers taken from JSON
    logical                                 :: logical_value      !< Logical value taken from JSON
    character(kind=JCK,len=:),allocatable   :: string_value       !< String value taken from JSON
    real(R8P),                allocatable   :: float_array(:)     !< Array of Floats taken from JSON

    this = 'test_check_json'

    call logger%info('***************************************************************')
    call logger%info('               Testing system and JSON methods                 ')
    call logger%info('***************************************************************')

    dirname = 'files'
    call logger%check_directory(dirname, .false., routine = this, is_fatal = .true.)

    filename = dirname//'/test_file.json'
    call logger%check_file(filename, routine = this, is_fatal = .true.)
    call json%load_file(filename)

    json_path = 'Integer value'
    call logger%check_json_value(json, json_path, json_integer, routine = this, file=__FILE__, line=__LINE__, error = error_code)
    if(error_code == 0) call json%get(json_path, integer_value)

    json_path = 'Float value'
    call logger%check_json_value(json, json_path, json_real, routine = this, error = error_code)
    if(error_code == 0) call json%get(json_path, float_value)

    json_path = 'Array of integers'
    call logger%check_json_value(json, json_path, json_array, routine = this, error = error_code)
    if(error_code == 0) call json%get(json_path, integer_array)

    json_path = 'Logical value'
    call logger%check_json_value(json, json_path, json_logical, routine = this, file=__FILE__, error = error_code)
    if(error_code == 0) call json%get(json_path, logical_value)

    json_path = 'String value'
    call logger%check_json_value(json, json_path, json_string, routine = this, error = error_code)
    if(error_code == 0) call json%get(json_path, string_value)

    json_path = 'Array of floats'
    call logger%check_json_value(json, json_path, json_array, routine = this, line=__LINE__, error = error_code)
    if(error_code == 0) call json%get(json_path, float_array)
    call json%destroy()

    if(rank /= 0) dirname = 'file'
    call logger%check_directory(dirname, .true., routine = this, is_fatal = .false., file=__FILE__, line=__LINE__)
    if(rank /= 0) call execute_command_line('rm -rf '//dirname)
    if(rank /= 0) filename = 'files/file.json'
    call logger%check_file(filename, routine = this, is_fatal = .false.)

    call logger%info('***************************************************************')
    call logger%info('                  Tests passed...                              ')
    call logger%info('***************************************************************')
  end subroutine test_check_json
end module test_check_json_m