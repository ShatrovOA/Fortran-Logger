# Fortran Logger
 
[![Status](https://img.shields.io/badge/status-stable-brightgreen.svg)]()
[![Build Status](https://travis-ci.com/ShatrovOA/Fortran-Logger.svg?branch=master)](https://travis-ci.com/ShatrovOA/Fortran-Logger)
[![Coverage Status](https://codecov.io/gh/ShatrovOA/Fortran-Logger/branch/master/graph/badge.svg?token=QT9Y19KF8X)](https://codecov.io/gh/ShatrovOA/Fortran-Logger)
![GitHub](https://img.shields.io/github/license/ShatrovOA/Fortran-Logger?color=brightgreen&logo=License)
## Main features
- Free and Open Source Project
- MPI Support
- Fortran 2008+ standard compliant
- Highly customizable logger and error checker
- CLI Support for runtime change of logging level

## Installation
In order to build this library you will need to have modern fortran compiler installed. The only currently possible way to install library is with [FoBiS.py](https://github.com/szaghi/FoBiS). 

Next example will build static library with MPI support and optimize flags
```bash
git clone https://github.com/ShatrovOA/Fortran-Logger
cd Fortran-Logger
FoBiS.py build -mode static-gnu-mpi
```
List of available build modes can be obtained by running 
```bash
FoBiS.py build -lmodes
```

## Logging Format
Logger format is highly customizable, which means that user can define it's own unique way of logging. Initialization subroutine has the following interface.
```fortran
  subroutine initialize(self, log_level,                                                                        &
                        error_unit, error_color_fg, error_color_bg, error_style, error_prefix, error_suffix,    &
                        warn_unit, warn_color_fg, warn_color_bg, warn_style, warn_prefix, warn_suffix,          &
                        info_unit, info_color_fg, info_color_bg, info_style, info_prefix, info_suffix,          &
                        debug_unit, debug_color_fg, debug_color_bg, debug_style, debug_prefix, debug_suffix,    &
                        routine_prefix, routine_suffix, print_timestamp, timestamp_format, timestamp_color_fg,  &
                        timestamp_color_bg, timestamp_style, timestamp_prefix, timestamp_suffix)
```
All arguments are optional. The meaning of the arguments (except the obvious passed self) are:
```fortran
  integer(I4P),     intent(in), optional  :: log_level            !< Level of logging
  integer(I4P),     intent(in), optional  :: error_unit           !< Unit used to print ERROR messages
  character(len=*), intent(in), optional  :: error_color_fg       !< ERROR foreground color
  character(len=*), intent(in), optional  :: error_color_bg       !< ERROR background color
  character(len=*), intent(in), optional  :: error_style          !< ERROR style
  character(len=*), intent(in), optional  :: error_prefix         !< ERROR prefix
  character(len=*), intent(in), optional  :: error_suffix         !< ERROR suffix
  integer(I4P),     intent(in), optional  :: warn_unit            !< Unit used to print WARN messages
  character(len=*), intent(in), optional  :: warn_color_fg        !< WARN foreground color
  character(len=*), intent(in), optional  :: warn_color_bg        !< WARN background color
  character(len=*), intent(in), optional  :: warn_style           !< WARN style
  character(len=*), intent(in), optional  :: warn_prefix          !< WARN prefix
  character(len=*), intent(in), optional  :: warn_suffix          !< WARN suffix
  integer(I4P),     intent(in), optional  :: info_unit            !< Unit used to print INFO messages
  character(len=*), intent(in), optional  :: info_color_fg        !< INFO foreground color
  character(len=*), intent(in), optional  :: info_color_bg        !< INFO background color
  character(len=*), intent(in), optional  :: info_style           !< INFO style
  character(len=*), intent(in), optional  :: info_prefix          !< INFO prefix
  character(len=*), intent(in), optional  :: info_suffix          !< INFO suffix
  integer(I4P),     intent(in), optional  :: debug_unit           !< Unit used to print DEBUG messages
  character(len=*), intent(in), optional  :: debug_color_fg       !< DEBUG foreground color
  character(len=*), intent(in), optional  :: debug_color_bg       !< DEBUG background color
  character(len=*), intent(in), optional  :: debug_style          !< DEBUG style
  character(len=*), intent(in), optional  :: debug_prefix         !< DEBUG prefix
  character(len=*), intent(in), optional  :: debug_suffix         !< DEBUG suffix
  character(len=*), intent(in), optional  :: routine_prefix       !< Routine prefix. routine is optional agrument in most of logger methods
  character(len=*), intent(in), optional  :: routine_suffix       !< Routine suffix. routine is optional agrument in most of logger methods
  logical,          intent(in), optional  :: print_timestamp      !< Display current timestamp with all messages. Default is .true.
  character(len=*), intent(in), optional  :: timestamp_format     !< Timestamp format, C style
  character(len=*), intent(in), optional  :: timestamp_color_fg   !< Timestamp foreground color
  character(len=*), intent(in), optional  :: timestamp_color_bg   !< Timestamp background color
  character(len=*), intent(in), optional  :: timestamp_style      !< Timestamp style
  character(len=*), intent(in), optional  :: timestamp_prefix     !< Timestamp prefix
  character(len=*), intent(in), optional  :: timestamp_suffix     !< Timestamp suffix
```
If user builds library with mpi support (e.g. modes: static-gnu-mpi, shared-intel-mpi-debug) then one more argument appears, which is required:
```fortran
  type(MPI_Comm),   intent(in)            :: comm                 !< MPI Communicator
```
This library supports 5 levels of logging (log_level argument), defined in public parameters:

* LOGGER_NULL_LEVEL = 0
* LOGGER_ERROR_LEVEL = 1
* LOGGER_WARN_LEVEL = 2
* LOGGER_INFO_LEVEL = 3
* LOGGER_DEBUG_LEVEL = 4

Each level will produce more output then previous. LOGGER_NULL_LEVEL will produce no output even if fatal error occured, program will continue execution. If you don't pass anything then LOGGER_DEBUG_LEVEL will be used.

### CLI support
With the help of [FLAP](https://github.com/szaghi/FLAP) Fortran Logger supports change of log level at runtime. When you run your application use ```--logger_level``` or it's shorter version ```-log_lev``` switch and pass desired level of logging. Value passed through CLI switch will override one you passed within ```initialize``` subroutine.
