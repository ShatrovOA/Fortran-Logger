# Fortran Logger
 

[![Build Status](https://travis-ci.com/ShatrovOA/Fortran-Logger.svg?branch=master)](https://travis-ci.com/ShatrovOA/Fortran-Logger)

## Example of usage

```fortran
use fortran_logger_m, only : fortran_logger
implicit none
  type(fortran_logger) :: logger

  call logger%initialize(log_level = 5, print_timestamp = .true.)

  call logger%debug(routine = 'test_routine 1', message = 'enter')

  call logger%info(routine = 'test_routine 1', message = 'doing some stuff')

  call logger%warn(routine = 'test_routine 1', message = "you can't do that")

  ierror = -1
  call logger%check_error(check_routine = 'external subroutine', err = ierror, is_fatal = .true.)

  call logger%finalize()
```


```shell
Sat Feb 22 22:29:41 2020  INFO  -- [test_routine 1] doing some stuff
Sat Feb 22 22:29:41 2020  WARN  -- [test_routine 1] you cant do that
Sat Feb 22 22:29:41 2020  ERROR --  Subroutine external subroutine returned error code: -1 on rank 0
Sat Feb 22 22:29:41 2020  ERROR --  This error is fatal. Program will stop executing now...
```
