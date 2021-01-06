!< Fortran-Logger project, definition of [[element_object]] class that handles different levels outputs.

module logger_element_object
!< Fortran-Logger project, definition of [[element_object]] class that handles different levels outputs.
use penf, only: I4P
use face, only: colorize
use iso_fortran_env, only: output_unit
implicit none
private
public :: element_object

  type :: element_object
  !< Definition of Object Class
  private
    integer(I4P)                  :: out_unit !< Unit used to print output
    character(len=:), allocatable :: string   !< Element string
    character(len=:), allocatable :: color_fg !< Foreground color
    character(len=:), allocatable :: color_bg !< Background color
    character(len=:), allocatable :: style    !< Style
    character(len=:), allocatable :: prefix   !< Prefix string
    character(len=:), allocatable :: suffix   !< Suffix string
  contains
  private
  ! public methods
    procedure, pass(self), public :: finalize     !< Finalize element
    procedure, pass(self), public :: initialize   !< Initialize element
    procedure, pass(self), public :: output       !< Return formatted output of element
    procedure, pass(self), public :: update_unit  !< Replace current [[out_unit]] with new one
    procedure, pass(self), public :: get_unit     !< Return current [[out_unit]]
  endtype element_object

contains

!-------------------------------------------------------------------------------------
  pure subroutine finalize(self)
!-------------------------------------------------------------------------------------
!< Finalization class subroutine  
!-------------------------------------------------------------------------------------
    class(element_object), intent(inout) :: self  !< Element

    self%out_unit = output_unit
    if (allocated(self%string))   deallocate(self%string)
    if (allocated(self%color_fg)) deallocate(self%color_fg)
    if (allocated(self%color_bg)) deallocate(self%color_bg)
    if (allocated(self%style))    deallocate(self%style)
    if (allocated(self%prefix))   deallocate(self%prefix)
    if (allocated(self%suffix))   deallocate(self%suffix)
  end subroutine finalize

!-------------------------------------------------------------------------------------
  pure subroutine initialize(self, out_unit, string, color_fg, color_bg, style, prefix, suffix)
!-------------------------------------------------------------------------------------
!< Initialization class subroutine  
!-------------------------------------------------------------------------------------
    class(element_object),  intent(inout)        :: self      !< Element
    integer(I4P),           intent(in), optional :: out_unit  !< Unit used to print output
    character(len=*),       intent(in), optional :: string    !< Element string
    character(len=*),       intent(in), optional :: color_fg  !< Foreground color
    character(len=*),       intent(in), optional :: color_bg  !< Background color
    character(len=*),       intent(in), optional :: style     !< Style
    character(len=*),       intent(in), optional :: prefix    !< Prefix string
    character(len=*),       intent(in), optional :: suffix    !< Suffix string

    call self%finalize()
    if(present(out_unit)) self%out_unit = out_unit
    self%string = '';   if(present(string)) self%string = string
    self%color_fg = ''; if(present(color_fg)) self%color_fg = color_fg
    self%color_bg = ''; if(present(color_bg)) self%color_bg = color_bg
    self%style = '';    if(present(style)) self%style = style
    self%prefix = '';   if(present(prefix)) self%prefix = prefix
    self%suffix = ' ';   if(present(suffix)) self%suffix = suffix
  end subroutine initialize

!-------------------------------------------------------------------------------------
  function output(self, update_string) 
!-------------------------------------------------------------------------------------
!< Generate output message
!-------------------------------------------------------------------------------------
    class(element_object), intent(inout)        :: self           !< Element
    character(len=*),      intent(in), optional :: update_string  !< Element string
    character(len=:),      allocatable          :: output         !< Formatted output.

    if(present(update_string)) self%string = update_string
    output = self%prefix//colorize(self%string, color_fg=self%color_fg, color_bg=self%color_bg, style=self%style)//self%suffix
  end function output

!-------------------------------------------------------------------------------------
  pure function get_unit(self) result(unit)
!-------------------------------------------------------------------------------------
!< Returns current output unit
!-------------------------------------------------------------------------------------
    class(element_object),  intent(in) :: self !< Element
    integer(I4P)                       :: unit !< Output unit

    unit = self%out_unit
  end function get_unit

!-------------------------------------------------------------------------------------
  pure subroutine update_unit(self, new_unit)
!-------------------------------------------------------------------------------------
!< Changes current output unit
!-------------------------------------------------------------------------------------
    class(element_object),  intent(inout) :: self     !< Element
    integer(I4P),           intent(in)    :: new_unit !< New output unit number

    self%out_unit = new_unit
  end subroutine update_unit

end module logger_element_object