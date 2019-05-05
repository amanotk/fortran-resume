program kadai5
  implicit none
  character(len=*), parameter :: fmt = '(f12.5, f12.5)'

  integer :: n, t
  real(8) :: x, y, d

  integer :: argc, length, status
  character(len=256) :: argv

  !
  ! process command line argument
  !
  n    = 0
  t    = 0
  argc = command_argument_count()
  if( argc == 1 ) then
     ! read level
     call get_command_argument(1, argv, length, status)
     if( status == 0 ) then
        read(argv, *) n
     end if
     t = 0
  else if( argc >= 2 ) then
     ! read level
     call get_command_argument(1, argv, length, status)
     if( status == 0 ) then
        read(argv, *) n
     end if
     ! read type
     call get_command_argument(2, argv, length, status)
     if( status  == 0 ) then
        read(argv, *) t
     end if
  else
     write(0,*) 'Error: specify level and type of Hilbert curve with command line arguments'
     stop
  end if

  ! step size
  d = 0.5_8 / 2**n

  select case(t)
  case(0)
     ! LDR
     x = 1 - d/2
     y = 1 - d/2
     write(*, fmt) x, y
     call ldr(n, x, y, d)
  case(1)
     ! URD
     x = 0 + d/2
     y = 0 + d/2
     write(*, fmt) x, y
     call urd(n, x, y, d)
  case(2)
     ! RUL
     x = 0 + d/2
     y = 0 + d/2
     write(*, fmt) x, y
     call rul(n, x, y, d)
  case(3)
     ! DLU
     x = 1 - d/2
     y = 1 - d/2
     write(*, fmt) x, y
     call dlu(n, x, y, d)
  case default
     write(*,*) 'Error'
  end select

  stop
contains

  subroutine mvl(x, y, d)
    implicit none
    real(8), intent(inout) :: x, y
    real(8), intent(in)    :: d

    write(*, fmt) x-d, y

    x = x - d
    y = y

  end subroutine mvl

  subroutine mvr(x, y, d)
    implicit none
    real(8), intent(inout) :: x, y
    real(8), intent(in)    :: d

    write(*, fmt) x+d, y

    x = x + d
    y = y

  end subroutine mvr

  subroutine mvu(x, y, d)
    implicit none
    real(8), intent(inout) :: x, y
    real(8), intent(in)    :: d

    write(*, fmt) x, y+d

    x = x
    y = y + d

  end subroutine mvu

  subroutine mvd(x, y, d)
    implicit none
    real(8), intent(inout) :: x, y
    real(8), intent(in)    :: d

    write(*, fmt) x, y-d

    x = x
    y = y - d

  end subroutine mvd

  recursive subroutine ldr(n, x, y, d)
    implicit none
    integer, intent(in) :: n
    real(8), intent(inout) :: x, y
    real(8), intent(in)    :: d

    if( n >= 1 ) then
       call dlu(n-1, x, y, d)
       call mvl(x, y, d)
       call ldr(n-1, x, y, d)
       call mvd(x, y, d)
       call ldr(n-1, x, y, d)
       call mvr(x, y, d)
       call urd(n-1, x, y, d)
    else
       write(*, fmt) x-d, y
       write(*, fmt) x-d, y-d
       write(*, fmt) x  , y-d
       x = x
       y = y - d
    end if

  end subroutine ldr

  recursive subroutine urd(n, x, y, d)
    implicit none
    integer, intent(in) :: n
    real(8), intent(inout) :: x, y
    real(8), intent(in)    :: d

    if( n >= 1 ) then
       call rul(n-1, x, y, d)
       call mvu(x, y, d)
       call urd(n-1, x, y, d)
       call mvr(x, y, d)
       call urd(n-1, x, y, d)
       call mvd(x, y, d)
       call ldr(n-1, x, y, d)
    else
       write(*, fmt) x  , y+d
       write(*, fmt) x+d, y+d
       write(*, fmt) x+d, y
       x = x + d
       y = y
    end if

  end subroutine urd

  recursive subroutine rul(n, x, y, d)
    implicit none
    integer, intent(in) :: n
    real(8), intent(inout) :: x, y
    real(8), intent(in)    :: d

    if( n >= 1 ) then
       call urd(n-1, x, y, d)
       call mvr(x, y, d)
       call rul(n-1, x, y, d)
       call mvu(x, y, d)
       call rul(n-1, x, y, d)
       call mvl(x, y, d)
       call dlu(n-1, x, y, d)
    else
       write(*, fmt) x+d, y
       write(*, fmt) x+d, y+d
       write(*, fmt) x  , y+d
       x = x
       y = y + d
    end if

  end subroutine rul

  recursive subroutine dlu(n, x, y, d)
    implicit none
    integer, intent(in) :: n
    real(8), intent(inout) :: x, y
    real(8), intent(in)    :: d

    if( n >= 1 ) then
       call ldr(n-1, x, y, d)
       call mvd(x, y, d)
       call dlu(n-1, x, y, d)
       call mvl(x, y, d)
       call dlu(n-1, x, y, d)
       call mvu(x, y, d)
       call rul(n-1, x, y, d)
    else
       write(*, fmt) x  , y-d
       write(*, fmt) x-d, y-d
       write(*, fmt) x-d, y
       x = x - d
       y = y
    end if

  end subroutine dlu

end program kadai5
