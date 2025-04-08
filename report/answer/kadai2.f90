!
! Note that this program assumes automatic memory deallocation capability, which
! is guaranteed in the Fortran 2003 standard
!
module mod_multiprec
  implicit none
  private

  integer, parameter :: radix     = 10
  integer, parameter :: pkind     = 1

  ! number of digit (can be changed by calling set_max_digit)
  integer :: max_digit = 1000

  ! multiple-precision type
  type :: multiprec
     integer(kind=pkind), allocatable :: digit(:) ! 0-9 for each digit
  end type multiprec

  interface operator(+)
     module procedure add
  end interface operator(+)

  interface operator(-)
     module procedure sub
  end interface operator(-)

  interface operator(*)
     module procedure mul
  end interface operator(*)

  interface operator(/)
     module procedure div
  end interface operator(/)

  public :: multiprec
  public :: set_max_digit, get_max_digit, alloc_multiprec
  public :: operator(+), operator(-), operator(*), operator(/)

contains

  ! return result of addition of two multiprec variables (a + b)
  function add(a, b) result(ret)
    implicit none
    type(multiprec), intent(in) :: a, b
    type(multiprec)             :: ret

    integer(kind=pkind) :: x(max_digit), y(max_digit)
    integer             :: i, r, z

    call alloc_multiprec(ret)

    x = a%digit
    y = b%digit
    r = 0
    do i = max_digit, 1, -1
       z = int(x(i), kind=4) + int(y(i), kind=4) + r
       r = z / radix
       z = z - radix * r
       ret%digit(i) = z
    end do

  end function add

  ! return result of subtraction between two multiprec variables (a - b)
  function sub(a, b) result(ret)
    implicit none
    type(multiprec), intent(in) :: a, b
    type(multiprec)             :: ret

    integer(kind=pkind) :: x(max_digit), y(max_digit)
    integer             :: i, r, z

    call alloc_multiprec(ret)

    x = a%digit
    y = b%digit
    r = 1
    do i = max_digit, 1, -1
       z = int(x(i), kind=4) - int(y(i), kind=4) + radix + r - 1
       r = z / radix
       z = z - radix * r
       ret%digit(i) = z
    end do

  end function sub

  ! return result of multiplication of a multiprec by a scalar integer (a * b)
  function mul(a, b) result(ret)
    implicit none
    type(multiprec), intent(in) :: a
    integer, intent(in)         :: b
    type(multiprec)             :: ret

    integer(kind=pkind) :: x(max_digit)
    integer             :: i, r, y, z

    call alloc_multiprec(ret)

    x = a%digit
    y = b
    r = 0
    do i = max_digit, 1, -1
       z = int(x(i), kind=4) * y + r
       r = z / radix
       z = z - r * radix
       ret%digit(i) = z
    end do

  end function mul

  ! return result of division of a multiplrec by a scalar integer (a / b)
  function div(a, b) result(ret)
    implicit none
    type(multiprec), intent(in) :: a
    integer, intent(in)         :: b
    type(multiprec)             :: ret

    integer(kind=pkind) :: x(max_digit)
    integer             :: i, r, y, z

    call alloc_multiprec(ret)

    x = a%digit
    y = b
    r = 0
    do i = 1, max_digit, 1
       z = x(i) + r * radix
       r = mod(z, b)
       ret%digit(i) = z / b
    end do

  end function div

  ! set maximum digit
  subroutine set_max_digit(n)
    implicit none
    integer, intent(in) :: n

    max_digit = n

  end subroutine set_max_digit

  ! get maximum digit
  function get_max_digit() result(n)
    implicit none
    integer :: n

    n = max_digit

  end function get_max_digit

  ! allocate memory for multiprec
  subroutine alloc_multiprec(x)
    type(multiprec), intent(inout) :: x

    if( allocated(x%digit) ) then
       deallocate(x%digit)
    end if

    allocate(x%digit(max_digit))

  end subroutine alloc_multiprec

end module mod_multiprec

!
! main program for multiple-precision calculation of circular ratio PI
!
program kadai2
  use mod_multiprec
  implicit none

  character(len=128) :: arg
  integer :: i, ndigit, niteration
  type(multiprec) :: a, b, c, pi

  ! get ndigit from command line
  if( command_argument_count() >= 1 ) then
     call get_command_argument(1, arg)
     read(arg, *) ndigit
  else
     write(*,*) 'Error: please specify the number of digits for computing PI.'
     stop
  end if

  ! number of iteration (+10 for small ndigit)
  niteration = ceiling(ndigit / (2*log10(5.0_8))) + 10

  ! memory allocation (+10 for safety)
  call set_max_digit(ndigit+10)
  call alloc_multiprec(a)
  call alloc_multiprec(b)
  call alloc_multiprec(c)
  call alloc_multiprec(pi)

  ! a == 16/5 = 3.2
  a%digit    = 0
  a%digit(1) = 3
  a%digit(2) = 2

  ! b == 4/239 = 0.0167....
  b%digit    = 0
  b%digit(1) = 4
  b = b / 239

  ! c == a - b
  c = a - b

  ! calculate pi using Machin's formula
  pi%digit = 0
  do i = 1, niteration
     if( mod(i,2) == 0 ) then
        pi = pi - c
     else
        pi = pi + c
     end if

     a = a / (5*5)
     b = b / (239*239)
     c = (a - b) / (2*i+1)
  end do

  ! output
  call print_pi(pi, ndigit)

  stop
contains
  ! print out pi to the terminal
  subroutine print_pi(pi, ndigit)
    implicit none
    type(multiprec), intent(in) :: pi
    integer, intent(in) :: ndigit

    integer :: i, j, k

    ! first line
    i = 0
    write(*, fmt='(a)', advance='no') 'PI = 3.'
    do j = 0, 4
       do k = 1, 10
          write(*, fmt='(i1.1)', advance='no') pi%digit(50*i+10*j+k+1)
       end do
       write(*, fmt='(x)', advance='no')
    end do
    write(*,*)

    ! next
    do i = 1, ndigit/50-1
       write(*, fmt='(7x)', advance='no')
       do j = 0, 4
          do k = 1, 10
             write(*, fmt='(i1.1)', advance='no') pi%digit(50*i+10*j+k+1)
          end do
          write(*, fmt='(x)', advance='no')
       end do
       write(*,*)
    end do

  end subroutine print_pi
end program kadai2
