!
! module for root finding
!
module mod_root
  implicit none
  private

  integer, parameter :: default_maxit = 50
  real(8), parameter :: default_tol   = 1.0e-8_8
  real(8), parameter :: epsilon       = 1.0e-32_8

  public :: secant, newton

contains
  !
  ! newton method
  !
  subroutine newton(f, df, x, error, status, maxit, tol)
    implicit none
    real(8), intent(inout) :: x
    real(8), intent(out) :: error
    integer, intent(out) :: status
    integer, intent(in), optional :: maxit
    real(8), intent(in), optional :: tol

    interface
       function f(x) result(y)
         real(8), intent(in) :: x
         real(8) :: y
       end function f

       function df(x) result(y)
         real(8), intent(in) :: x
         real(8) :: y
       end function df
    end interface

    integer :: n, nmax
    real(8) :: dx, tolerance

    if (.not. present(maxit)) then
       nmax = default_maxit
    else
       nmax = maxit
    end if

    if (.not. present(tol)) then
       tolerance = default_tol
    else
       tolerance = tol
    end if

    status = 1
    do n = 1, nmax
       dx =- f(x) / (df(x) + epsilon)
       x  = x + dx
       error = abs(dx)

       ! check convergence
       if (error < tolerance) then
          status = 0
          exit
       end if
    end do

    return
  end subroutine newton

  !
  ! secant method
  !
  subroutine secant(f, x, error, status, maxit, tol)
    implicit none
    real(8), intent(inout) :: x
    real(8), intent(out) :: error
    integer, intent(out) :: status
    integer, intent(in), optional :: maxit
    real(8), intent(in), optional :: tol

    interface
       function f(x) result(y)
         real(8), intent(in) :: x
         real(8) :: y
       end function f
    end interface

    integer :: n, nmax
    real(8) :: x1, x2, dx, dy, y1, y2, tolerance

    if (.not. present(maxit)) then
       nmax = default_maxit
    else
       nmax = maxit
    end if

    if (.not. present(tol)) then
       tolerance = default_tol
    else
       tolerance = tol
    end if

    ! initial guess
    x1 = x
    x2 = x * 1.001

    status = 1
    y1 = f(x1)
    y2 = f(x2)
    do n = 1, nmax
       dy = (x2 - x1)/(y2 - y1 + epsilon)
       dx =- y2 * dy
       error = abs(dx)
       x1 = x2
       x2 = x2 + dx
       y1 = y2
       y2 = f(x2)

       ! check convergence
       if (error < tolerance) then
          status = 0
          exit
       end if
    end do

    x = x1
  end subroutine secant

end module mod_root

!
! module for equation to be solved
!
module mod_equation
  implicit none
  private

  real(8) :: x      ! normalized distance from the sun
  real(8) :: gamma  ! polytropic index
  real(8) :: energy ! energy constant

  public :: set_gamma, set_distance, f, df

contains
  ! set gamma value
  subroutine set_gamma(gam)
    implicit none
    real(8), intent(in) :: gam

    gamma  = gam
    energy = (5 - 3*gamma)/(gamma - 1)

  end subroutine set_gamma

  ! set distance
  subroutine set_distance(distance)
    implicit none
    real(8), intent(in) :: distance

    x = distance

  end subroutine set_distance

  ! equation to be solved
  function f(u) result(y)
    implicit none
    real(8), intent(in) :: u
    real(8) :: y

    y = u**(gamma+1) - (4/x + energy)*u**(gamma-1) + 2/(gamma-1)*x**(2-2*gamma)

  end function f

  ! df/du
  function df(u) result(y)
    implicit none
    real(8), intent(in) :: u
    real(8) :: y

    y = (gamma+1)*u**gamma - (gamma-1)*(4/x + energy)*u**(gamma-2)

  end function df

end module mod_equation

!
! main program
!
program kadai3
  use mod_root
  use mod_equation
  implicit none

  integer, parameter :: m = 200
  integer, parameter :: n = 2*m + 1

  integer :: i, j, status
  real(8) :: x(n), u(n), mach(n), err(n)
  real(8) :: xmax, xmin, delta
  real(8) :: gamma

  xmin = 1.0e-3_8
  xmax = 1.0e+3_8

  ! read polytropic index from stdin
  read(*,*) gamma
  if( gamma < 1.0_8 .or. gamma > 5.0_8/3.0_8 ) then
     write(*,*) 'Error: invalid polytropic index', gamma
  end if

  call set_gamma(gamma)

  !
  ! critical point
  !
  x(m+1)   = 1.0_8
  u(m+1)   = 1.0_8
  err(m+1) = 0.0_8

  !
  ! search for subsonic region (x < 1)
  !
  delta = 10**(log10(xmin)/m) - 1

  do i = 1, m
     j = m + 1 - i
     x(j) = x(j+1) * (1 + delta)
     u(j) = u(j+1)

     ! find root
     call set_distance(x(j))
     call newton(f, df, u(j), err(j), status)

     if( status /= 0 ) then
        write(0,*) 'Error: unable to find root: at x = ', x(j)
        stop
     end if
  end do

  !
  ! search for supersonic region (x > 1)
  !
  delta = 10**(log10(xmax)/m) - 1

  do i = 1, m
     j = m + 1 + i
     x(j) = x(j-1) * (1 + delta)
     u(j) = u(j-1)

     ! find root
     call set_distance(x(j))
     call newton(f, df, u(j), err(j), status)

     if( status /= 0 ) then
        write(0,*) 'Error: unable to find root: at x = ', x(j)
        stop
     end if
  end do

  ! sound speed
  mach = u / (x**(1-gamma) * u**(0.5*(1-gamma)))

  ! output data
  do i = 1, n
     write(*, '(e20.8, e20.8, e20.8, e20.8)') x(i), u(i), mach(i), err(i)
  end do

end program kadai3
