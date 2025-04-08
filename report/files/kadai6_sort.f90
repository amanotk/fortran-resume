module mod_sort
  implicit none
  private

  ! change the following flag to .true. if you want to test the sort routine
  logical, parameter :: test_qsort = .false.
  logical, parameter :: test_msort = .false.
  logical, parameter :: test_hsort = .false.
  logical, parameter :: test_bsort = .true.

  public :: test_qsort, test_msort, test_hsort, test_bsort
  public :: qsort, msort, hsort, bsort

contains

  !
  ! swap two arguments
  !
  subroutine swap(a, b)
    implicit none
    integer, intent(inout) :: a, b

    integer :: c

    c = a
    a = b
    b = c

  end subroutine swap

  !
  ! quick sort
  !
  subroutine qsort(x)
    implicit none
    integer, intent(inout) :: x(:)

    call bsort(x)

  end subroutine qsort

  !
  ! merge sort
  !
  subroutine msort(x)
    implicit none
    integer, intent(inout) :: x(:)

    call bsort(x)

  end subroutine msort

  !
  ! heap sort
  !
  subroutine hsort(x)
    implicit none
    integer, intent(inout) :: x(:)

    call bsort(x)

  end subroutine hsort

  !
  ! bubble sort
  !
  subroutine bsort(x)
    implicit none
    integer, intent(inout) :: x(:)

    integer :: i, j, n

    n = size(x)
    do i = 1, n
       do j = 1, n-i
          if( x(j) > x(j+1) ) then
             call swap(x(j), x(j+1))
          end if
       end do
    end do

  end subroutine bsort

end module mod_sort
