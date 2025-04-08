module mod_sort
  implicit none
  private

  ! change the following flag to .true. if you want to test the sort routine
  logical, parameter :: test_qsort = .true.
  logical, parameter :: test_msort = .true.
  logical, parameter :: test_hsort = .true.
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
  ! swap in quick sort
  !
  recursive subroutine qswap(x, pivot, left, right)
    implicit none
    integer, intent(inout) :: x(:)
    integer, intent(in)    :: pivot, left, right

    integer :: p, l, r, xp

    xp = x(pivot)

    l = left
    r = right
    do
       do while(x(l) < xp)
          l = l + 1
       end do

       do while(x(r) > xp)
          r = r - 1
       end do

       if( l >= r ) then
          exit
       end if

       call swap(x(l), x(r))
       l = l + 1
       r = r - 1
    end do

    if( left < l-1 ) then
       p = left
       call qswap(x, p, left, l-1)
    end if

    if( r+1 < right ) then
       p = r+1
       call qswap(x, p, r+1, right)
    end if

  end subroutine qswap

  !
  ! quick sort
  !
  subroutine qsort(x)
    implicit none
    integer, intent(inout) :: x(:)

    call qswap(x, 1, 1, size(x))

  end subroutine qsort

  !
  ! merge two arrays
  !
  subroutine mmerge(x, x1, x2)
    implicit none
    integer, intent(inout) :: x(:)
    integer, intent(in)    :: x1(:), x2(:)

    integer :: i, i1, i2, n, n1, n2

    n  = size(x)
    n1 = size(x1)
    n2 = size(x2)

    i1 = 1
    i2 = 1
    do i = 1, n
       if( x1(i1) < x2(i2) ) then
          x(i) = x1(i1)
          i1 = i1 + 1
       else
          x(i) = x2(i2)
          i2 = i2 + 1
       end if

       ! either x1 or x2 is empty
       if( i1 > n1 .or. i2 > n2 ) then
          exit
       end if
    end do

    i = i + 1

    ! if x2 is empty
    if( i2 > n2 ) then
       do while(i1 <= n1)
          x(i) = x1(i1)
          i  = i  + 1
          i1 = i1 + 1
       end do
    end if

    ! if x1 is empty
    if( i1 > n1 ) then
       do while(i2 <= n2)
          x(i) = x2(i2)
          i  = i  + 1
          i2 = i2 + 1
       end do
    end if

  end subroutine mmerge

  !
  ! devide array (sort) and merge
  !
  recursive subroutine mdivide(x, y)
    implicit none
    integer, intent(inout) :: x(:)
    integer, intent(inout) :: y(:)

    integer :: n, m

    n = size(x)
    m = (1 + n)/2

    if( n == 1 ) then
       return
    end if

    ! assumeing y == x is satisfied in advance
    call mdivide(x(1:m), y(1:m))
    call mdivide(x(m+1:n), y(m+1:n))
    call mmerge(y, x(1:m), x(m+1:n))
    x = y

  end subroutine mdivide

  !
  ! merge sort
  !
  subroutine msort(x)
    implicit none
    integer, intent(inout) :: x(:)

    integer :: n
    integer, allocatable :: y(:)

    n = size(x)
    allocate(y(n))

    y = x
    call mdivide(x, y)

    deallocate(y)

  end subroutine msort

  !
  ! insert an element into heap
  !
  subroutine hinsert(x, i, n)
    implicit none
    integer, intent(inout) :: x(:)
    integer, intent(in)    :: i, n

    integer :: j, k

    j = i
    do while(j <= n/2)
       k = 2*j
       ! <<< Caution >>>
       ! single clause like "k < n .and. x(k) < x(k+1)" does not work here
       ! in contrast to "&&" operator in C/C++
       if( k < n ) then
          if( x(k) < x(k+1) ) then
             k = k + 1
          end if
       end if

       if( x(j) < x(k) ) then
          call swap(x(j), x(k))
          j = k
       else
          exit
       end if
    end do

  end subroutine hinsert

  !
  ! heap sort
  !
  subroutine hsort(x)
    implicit none
    integer, intent(inout) :: x(:)

    integer :: i, n

    n = size(x)

    ! construct heap
    do i = n/2, 1, -1
       call hinsert(x, i, n)
    end do

    ! sort
    do i = 1, n
       call swap(x(1), x(n-i+1))
       call hinsert(x, 1, n-i)
    end do

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
