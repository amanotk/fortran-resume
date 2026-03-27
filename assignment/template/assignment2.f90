program kadai2
  implicit none

  integer, parameter :: power = 6
  integer :: i
  integer :: nx(power)
  real(8) :: cputime(2, power)

  call random_seed_clock()

  !
  ! check sort results (each sort is performed 10 times with different inputs)
  !
  write(0, fmt='(a)', advance='no') 'checking bubble_sort ...'
  call check_sort(bubble_sort, 1024, 1024, 10)
  write(0, *) 'done'

  write(0, fmt='(a)', advance='no') 'checking my_sort     ...'
  call check_sort(my_sort, 1024, 1024, 10)
  write(0, *) 'done'

  !
  ! now measure sort performance
  !
  cputime = 0.0_8
  i = 1
  nx(i) = 16
  call measure(bubble_sort, nx(i), cputime(1,i))
  call measure(my_sort, nx(i), cputime(2,i))

  do i = 2, power
     nx(i) = nx(i-1) * 4
     ! bubble sort
     write(0, fmt='("bubble_sort for N = ", i6, " ...")', advance='no') nx(i)
     call flush(0)
     call measure(bubble_sort, nx(i), cputime(1,i))
     write(0,*) 'done'

     ! my_sort
     write(0, fmt='("my_sort     for N = ", i6, " ...")', advance='no') nx(i)
     call flush(0)
     call measure(my_sort, nx(i), cputime(2,i))
     write(0,*) 'done'
  end do

  write(*, fmt='(a12, 2x, 2(a14))') '# data size', 'bubble_sort', 'my_sort'
  do i = 1, power
     write(*, '(i12, 2x, 2(e14.3))') nx(i), &
          & cputime(1,i), cputime(2,i)
  end do

contains

  !
  ! random seed
  !
  subroutine random_seed_clock()
    implicit none
    integer :: nseed, clock
    integer, allocatable :: seed(:)

    call system_clock(clock)

    call random_seed(size=nseed)
    allocate(seed(nseed))

    seed = clock
    call random_seed(put=seed)

    deallocate(seed)
  end subroutine random_seed_clock

  !
  ! make integer random numbers in the range [0, maxint-1]
  !
  subroutine randint(x, maxint)
    implicit none
    integer, intent(out) :: x(:)
    integer, intent(in)  :: maxint

    real(8) :: r(size(x))

    call random_number(r)

    x = int(maxint * r)

  end subroutine randint

  !
  ! measure cpu time
  !
  subroutine measure(f, n, cputime)
    implicit none
    integer, intent(in)  :: n
    real(8), intent(out) :: cputime
    ! declare sorting subroutine
    interface
       subroutine f(x)
         integer, intent(inout) :: x(:)
       end subroutine f
    end interface

    integer :: x(n)
    integer :: i, m
    real(8) :: t1, t2, dt

    call randint(x, 100*n)

    ! estimate execution time
    m = 1
    call cpu_time(t1)
    call f(x)
    call cpu_time(t2)
    dt = t2 - t1

    if(dt < 5.0) then
       !
       ! when the execution time is short enough, try to estimate it more
       ! accurately by calling the routine many times
       !
       if(dt < 1.0e-8) then
          m = 20000
       else if(dt < 1.0e-3) then
          m = 1000
       else
          m = int(5 / dt)
       end if

       ! start actual measurement
       dt = 0
       do i = 1, m
          call randint(x, 100*n)
          call cpu_time(t1)
          call f(x)
          call cpu_time(t2)
          dt = dt + (t2 - t1)/m
       end do
    end if

    cputime = dt

  end subroutine measure

  !
  ! check sort (for debugging purpose)
  !
  subroutine check_sort(f, n, maxint, count)
    implicit none
    integer, intent(in) :: n
    integer, intent(in) :: maxint
    integer, intent(in) :: count

    ! declare sorting subroutine
    interface
       subroutine f(x)
         integer, intent(inout) :: x(:)
       end subroutine f
    end interface

    integer :: i, j
    integer :: x(n)

    do j = 1, count
       call randint(x, maxint)

       call f(x)

       do i = 1, n-1
          if( x(i) > x(i+1) ) then
             write(0,*) 'sort failed !'
             stop
          end if
       end do
    end do

  end subroutine check_sort

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
  ! bubble sort
  !
  subroutine bubble_sort(x)
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

  end subroutine bubble_sort

  !
  ! my sort
  !
  subroutine my_sort(x)
    implicit none
    integer, intent(inout) :: x(:)

    ! IMPLEMENT ME !
    call bubble_sort(x)

  end subroutine my_sort

end program kadai2
