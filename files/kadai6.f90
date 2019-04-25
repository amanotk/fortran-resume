program kadai6
  use mod_sort
  implicit none

  integer, parameter :: power = 7
  integer :: i
  integer :: nx(power)
  real(8) :: cputime(4, power)

  call random_seed_clock()

  !
  ! check sort results (each sort is performed 10 times with different inputs)
  !
  write(0, fmt='(a)', advance='no') 'checking qsort...'
  call check_sort(qsort, 1024, 1024, 10)
  write(0, *) 'done'

  write(0, fmt='(a)', advance='no') 'checking msort...'
  call check_sort(msort, 1024, 1024, 10)
  write(0, *) 'done'

  write(0, fmt='(a)', advance='no') 'checking hsort...'
  call check_sort(hsort, 1024, 1024, 10)
  write(0, *) 'done'

  write(0, fmt='(a)', advance='no') 'checking bsort...'
  call check_sort(bsort, 1024, 1024, 10)
  write(0, *) 'done'

  !
  ! now measure sort performance
  !
  cputime = 0.0_8
  i = 1
  nx(i) = 16
  if( test_qsort ) then
     call measure(qsort, nx(i), cputime(1,i))
  end if
  if( test_msort ) then
     call measure(msort, nx(i), cputime(2,i))
  end if
  if( test_hsort ) then
     call measure(hsort, nx(i), cputime(3,i))
  end if
  if( test_bsort ) then
     call measure(bsort, nx(i), cputime(4,i))
  end if

  do i = 2, power
     nx(i) = nx(i-1) * 4
     ! quick sort
     if( cputime(1,i) < 5.0 .and. test_qsort ) then
        write(0, fmt='("qsort for N = ", i6, " ...")', advance='no') nx(i)
        call flush(0)
        call measure(qsort, nx(i), cputime(1,i))
        write(0,*) 'done'
     end if
     ! merge sort
     if( cputime(2,i) < 5.0 .and. test_msort ) then
        write(0, fmt='("msort for N = ", i6, " ...")', advance='no') nx(i)
        call flush(0)
        call measure(msort, nx(i), cputime(2,i))
        write(0,*) 'done'
     end if
     ! heap sort
     if( cputime(3,i) < 5.0 .and. test_hsort ) then
        write(0, fmt='("hsort for N = ", i6, " ...")', advance='no') nx(i)
        call flush(0)
        call measure(hsort, nx(i), cputime(3,i))
        write(0,*) 'done'
     end if
     ! bubble sort
     if( cputime(4,i) < 5.0 .and. test_bsort ) then
        write(0, fmt='("bsort for N = ", i6, " ...")', advance='no') nx(i)
        call flush(0)
        call measure(bsort, nx(i), cputime(4,i))
        write(0,*) 'done'
     end if
  end do

  do i = 1, power
     write(*, '(i8, 4x, 4(e12.3))') nx(i), &
          & cputime(1,i), cputime(2,i), cputime(3,i), cputime(4,i)
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

end program kadai6
