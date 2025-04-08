program kadai4
  implicit none

  integer :: n   ! number of cells
  integer :: m   ! number of steps to compute

  integer :: i, j, r
  integer :: rule(8)
  integer, allocatable :: x(:), y(:)

  ! read header from stdin
  call read_header(rule, n, m)

  ! memory allocation
  allocate(x(n))
  allocate(y(n))

  ! read initial condition
  read(*,*) x

  ! output initial condition
  call output(x)

  do i = 1, m
     ! substitue to temporary array
     y = x

     ! update (x(1) and x(n) do not change)
     do j = 2, n-1
        r = 4*y(j-1) + 2*y(j) + y(j+1)
        x(j) = rule(8 - r)
     end do

     ! output
     call output(x)
  end do

  deallocate(x)
  deallocate(y)

  stop
contains

  !
  ! convert a rule expressed in a decimal number to binary expression
  !
  subroutine dec2bin(dec, bin)
    implicit none
    integer, intent(in)  :: dec
    integer, intent(out) :: bin(8)

    integer :: i, d

    ! check input
    if( dec >= 2**8 .or. dec < 0 ) then
       write(*,*) 'Error: invalid rule'
       stop
    end if

    d = dec

    ! now convert to binary expression
    do i = 1, 8
       bin(9-i) = mod(d, 2)
       d = d/2
    end do

  end subroutine dec2bin

  !
  ! read rule, number of cells, and number of steps to compute
  !
  subroutine read_header(rule, n, m)
    implicit none
    integer, intent(out) :: rule(8)
    integer, intent(out) :: n
    integer, intent(out) :: m

    integer :: i, j, decrule, nonzero
    real(8) :: rand

    ! read and convert rule
    read(*,*) decrule
    call dec2bin(decrule, rule)

    ! read number of cells
    read(*,*) n

    ! read number of time steps
    read(*,*) m

  end subroutine read_header

  !
  ! output
  !
  subroutine output(x)
    implicit none
    integer, intent(inout) :: x(:)

    integer :: j

    do j = 1, size(x)
       write(*, fmt='(i2)', advance='no') x(j)
    end do
    write(*,*)

  end subroutine output

end program kadai4
