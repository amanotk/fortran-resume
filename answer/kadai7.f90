program kadai7
  implicit none

  integer, parameter :: np = 20000
  integer, parameter :: nb = 32

  integer :: i, j, nt, mt
  real(8) :: u, d, t, tau, tmax, xmin, xmax
  real(8) :: xp(np), xbin(nb), fx(nb)

  t   = 0.0_8
  u   = 0.5_8
  d   = 1.0_8
  tau = 1.0e-3
  tmax =  4.0_8
  xmin = -10.0_8
  xmax = +10.0_8

  nt = tmax/tau
  mt = nt / 4 + 1
  call random_seed_clock()
  call init(xp)

  do i = 1, nt + 1
     call push(xp, u, d, tau)
     call histogram(xp, xmin, xmax, nb, xbin, fx)
     t = t + tau

     ! output data
     if( mod(i,mt) == 0 ) then
        do j = 1, nb
           write(*, '(e15.8, x, e15.8, x, e15.8)') t, xbin(j), fx(j)
        end do
        write(*,*)
     end if
  end do

contains

  ! initialization
  subroutine init(xp)
    implicit none
    real(8), intent(out) :: xp(:)

    xp = 0.0_8
  end subroutine init

  ! push particle positions
  subroutine push(xp, u, d, tau)
    implicit none
    real(8), intent(inout) :: xp(:)
    real(8), intent(in) :: u, d, tau

    integer :: i, np
    real(8) :: r(size(xp))

    call random_number(r)

    r = sign(1.0_8, r-0.5_8)
    xp = xp + u*tau + r*sqrt(2*d*tau)

  end subroutine push

  ! calculate histogram
  subroutine histogram(x, xmin, xmax, nbin, binc, hist)
    implicit none
    real(8), intent(in) :: x(:)
    real(8), intent(in) :: xmin, xmax
    integer, intent(in) :: nbin
    real(8), intent(out) :: binc(nbin)
    real(8), intent(out) :: hist(nbin)

    integer :: i, j, np
    real(8) :: h, norm

    np   = size(x)
    h    = (xmax - xmin) / nbin
    norm = 1.0_8 / (size(x) * h)
    hist = 0.0_8

    do i = 1, np
       j = int( (x(i)-xmin)/h ) + 1
       if( j < 1 .or. j > nbin ) cycle

       hist(j) = hist(j) + 1.0_8
    end do

    do j = 1, nbin
       hist(j) = hist(j) * norm
       binc(j) = (j - 0.5_8) * h + xmin
    end do

  end subroutine histogram

  ! set random seed
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

end program kadai7
