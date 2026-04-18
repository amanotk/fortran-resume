program assignment3
  implicit none
  integer :: n
  write(0, fmt='(a)', advance='no') 'Input the order of Hilbert curve n : '
  read(*, *) n
  call generate_hilbert(n)
contains
  subroutine generate_hilbert(n)
    implicit none
    integer, intent(in) :: n
    integer :: size_grid, total, i, x, y
    real(8) :: step
    size_grid = 2**(n + 1)
    step = 1.0_8 / size_grid
    total = size_grid * size_grid
    do i = 0, total - 1
      call d2xy(i, n + 1, x, y)
      write(*, '(2(f20.10))') (x + 0.5_8) * step, (y + 0.5_8) * step
    end do
  end subroutine
  
  subroutine d2xy(d, n, x, y)
    implicit none
    integer, intent(in) :: d, n
    integer, intent(out) :: x, y
    integer :: t, rx, ry, s, temp, i
    x = 0; y = 0
    t = d
    do i = 0, n - 1
      s = 2**i
      rx = mod(t, 2)
      ry = mod(t / 2, 2)
      if (ry == 0) then
        if (rx == 1) then
          x = s - 1 - x
          y = s - 1 - y
        end if
        temp = x
        x = y
        y = temp
      end if
      x = x + rx * s
      y = y + ry * s
      t = t / 4
    end do
  end subroutine
end program
