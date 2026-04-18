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
    
    integer :: ix, iy, size_grid
    real(8) :: step
    
    size_grid = 2**(n + 1)
    step = 1.0_8 / size_grid
    
    ! Start at position that keeps curve in [0, 1]
    ! The LDR curve extends left and down from start, so we start at top-right quadrant
    ix = size_grid - 1
    iy = size_grid - 1
    
    ! Generate Hilbert curve starting with LDR orientation
    call ldr(n + 1, ix, iy, step)
    
  end subroutine generate_hilbert

  recursive subroutine ldr(m, x, y, step)
    implicit none
    integer, intent(in) :: m
    integer, intent(inout) :: x, y
    real(8), intent(in) :: step
    
    if (m == 0) then
      write(*, '(2(f20.10))') (x + 0.5_8) * step, (y + 0.5_8) * step
      return
    end if
    
    call dlu(m - 1, x, y, step)
    x = x - 1                       ! L: move left
    call ldr(m - 1, x, y, step)
    y = y - 1                       ! D: move down
    call ldr(m - 1, x, y, step)
    x = x + 1                       ! R: move right
    call urd(m - 1, x, y, step)
    
  end subroutine ldr

  recursive subroutine urd(m, x, y, step)
    implicit none
    integer, intent(in) :: m
    integer, intent(inout) :: x, y
    real(8), intent(in) :: step
    
    if (m == 0) then
      write(*, '(2(f20.10))') (x + 0.5_8) * step, (y + 0.5_8) * step
      return
    end if
    
    call rul(m - 1, x, y, step)
    y = y + 1                       ! U: move up
    call urd(m - 1, x, y, step)
    x = x + 1                       ! R: move right
    call urd(m - 1, x, y, step)
    y = y - 1                       ! D: move down
    call ldr(m - 1, x, y, step)
    
  end subroutine urd

  recursive subroutine rul(m, x, y, step)
    implicit none
    integer, intent(in) :: m
    integer, intent(inout) :: x, y
    real(8), intent(in) :: step
    
    if (m == 0) then
      write(*, '(2(f20.10))') (x + 0.5_8) * step, (y + 0.5_8) * step
      return
    end if
    
    call urd(m - 1, x, y, step)
    x = x + 1                       ! R: move right
    call rul(m - 1, x, y, step)
    y = y + 1                       ! U: move up
    call rul(m - 1, x, y, step)
    x = x - 1                       ! L: move left
    call dlu(m - 1, x, y, step)
    
  end subroutine rul

  recursive subroutine dlu(m, x, y, step)
    implicit none
    integer, intent(in) :: m
    integer, intent(inout) :: x, y
    real(8), intent(in) :: step
    
    if (m == 0) then
      write(*, '(2(f20.10))') (x + 0.5_8) * step, (y + 0.5_8) * step
      return
    end if
    
    call ldr(m - 1, x, y, step)
    y = y - 1                       ! D: move down
    call dlu(m - 1, x, y, step)
    x = x - 1                       ! L: move left
    call dlu(m - 1, x, y, step)
    y = y + 1                       ! U: move up
    call rul(m - 1, x, y, step)
    
  end subroutine dlu

end program assignment3
