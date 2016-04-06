program sample
  implicit none

  integer :: i, j
  integer :: s(3)

  ! 2次元配列 (10 x 10 => 計100要素)
  real(8) :: a(10,10)

  ! 3次元配列 (4 x 8 x 16 => 計512要素)
  real(8) :: b(4, 8, 16)

  ! 動的配列も同様に宣言できる
  real(8), allocatable :: c(:,:)

  ! reshapeによって多次元配列を初期化
  integer, parameter :: x(2,3) = reshape((/1, 2, 3, 4, 5, 6/), (/2, 3/))

  ! 要素数: 4 x 8 x 16 = 512
  write(*,*) 'total number of elements = ', size(b)

  ! 配列の形状を取得(1次元の整数配列): (/4, 8, 16/)
  s = shape(b)

  ! 動的配列: 4 x 8
  if( .not. allocated(c) ) then
     allocate(c(s(1),s(2)))
  end if

  write(*,*) 'shape of c = ', shape(c)

  deallocate(c)

  ! 定数配列の中身を表示(reshapeの意味を確認 = Fortranはcolumn major)
  do j = 1, 3
     do i = 1, 2
        write(*,*) x(i,j)
     end do
  end do

  stop
end program sample
