program sample
  implicit none

  integer, parameter :: s = 12
  integer, parameter :: n = 4
  integer, parameter :: m = s / n

  integer :: i, j
  real(8) :: x(n,m)

  ! リダイレクトを用いて
  !   $ ./a.out < data.dat
  ! のようにデータを読み込む
  ! 配列のメモリが連続した要素に順に読み込まれる
  read(*,*) x

  ! 以下の方法では上手く読み込めない(readが行末まで読み込むため)
  !do j = 1, m
  !   do i = 1, n
  !      read(*,*) x(i,j)
  !   end do
  !end do

  ! 出力
  do j = 1, m
     do i = 1, n
        write(*,*) i, j, x(i,j)
     end do
  end do

  stop
end program sample
