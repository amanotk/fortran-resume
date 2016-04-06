program sample
  implicit none

  integer :: i

  ! 最も基本的な配列の宣言
  integer :: a(5)

  ! 配列の添字範囲を指定して宣言
  integer :: b(0:4)

  ! 宣言と同時に初期化
  integer :: c(5) = (/1, 2, 4, 8, 16/)

  ! 定数配列
  integer, parameter :: d(3) = (/-1, 0, 1/)

  ! 実数型
  real(8) :: x(100)
  real(8) :: sum

  ! doループで配列の各要素を処理する
  do i = 1, 5
     a(i) = i
  end do

  ! 各要素同士でのの演算も出来る
  do i = 0, 4
     b(i) = 2*c(i) + a(i)
  end do

  ! 出力
  do i = 1, 5
     write(*,*) 'a(', i, ') = ', a(i)
  end do

  ! このような書き方もできる
  write(*,*) 'b(0:4) = ', (b(i), i = 0, 4, 1)

  ! 単にこれだけでもよい
  write(*,*) 'c      = ', c

  ! 定数配列の書き換えはできない
  !d(1) = 100

  ! 値をセットして
  do i = 1, 100
     x(i) = real(i, kind=8)
  end do

  ! 配列の和を求める
  do i = 1, 100
     sum = sum + x(i)
  end do

  write(*,*) 'sum = ', sum

  stop
end program sample
