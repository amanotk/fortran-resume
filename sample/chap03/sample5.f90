program sample
  implicit none

  ! 8バイトの実数型
  real(8) :: x

  write(*,*) 'Input a real number: '

  ! 標準入力から値を読み込み変数xに代入
  read(*,*) x

  !
  ! 標準で様々な関数が用意されている
  ! 以下はほんの一例
  !
  ! 平方根     => sqrt(x)
  ! 絶対値     => abs(x)
  ! 三角関数   => sin(x), cos(x), tan(x)
  ! 指数関数   => exp(x)
  ! 対数関数   => log(x), log10(x)
  ! 双極関数   => sinh(x), cosh(x), tanh(x)
  ! 逆三角関数 => asin(x), acos(x), atan(x)
  !
  write(*,*) sin(x) ! sin(x)を計算し表示
  write(*,*) cos(x) ! cos(x)を計算し表示

  stop
end program sample
