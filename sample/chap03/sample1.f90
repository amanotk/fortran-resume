program sample
  implicit none ! 暗黙の型宣言禁止

  ! 変数を使う前には必ず以下のように宣言を行う
  integer :: n, m ! 整数型(通常4バイト)
  real    :: x, y ! 実数型(通常4バイト)

  logical :: torf = .true. ! 論理型(このように初期化もできる)

  ! 整数を代入して表示
  n = 8
  write(*,*) 'integer => ', n

  ! 実数を代入して表示
  x = 3.14
  write(*,*) 'real (original) => ', x

  ! mにnの値を代入
  m = n
  write(*,*) 'real (copy) => ', m

  ! yにxの値を代入
  y = x

  ! xの値を変更
  x = 2.71

  ! yの値を表示（xを変更してもeは変更されない）
  write(*,*) 'real (not modified) => ', y

  ! 論理型の値を表示
  write(*,*) 'logical => ', torf

  stop
end program sample
