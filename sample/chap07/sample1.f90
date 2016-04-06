program sample
  implicit none
  !
  ! 外部関数を呼び出すために必要
  ! (本当は後で学ぶモジュールを使うほうがスマート)
  !
  interface
     real(8) function square_ext(x)
       real(8) :: x
     end function square_ext
  end interface

  ! 実はこれだけでもOKなのですが
  !real(8), external :: square_ext

  write(*,*) 2.0_8, square(2.0_8), square_ext(2.0_8)

  ! これはエラー
  !write(*,*) square(2.0)

  stop
contains
  ! --- 内部手続きの宣言はここから ---

  !
  ! 関数の宣言
  !
  function square(x) result(y)    ! squareという名前を宣言(引数がx、返値がy)
    implicit none                 !
    real(8) :: x                  ! 引数を宣言
    real(8) :: y                  ! 返値を宣言

    y = x**2                      ! 返り値

    return                        ! 呼び出し元に制御を戻す
  end function square

  ! --- ここまで ---
end program sample

!
! 外部手続き
!
function square_ext(x) result(y)
  implicit none
  real(8) :: x
  real(8) :: y

  y = x**2

  return
end function square_ext
