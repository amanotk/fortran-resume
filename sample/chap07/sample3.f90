program sample
  implicit none

  ! 16進数変換のためのテーブル
  character(len=1), parameter :: hex_char(0:15) = &
       & (/'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', &
       &   'A', 'B', 'C', 'D', 'E', 'F'/)

  integer :: n = 10
  character(len=8) :: hexstr

  call sub()

  ! 整数を16進数に変換して表示
  n = 15*16**6 + 4*16**4 + 3*16**3 + 16**2 + 1
  call decimal2hex(n, hexstr)
  write(*,*) 'decimal = ', n, ' ===> hex = ', hexstr

  stop
contains
  ! 内部手続の定義
  subroutine sub()
    implicit none
    ! もし以下の行があればメインプログラムのnとサブプログラムのnは独立
    !integer :: n

    write(*,*) n        ! メインプログラム中の変数nにアクセス
  end subroutine sub

  ! 10進数を16進数に変換
  subroutine decimal2hex(decimal, hex)
    implicit none
    integer :: decimal
    character(len=*) :: hex

    integer :: i, n, d

    d = decimal
    do i = 1, 8
       n = d / 16**(8-i)
       d = d - n * 16**(8-i)
       ! 呼び出し元で宣言された変数(hex_char)を参照
       hex(i:i) = hex_char(n)
    end do

  end subroutine decimal2hex

end program sample
