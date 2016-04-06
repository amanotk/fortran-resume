program sample
  implicit none

  character(8) :: hexstr
  integer :: i
  real(8) :: a, b, c
  real(8) :: x(10)

  do i = 1, 10
     x(i) = real(i, 8)
  end do

  ! intent属性
  a = 1.0
  b = 2.0
  call add(a, b, c)
  write(*,*) a, b, c

  ! 配列の渡し方
  write(*,*) 'average ===> ', average1(x), average2(10, x)

  ! save属性の使い方
  do i = 1, 10
     call fibonacci()
  end do

  ! optional
  call hello('Michael')     ! 標準出力へ
  call hello('Jackson', 0)  ! 標準エラー出力へ

  ! キーワード引数の使い方
  call hello(unit=6, name='Albert') ! 標準出力へ
  call hello(name='Einstein')       ! 標準出力へ

  ! rescursive
  write(*,*) fact(2), fact(4), fact(6)

  stop
contains

  !
  ! <<< intent属性 >>>
  !
  ! * intent(in)    => 入力用変数(値の変更不可)
  ! * intent(out)   => 出力用変数
  ! * intent(inout) => 入出力
  !
  subroutine add(a, b, c)
    implicit none
    real(8), intent(in)  :: a, b
    real(8), intent(out) :: c

    ! 以下はコンパイルエラー
    !a = 1.0_8

    ! 出力用の変数に値を代入
    c = a + b

  end subroutine add

  !
  ! <<< 形状引継ぎ配列の使い方 >>>
  !
  function average1(x) result(ave)
    implicit none
    real(8), intent(in) :: x(:)
    real(8) :: ave

    ave = sum(x) / size(x)

  end function average1

  !
  ! <<< 配列サイズの引数渡し >>>
  !
  function average2(n, x) result(ave)
    implicit none
    integer, intent(in) :: n
    real(8), intent(in) :: x(n)
    real(8) :: ave

    ave = sum(x) / size(x)

  end function average2

  !
  ! <<< save属性 >>>
  !
  subroutine fibonacci()
    implicit none
    ! 以下の3つがsave属性付き
    integer, save :: n  = 1
    integer, save :: f0 = 0
    integer, save :: f1 = 0

    integer :: f2

    if (n == 1) then
       write(*,*) 'Fibonacci number [', 0, '] = ', f0
       f2 = 1
    else
       f2 = f0 + f1
    end if

    write(*,*) 'Fibonacci number [', n, '] = ', f2

    ! 次回呼び出し用
    n  = n + 1
    f0 = f1
    f1 = f2

  end subroutine fibonacci

  !
  ! <<< optional属性 >>>
  !
  subroutine hello(name, unit)
    implicit none
    character(len=*), intent(in)  :: name
    integer, intent(in), optional :: unit

    integer :: u

    if( present(unit) ) then
       u = unit ! unitを指定
    else
       u = 6    ! デフォルトは標準出力
    end if

    write(u,*) 'Hello ', name ! 表示

    return
  end subroutine hello

  !
  ! <<< recursive (再帰的呼び出し) >>>
  !
  recursive function fact(n) result(m)
    implicit none
    integer, intent(in) :: n
    integer :: m

    if(n == 1) then
       m = 1
    else
       m = n*fact(n-1)
    end if

  end function fact

end program sample
