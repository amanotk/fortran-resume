program sample
  implicit none

  !
  ! 外部サブルーチンを使う
  ! (サブルーチンの場合はこれが無くても動く)
  !
  interface
     subroutine hello_ext(name)
       character(len=*) :: name
     end subroutine hello_ext
  end interface

  ! こういう書き方もできる
  !external :: hello_ext

  call hello('Michel')
  call hello_ext('Jackson')

  stop
contains
  !
  ! サブルーチンの宣言(内部手続き)
  !
  subroutine hello(name)
    implicit none
    character(len=*) :: name

    write(*,*) 'Hello ', name

    return
  end subroutine hello

end program sample

!
! サブルーチンの宣言(外部手続き)
!
subroutine hello_ext(name)
  implicit none
  character(len=*) :: name

  write(*,*) 'Hello (ext) ', name

  return
end subroutine hello_ext
