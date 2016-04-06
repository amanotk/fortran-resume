program sample
  implicit none

  integer :: i, n, ios
  real(8) :: x, y

  n = 64

  !
  ! ファイルを開く
  !
  ! unit     : 装置番号(ファイルを識別するための数値)
  ! iostat   : 正常時は0
  ! file     : ファイル名
  ! action   : 操作
  ! form     : formatted(アスキー形式) or unformatted(バイナリ形式)
  ! status   : new, old, replace
  ! position : rewind, append
  !

  ! * ファイルの新規作成(既に存在する場合はエラー)
  open(unit=10, iostat=ios, file='ascii.dat', action='write', &
       & form='formatted', status='new')

  ! * ファイルの新規作成(既に存在する場合は中身を破棄)
  !open(unit=10, iostat=ios, file='ascii.dat', action='write', &
  !     & form='formatted', status='replace')

  ! * 既に存在するファイルを開き, 位置をファイル終端に指定する(データの付け足し)
  !open(unit=10, iostat=ios, file='ascii.dat', action='write', &
  !     & form='formatted', status='old', position='append')

  ! ファイルが正常に開けたかどうかをチェックする
  if (ios /= 0) then
     write(*,*) 'Failed to open file for output'
     stop
  end if

  ! ファイル(装置番号=10)にデータを書き出し
  do i = 1, 64
     x = real(i,8)/real(n-1,8)
     y = cos(2*3.1415_8 * x)
     write(10, '(e20.8, e20.8)') x, y
  end do

  close(10)

  !
  ! 既に存在するファイルを読み込み専用で開き, 位置をファイル先頭に指定する
  ! (ただしpositionは指定しなくても良い)
  !
  open(unit=20, iostat=ios, file='ascii.dat', action='read', &
       & form='formatted', status='old', position='rewind')

  ! またチェック
  if (ios /= 0) then
     write(*,*) 'Failed to open file for input'
     stop
  end if

  do i = 1, n
     ! データを読み込んで表示
     read(20,*) x, y
     write(*, '(e20.8, e20.8)') x, y
  end do

  close(20)

  stop
end program sample
