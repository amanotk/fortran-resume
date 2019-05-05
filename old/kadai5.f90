!
! module for counting unique key strings using hash table
!
! This code can be compiled with gfortran >= 4.9
!
module mod_count
  implicit none

  integer, parameter :: hashsize = 10007 ! sufficiently large prime number
  integer, parameter :: maxtry   = 10    ! number of retry for hash collision

  ! data structure for hash
  type :: hash_type
     integer :: count
     character(len=:), allocatable :: key
  end type hash_type

  ! hash table
  type(hash_type) :: hashtable(hashsize)

contains

  ! initialize hash table
  subroutine init_hash()
    implicit none

    integer :: i

    do i = 1, hashsize
       hashtable(i)%count = 0
    end do

  end subroutine init_hash

  !
  ! hash function
  !
  ! convert a given string to an integer
  ! returned integer can be used for the index of hashtable
  !
  function hash(ch) result(ret)
    implicit none
    character(*), intent(in) :: ch
    integer :: ret

    integer :: i, j, n, mul


    ret = 0
    n   = len(ch)/4

    ! for each four-byte chunks
    do i = 1, n
       mul = 1
       do j = 1, 4
          ret = ret + mul * ichar(ch(4*(i-1)+j:4*(i-1)+j))
          mul = mul * 256
       end do
    end do

    ! remainder
    mul = 1
    do j = 1, mod(len(ch), 4)
       ret = ret + mul * ichar(ch(4*n+j:4*n+j))
       mul = mul * 256
    end do

    ! abs() needed to fix possible overflow
    ret = abs(ret)

    ! convert to integer in the range [1, hashsize]
    ret = mod(abs(ret), hashsize) + 1

  end function hash

  !
  ! read line
  !
  ! This routine tries to read a whole line into a string to be returned.
  ! The allocatable character feature and iso_fortran_env module, both
  ! introduced in the Fortran 2003 standard, are used.
  !
  function read_line(unit, status) result(line)
    use iso_fortran_env
    implicit none
    integer, intent(in)       :: unit
    integer, intent(out)      :: status
    character(:), allocatable :: line

    integer, parameter        :: n = 32
    integer                   :: nread
    character(len=n)          :: buf
    character(:), allocatable :: str

    ! initialize
    str = ''

    ! first attempt to read string into buffer
    read(unit, fmt='(a)', advance='no', size=nread, iostat=status) buf

    ! continue until either end-of-file or end-of-line is detected
    do while( .not. (is_iostat_eor(status) .or. is_iostat_end(status)) )
       str = str // buf(1:nread)
       read(unit, fmt='(a)', advance='no', size=nread, iostat=status) buf
    end do

    str = str // buf(1:nread)

    ! return
    line = str

  end function read_line

  !
  ! process one line
  !
  ! This routine first calculate an address (or index) by hash() function to
  ! which a given key string is stored. If there is no data stored on the
  ! address, register the key
  !
  subroutine process_line(line)
    implicit none
    character(*), intent(in) :: line

    integer :: nstr, addr, ntry
    character(len(line)) :: str

    ! ignore white space or newline
    if( str == '' .or. str == '\n' ) then
       return
    end if

    str  = trim(line)
    nstr = len(str)
    addr = hash(str)

    ! calculate address for the hashtable by hash() function
    addr = hash(str)
    ntry = 1

    ! loop to avoid hash collision
    do while( .true. )
       if( hashtable(addr)%count == 0 ) then
          ! new key
          hashtable(addr)%key   = str
          hashtable(addr)%count = hashtable(addr)%count + 1
          ! write to stdout
          write(*,*) str
          exit
       else if( hashtable(addr)%key == str ) then
          ! already registered key
          hashtable(addr)%count = hashtable(addr)%count + 1
          exit
       else
          ! collision => try to find vacancy
          write(0,*) 'Collision detected !', ntry
          addr = mod(addr+1, hashsize)
          ntry = ntry + 1
          if( ntry > maxtry ) then
             write(0,*) 'Error ! : cannot find vacancy in hashtable'
             call flush()
             stop
          end if
       end if
    end do

  end subroutine process_line

end module mod_count

!
! main program
!
program kadai5
  use iso_fortran_env
  use mod_count
  implicit none

  integer, parameter :: stdin = 5
  integer :: status
  character(len=:), allocatable :: line

  ! initialize
  call init_hash()
  line = read_line(stdin, status)

  ! continue until end-of-file is detected
  do while( .not. is_iostat_end(status) )
     call process_line(line)
     line = read_line(stdin, status)
  end do

  stop
end program kadai5
