program kadai8_2
  implicit none

  integer, allocatable :: quota(:)    ! number of available slots
  integer, allocatable :: nsd(:)      ! number of students after assignment
  integer, allocatable :: table(:,:)  ! student assignment table
  integer, allocatable :: spref(:,:)  ! students' list of preference
  integer, allocatable :: srank(:,:)  ! students ranking by department
  real(8), allocatable :: score(:,:)  ! students' score
  real(8), allocatable :: weight(:,:) ! subject weight
  real(8), allocatable :: temp(:)     ! temporary array
  integer :: nstd, ndept, nsubj
  integer :: i, j

  ! read number of departments and subjects
  read(*,*) ndept, nsubj

  allocate(quota(ndept))
  allocate(weight(ndept,nsubj))

  ! read available slots and subject weight
  do i = 1, ndept
     read(*,*) quota(i), weight(i,:)
  end do

  ! read number of students
  read(*,*) nstd

  allocate(nsd(ndept+1))
  allocate(table(nstd,ndept+1))
  allocate(score(nstd,nsubj))
  allocate(srank(nstd,ndept))
  allocate(spref(nstd,ndept))
  allocate(temp(ndept+nsubj))

  ! read student data
  do i = 1, nstd
     ! read into temporary buffer
     read(*,*) temp
     ! copy into score and list of preference
     score(i,:) = temp(1:nsubj)
     spref(i,:) = temp(nsubj+1:ndept+nsubj)
  end do

  ! calculate student ranking for each department
  call ranking(score, weight, srank)

  ! matching via Gale-Shapley algorithm
  call match(srank, spref, nsd, table, quota, nstd, ndept)

  !
  ! show results
  !
  do i = 1, ndept
     write(*, fmt='("Department[", i2, "] : [")', advance='no') i
     do j = 1, nsd(i)
        write(*, fmt='(x,i3)', advance='no') table(j,i)
     end do
     write(*, fmt='(a)') "]"
  end do

  ! sort failed students by ID
  i = ndept+1
  call bsort(table(1:nsd(ndept+1),i))
  write(*,*)
  write(*,fmt='(a)') 'Failed students:'
  do j = 1, nsd(i)
     write(*, fmt='(x,i3)', advance='no') table(j,i)
  end do
  write(*,*)

  ! check results just in case ;p
  call check(srank, spref, nsd, table, nstd, ndept)

  deallocate(quota)
  deallocate(weight)
  deallocate(nsd)
  deallocate(table)
  deallocate(score)
  deallocate(spref)
  deallocate(temp)

contains
  !
  ! Gale-Shapley Algorithm for department-student assignment
  !
  subroutine match(rank, pref, r, s, nslot, nstd, ndept)
    implicit none
    integer, intent(in)  :: rank(:,:)  ! students ranking by department
    integer, intent(in)  :: pref(:,:)  ! students' list of preference
    integer, intent(out) :: r(:)       ! number of students assigned
    integer, intent(out) :: s(:,:)     ! result of assignment
    integer, intent(in)  :: nslot(:)   ! number of available slots
    integer, intent(in)  :: nstd       ! number of students
    integer, intent(in)  :: ndept      ! number of departments

    integer :: i, x, y, z, nremain
    integer :: indices(nstd), p(nstd,ndept)
    integer :: itemp(nstd)

    ! initialization
    p = pref
    r = 0
    s = 0

    nremain = nstd
    do i = 1, nstd
       indices(i) = nstd-i+1
    end do

    !
    ! continue while a student having preferred departments
    !
    do while( nremain > 0 )
       ! pick a single student
       if( indices(1) > 0 ) then
          x = indices(1)
       else
          ! something strange happens
          write(0,*) 'Error: this should not happen !'
          exit
       end if

       ! pick the best department y for x
       y = p(x,1)
       if( y <= 0 ) then
          ! no such department was found (remove x)
          indices(1) = -1
          indices = cshift(indices, 1)
          nremain = nremain - 1
          ! put x into the list of failed students
          nsd(ndept+1) = nsd(ndept+1) + 1
          s(nsd(ndept+1),ndept+1) = x
          cycle
       else
          ! found (remove y from the list of preference of x)
          p(x,1) = -1
          p(x,:) = cshift(p(x,:), 1)
       end if

       !
       ! assignment
       !
       if( r(y) < nslot(y) ) then
          ! slot of y is available for x
          r(y) = r(y) + 1
          s(r(y),y) = x
          ! remove x from the list
          indices(1) = -1
          indices = cshift(indices, 1)
          nremain = nremain - 1
       else
          ! slot is full, pick the worst student z in the slot
          z = s(r(y),y)

          if( rank(z,y) < rank(x,y) ) then
             ! x is rejected
             s(r(y),y) = z
          else
             ! remove x from the list and add z instead
             s(r(y),y) = x
             indices(1) = z
          end if
       end if

       ! always keep table sorted by ranking
       call qargsort(itemp(1:r(y)), rank(s(1:r(y),y),y))
       s(1:r(y),y) = s(itemp(1:r(y)),y)
    end do

  end subroutine match

  !
  ! check results of Gale-Shapley matching
  !
  subroutine check(rank, pref, r, s, nstd, ndept)
    implicit none
    integer, intent(in) :: rank(:,:)  ! students' ranking by department
    integer, intent(in) :: pref(:,:)  ! students' list of preference
    integer, intent(in) :: r(:)       ! number of students assigned
    integer, intent(in) :: s(:,:)     ! result of assignment
    integer, intent(in) :: nstd       ! number of students
    integer, intent(in) :: ndept      ! number of departments

    integer :: i, j, k, x, y
    integer :: dept(nstd), error(nstd), status

    ! students' ID to department mapping
    do i = 1, ndept+1
       do j = 1, r(i)
          dept(s(j,i)) = i
       end do
    end do

    !
    ! brute-force check
    !
    status = 0
    error  = 0
    do i = 1, nstd
       x = dept(i)

       ! check for each department in the list of preference
       do j = 1, ndept
          y = pref(i,j)
          if(x == y .or. y <= 0) then
             ! this is assigned deparment or no more preference available
             exit
          else
             ! deparment rejected the student, check if it was appropriate
             do k = 1, r(y)
                ! accepted students should have higher ranking
                if( rank(s(k,y),y) > rank(i,y) ) then
                   status   = 1
                   error(i) = 1
                end if
             end do
          end if
       end do
    end do

    ! show error message
    do i = 1, nstd
       if( error(i) /= 0 ) then
          write(0, fmt='("Error for Stdt[", i4, "] assigned to Dept[", i4, "]")') i, dept(i)
       end if
    end do

  end subroutine check

  !
  ! calculate student ranking by department
  !
  subroutine ranking(score, weight, srank)
    implicit none
    real(8), intent(in)  :: score(:,:)
    real(8), intent(in)  :: weight(:,:)
    integer, intent(out) :: srank(:,:)

    integer :: i, j, ndept, nsubj, nstd
    integer :: p(size(score, 1))
    integer :: q(size(score, 1))
    real(8) :: s(size(score, 1))

    ndept = size(weight, 1)
    nsubj = size(weight, 2)
    nstd  = size(score, 1)

    do i = 1, ndept
       ! calcualte student ranking by sorting weighted score
       do j = 1, nstd
          s(j) = sum(weight(i,:) * score(j,:))
       end do
       p = int(-s * 1000) ! cast into integer
       call qargsort(q, p)

       ! student ID to ranking array
       do j = 1, nstd
          srank(q(j),i) = j
       end do
    end do

  end subroutine ranking

  !
  ! swap two arguments
  !
  subroutine swap(a, b)
    implicit none
    integer, intent(inout) :: a, b

    integer :: c

    c = a
    a = b
    b = c

  end subroutine swap

  !
  ! swap in quick sort
  !
  recursive subroutine qargswap(x, y, pivot, left, right)
    implicit none
    integer, intent(inout) :: x(:)
    integer, intent(in)    :: y(:)
    integer, intent(in)    :: pivot, left, right

    integer :: p, l, r
    real(8) :: xp

    xp = y(x(pivot))

    l = left
    r = right
    do
       do while(y(x(l)) < xp)
          l = l + 1
       end do

       do while(y(x(r)) > xp)
          r = r - 1
       end do

       if( l >= r ) then
          exit
       end if

       call swap(x(l), x(r))
       l = l + 1
       r = r - 1
    end do

    if( left < l-1 ) then
       p = left
       call qargswap(x, y, p, left, l-1)
    end if

    if( r+1 < right ) then
       p = r+1
       call qargswap(x, y, p, r+1, right)
    end if

  end subroutine qargswap

  !
  ! qucik sort of x by orders of y
  !
  subroutine qargsort(x, y)
    implicit none
    integer, intent(out) :: x(:)
    integer, intent(in)  :: y(:)

    integer :: i

    do i = 1, size(y)
       x(i) = i
    end do

    call qargswap(x, y, 1, 1, size(x))

  end subroutine qargsort

  !
  ! bubble sort
  !
  subroutine bsort(x)
    implicit none
    integer, intent(inout) :: x(:)

    integer :: i, j, n

    n = size(x)
    do i = 1, n
       do j = 1, n-i
          if( x(j) > x(j+1) ) then
             call swap(x(j), x(j+1))
          end if
       end do
    end do

  end subroutine bsort

end program kadai8_2
