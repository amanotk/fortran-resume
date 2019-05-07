program kadai8_1
  implicit none

  integer, allocatable :: p(:,:)      ! list of preference for male
  integer, allocatable :: q(:,:)      ! list of preference for female
  integer, allocatable :: marriage(:) ! female to male mapping
  integer :: i, n

  ! read data
  read(*,*) n

  allocate(p(n,n))
  allocate(q(n,n))
  allocate(marriage(n))

  ! initialization
  p = 0
  q = 0
  marriage = 0

  ! read list of preference
  read(*,*) p ! male
  read(*,*) q ! female

  ! take transpose to make everything easier to understand
  p = transpose(p)
  q = transpose(q)

  ! matching via Gale-Shapley algorithm
  call match(p, q, marriage, n)

  ! results
  do i = 1, n
     write(*,fmt='("F[", i3, "] <=> M[", i3, "]")') i, marriage(i)
  end do

  deallocate(p)
  deallocate(q)
  deallocate(marriage)

contains
  !
  ! Gale-Shapley Algorithm for Stable Marriage Problem
  !
  subroutine match(mpref, wpref, marriage, n)
    implicit none
    integer, intent(in)  :: mpref(:,:)    ! list of preference (male to female)
    integer, intent(in)  :: wpref(:,:)    ! list of preference (female to male)
    integer, intent(out) :: marriage(:)   ! marriage mapping (female to male)
    integer, intent(in)  :: n             ! number of male/female

    integer :: i, x, y, z, nsingle
    integer :: indices(n), p(n,n), q(n,n)

    ! copy
    p = mpref
    q = wpref

    ! initialization
    nsingle = n
    do i = 1, n
       indices(i) = i
    end do

    !
    ! continue while a single male exists
    !
    do while( nsingle > 0 )
       ! pick a single male
       if( indices(1) > 0 ) then
          x = indices(1)
       else
          ! something strange happens
          write(0,*) 'Error: this should not happen !'
          exit
       end if

       ! pick the best female y for x
       y = p(x,1)
       if( y < 0 ) then
          ! no such female was found (remove x from the single list)
          indices(1) = -1
          indices = cshift(indices, 1)
          nsingle = nsingle - 1
          continue
       else
          ! found (remove y from the list of preference of x)
          p(x,1) = -1
          p(x,:) = cshift(p(x,:), 1)
       end if
       !
       ! let's propose ;p
       !
       if( marriage(y) == 0 ) then
          ! y is single and (x, y) pair engage
          marriage(y) = x
          ! remove x from the single list
          indices(1) = -1
          indices = cshift(indices, 1)
          nsingle = nsingle - 1
       else
          ! y is engaged with z, so check if y prefers x or z
          z = marriage(y)
          do i = 1, n
             if( q(y,i) == z ) then
                ! x is rejected and (z, y) pair continue to engage
                marriage(y) = z
                exit
             else if( q(y,i) == x ) then
                ! z is rejected and (x, y) pair engage
                marriage(y) = x
                ! remove x from the single list and add z instead
                indices(1) = z
                exit
             end if
          end do
       end if

    end do

  end subroutine match

end program kadai8_1
