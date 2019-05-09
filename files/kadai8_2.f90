program kadai8_2
  implicit none

  integer, allocatable :: quota(:)    ! number of available slots
  integer, allocatable :: spref(:,:)  ! students' list of preference
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

  allocate(score(nstd,nsubj))
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

  !
  ! following is to check if the data has properly been read
  !

  ! department quota
  write(*,*) '*** departments quota ***'
  do i = 1, ndept
     write(*, fmt='("quota[", i2, "] = ", i4)') i, quota(i)
  end do
  write(*,*)

  ! department subjects weight
  write(*,*) '*** departments subject weight ***'
  do i = 1, ndept
     write(*, fmt='("weight[", i2, "] = ")', advance='no') i
     do j = 1, nsubj
        write(*, fmt='(f7.2)', advance='no') weight(i,j)
     end do
     write(*,*)
  end do
  write(*,*)

  ! students score
  write(*,*) '*** students score (for first ten students) ***'
  do i = 1, 10
     write(*, fmt='("score[", i2, "] = ")', advance='no') i
     do j = 1, nsubj
        write(*, fmt='(f7.2)', advance='no') score(i,j)
     end do
     write(*,*)
  end do
  write(*,*)

  ! students preference
  write(*,*) '*** students preference (for first ten students) ***'
  do i = 1, 10
     write(*, fmt='("pref[", i2, "] = ")', advance='no') i
     do j = 1, ndept
        write(*, fmt='(i7)', advance='no') spref(i,j)
     end do
     write(*,*)
  end do

  deallocate(quota)
  deallocate(weight)
  deallocate(score)
  deallocate(spref)
  deallocate(temp)

end program kadai8_2
