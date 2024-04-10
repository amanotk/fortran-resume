program gaussian_primes
    implicit none
    integer :: n, i, j, x, y, p, q
    logical, allocatable :: is_prime(:, :)
    character(len=256) :: arg

    if (command_argument_count() < 1) then
        write (*, *) "Usage: gaussian_primes <n>"
        stop
    end if

    call get_command_argument(1, arg)
    read (arg, *, iostat=i) n

    allocate (is_prime(0:n, 0:n))

    is_prime = .true.
    is_prime(0, 0) = .false.
    is_prime(1, 0) = .false.
    is_prime(0, 1) = .false.

    do i = 0, n
        do j = 0, n
            if (i == 0 .and. j == 0) cycle
            if (is_prime(i, j)) then
                p = i
                q = j
                do while (p <= n .and. q <= n)
                    x = p
                    y = q
                    do while (x >= 0 .and. y <= n)
                        is_prime(x, y) = .false.
                        is_prime(y, x) = .false.
                        x = x - j
                        y = y + i
                    end do
                    x = p
                    y = q
                    do while (x <= n .and. y >= 0)
                        is_prime(x, y) = .false.
                        is_prime(y, x) = .false.
                        x = x + j
                        y = y - i
                    end do
                    p = p + i
                    q = q + j
                end do
                is_prime(i, j) = .true.
                is_prime(j, i) = .true.
            end if
        end do
    end do

    do i = -n, n
        do j = -n, n
            if (is_prime(abs(i), abs(j))) then
                write (*, fmt='(i2)', advance='no') 1
            else
                write (*, fmt='(i2)', advance='no') 0
            end if
        end do
        write (*, *) ""
    end do

end program gaussian_primes
