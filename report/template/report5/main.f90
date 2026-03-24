program test_dimensions
  use dimensions
  use iso_fortran_env, only: dp => real64
  implicit none
  integer :: n_pass = 0, n_fail = 0

  call test_basic_units()
  call test_compound_units()
  call test_addition()
  call test_subtraction()
  call test_multiplication()
  call test_division()
  call test_scalar_multiply()
  call test_scalar_divide()
  call test_dimensionless()
  call test_dimension_mismatch()
  call test_prefix_units()

  print *, ""
  print *, "=============================="
  print '(A, I0, A, I0)', " Results: ", n_pass, " passed, ", n_fail, " failed"
  print *, "=============================="

  if (n_fail > 0) stop 1

contains

  subroutine check(condition, test_name)
    logical, intent(in) :: condition
    character(*), intent(in) :: test_name
    if (condition) then
      print '(A, A)', "PASS: ", test_name
      n_pass = n_pass + 1
    else
      print '(A, A)', "FAIL: ", test_name
      n_fail = n_fail + 1
    end if
  end subroutine check

  subroutine test_basic_units()
    type(quantity_t) :: q

    print *, ""
    print *, "--- Basic units ---"

    q = quantity(10.0_dp, "m")
    call check(q%value == 10.0_dp, "m: value")
    call check(all(q%dims == [1, 0, 0]), "m: dims")

    q = quantity(5.0_dp, "kg")
    call check(q%value == 5.0_dp, "kg: value")
    call check(all(q%dims == [0, 1, 0]), "kg: dims")

    q = quantity(3.0_dp, "s")
    call check(q%value == 3.0_dp, "s: value")
    call check(all(q%dims == [0, 0, 1]), "s: dims")
  end subroutine

  subroutine test_compound_units()
    type(quantity_t) :: q

    print *, ""
    print *, "--- Compound units ---"

    q = quantity(5.0_dp, "m/s")
    call check(all(q%dims == [1, 0, -1]), "m/s: velocity dims")

    q = quantity(2.0_dp, "m^2")
    call check(all(q%dims == [2, 0, 0]), "m^2: area dims")

    q = quantity(9.8_dp, "m/s^2")
    call check(all(q%dims == [1, 0, -2]), "m/s^2: acceleration dims")

    q = quantity(10.0_dp, "kg*m/s^2")
    call check(all(q%dims == [1, 1, -2]), "kg*m/s^2: force dims")
  end subroutine

  subroutine test_addition()
    type(quantity_t) :: a, b, c

    print *, ""
    print *, "--- Addition ---"

    a = quantity(2.0_dp, "m")
    b = quantity(3.0_dp, "m")
    c = a + b
    call check(c%value == 5.0_dp, "2 m + 3 m = 5 m")
    call check(all(c%dims == [1, 0, 0]), "addition: dims preserved")

    a = quantity(1.5_dp, "kg")
    b = quantity(2.5_dp, "kg")
    c = a + b
    call check(c%value == 4.0_dp, "1.5 kg + 2.5 kg = 4 kg")
  end subroutine

  subroutine test_subtraction()
    type(quantity_t) :: a, b, c

    print *, ""
    print *, "--- Subtraction ---"

    a = quantity(10.0_dp, "s")
    b = quantity(4.0_dp, "s")
    c = a - b
    call check(c%value == 6.0_dp, "10 s - 4 s = 6 s")
    call check(all(c%dims == [0, 0, 1]), "subtraction: dims preserved")
  end subroutine

  subroutine test_multiplication()
    type(quantity_t) :: a, b, c

    print *, ""
    print *, "--- Multiplication ---"

    a = quantity(2.0_dp, "m")
    b = quantity(3.0_dp, "m")
    c = a * b
    call check(c%value == 6.0_dp, "2 m * 3 m = 6 m^2")
    call check(all(c%dims == [2, 0, 0]), "multiplication: dims add")

    a = quantity(10.0_dp, "m")
    b = quantity(5.0_dp, "kg")
    c = a * b
    call check(c%value == 50.0_dp, "10 m * 5 kg = 50 m*kg")
    call check(all(c%dims == [1, 1, 0]), "m * kg: dims")
  end subroutine

  subroutine test_division()
    type(quantity_t) :: a, b, c

    print *, ""
    print *, "--- Division ---"

    a = quantity(10.0_dp, "m")
    b = quantity(2.0_dp, "s")
    c = a / b
    call check(c%value == 5.0_dp, "10 m / 2 s = 5 m/s")
    call check(all(c%dims == [1, 0, -1]), "division: dims subtract")

    a = quantity(6.0_dp, "m^2")
    b = quantity(2.0_dp, "m")
    c = a / b
    call check(c%value == 3.0_dp, "6 m^2 / 2 m = 3 m")
    call check(all(c%dims == [1, 0, 0]), "m^2 / m: dims")
  end subroutine

  subroutine test_scalar_multiply()
    type(quantity_t) :: a, b

    print *, ""
    print *, "--- Scalar multiplication ---"

    a = quantity(5.0_dp, "m")
    b = 2.0_dp * a
    call check(b%value == 10.0_dp, "2.0 * 5 m = 10 m")
    call check(all(b%dims == [1, 0, 0]), "scalar * quantity: dims unchanged")

    a = quantity(3.0_dp, "kg")
    b = a * 4.0_dp
    call check(b%value == 12.0_dp, "3 kg * 4.0 = 12 kg")
    call check(all(b%dims == [0, 1, 0]), "quantity * scalar: dims unchanged")
  end subroutine

  subroutine test_scalar_divide()
    type(quantity_t) :: a, b

    print *, ""
    print *, "--- Scalar division ---"

    a = quantity(10.0_dp, "s")
    b = a / 2.0_dp
    call check(b%value == 5.0_dp, "10 s / 2.0 = 5 s")
    call check(all(b%dims == [0, 0, 1]), "quantity / scalar: dims unchanged")
  end subroutine

  subroutine test_dimensionless()
    type(quantity_t) :: q

    print *, ""
    print *, "--- Dimensionless ---"

    q = quantity(3.14_dp, "")
    call check(q%value == 3.14_dp, "empty string: value")
    call check(all(q%dims == [0, 0, 0]), "empty string: dimensionless")

    q = quantity(2.0_dp, "1")
    call check(q%value == 2.0_dp, "'1': value")
    call check(all(q%dims == [0, 0, 0]), "'1': dimensionless")
  end subroutine

  subroutine test_dimension_mismatch()
    type(quantity_t) :: a, b, c

    print *, ""
    print *, "--- Dimension mismatch ---"

    a = quantity(1.0_dp, "m")
    b = quantity(1.0_dp, "kg")
    c = a + b
    call check(is_nan(c), "1 m + 1 kg = NaN")

    a = quantity(5.0_dp, "s")
    b = quantity(2.0_dp, "m")
    c = a - b
    call check(is_nan(c), "5 s - 2 m = NaN")
  end subroutine

  subroutine test_prefix_units()
    type(quantity_t) :: a, b, c

    print *, ""
    print *, "--- Prefix units ---"

    a = quantity(1000.0_dp, "mm")
    b = quantity(1.0_dp, "m")
    c = a + b
    call check(c%value == 2.0_dp, "1000 mm + 1 m = 2 m")
    call check(all(c%dims == [1, 0, 0]), "prefix addition: dims")

    a = quantity(2.0_dp, "km")
    b = quantity(500.0_dp, "m")
    c = a + b
    call check(c%value == 2.5_dp, "2 km + 500 m = 2.5 km")

    a = quantity(500.0_dp, "g")
    b = quantity(1.0_dp, "kg")
    c = a + b
    call check(c%value == 1.5_dp, "500 g + 1 kg = 1.5 kg")
    call check(all(c%dims == [0, 1, 0]), "prefix mass: dims")

    a = quantity(1000.0_dp, "ms")
    b = quantity(1.0_dp, "s")
    c = a + b
    call check(c%value == 2.0_dp, "1000 ms + 1 s = 2 s")

    a = quantity(1.0_dp, "km/ms")
    call check(all(a%dims == [1, 0, -1]), "km/ms: velocity dims")
    call check(a%value == 1.0e6_dp, "1 km/ms = 1.0e6 m/s")

    a = quantity(3.0_dp, "um")
    b = quantity(2.0_dp, "mm")
    c = a * b
    call check(all(c%dims == [2, 0, 0]), "prefix multiplication: dims")
  end subroutine

end program test_dimensions
