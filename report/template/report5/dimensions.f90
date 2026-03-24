module dimensions
  use iso_fortran_env, only: dp => real64
  use ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_is_nan
  implicit none
  private

  public :: quantity_t, quantity
  public :: operator(+), operator(-), operator(*), operator(/)
  public :: is_nan

  integer, parameter :: NUM_DIMS = 3

  type :: quantity_t
    real(dp) :: value = 0.0_dp
    integer :: dims(NUM_DIMS) = 0
  contains
    procedure :: init => quantity_init
  end type quantity_t

  interface quantity
    module procedure :: quantity_from_value_unit
  end interface quantity

  interface operator(+)
    module procedure :: quantity_add
  end interface operator(+)

  interface operator(-)
    module procedure :: quantity_subtract
  end interface operator(-)

  interface operator(*)
    module procedure :: quantity_multiply
    module procedure :: quantity_multiply_scalar_left
    module procedure :: quantity_multiply_scalar_right
  end interface operator(*)

  interface operator(/)
    module procedure :: quantity_divide
    module procedure :: quantity_divide_scalar
  end interface operator(/)

contains

  function is_nan(q) result(res)
    type(quantity_t), intent(in) :: q
    logical :: res
    res = ieee_is_nan(q%value)
  end function is_nan

  function nan_quantity() result(res)
    type(quantity_t) :: res
    res%value = ieee_value(0.0_dp, ieee_quiet_nan)
    res%dims = 0
  end function nan_quantity

  subroutine quantity_init(this, val, unit_str)
    class(quantity_t), intent(out) :: this
    real(dp), intent(in) :: val
    character(*), intent(in) :: unit_str

    this%value = val
    this%dims = 0

    ! TODO: unit_str をパースして dims を設定
    ! ヒント: index(), scan(), adjustl(), trim() が有用
    ! 
    ! サポートする形式:
    !   "m"        -> [1, 0, 0]
    !   "kg"       -> [0, 1, 0]
    !   "s"        -> [0, 0, 1]
    !   "m/s"      -> [1, 0, -1]
    !   "m^2"      -> [2, 0, 0]
    !   "kg*m/s^2" -> [1, 1, -2]
    !   "" or "1"  -> [0, 0, 0]

  end subroutine quantity_init

  function quantity_from_value_unit(val, unit_str) result(res)
    real(dp), intent(in) :: val
    character(*), intent(in) :: unit_str
    type(quantity_t) :: res

    call res%init(val, unit_str)
  end function quantity_from_value_unit

  function quantity_add(a, b) result(res)
    type(quantity_t), intent(in) :: a, b
    type(quantity_t) :: res

    ! TODO: 次元が同じかチェック
    ! 異なる場合は NaN を返す
    ! 同じなら値を足す

    res%value = a%value + b%value
    res%dims = a%dims

  end function quantity_add

  function quantity_subtract(a, b) result(res)
    type(quantity_t), intent(in) :: a, b
    type(quantity_t) :: res

    ! TODO: 次元が同じかチェック
    ! 異なる場合は NaN を返す
    ! 同じなら値を引く

    res%value = a%value - b%value
    res%dims = a%dims

  end function quantity_subtract

  function quantity_multiply(a, b) result(res)
    type(quantity_t), intent(in) :: a, b
    type(quantity_t) :: res

    ! TODO: 値を掛ける
    ! TODO: 次元を足す

    res%value = a%value * b%value
    res%dims = a%dims + b%dims

  end function quantity_multiply

  function quantity_multiply_scalar_left(scalar, q) result(res)
    real(dp), intent(in) :: scalar
    type(quantity_t), intent(in) :: q
    type(quantity_t) :: res

    ! TODO: スカラー倍（次元は変わらない）

    res%value = scalar * q%value
    res%dims = q%dims

  end function quantity_multiply_scalar_left

  function quantity_multiply_scalar_right(q, scalar) result(res)
    type(quantity_t), intent(in) :: q
    real(dp), intent(in) :: scalar
    type(quantity_t) :: res

    ! TODO: スカラー倍（次元は変わらない）

    res%value = q%value * scalar
    res%dims = q%dims

  end function quantity_multiply_scalar_right

  function quantity_divide(a, b) result(res)
    type(quantity_t), intent(in) :: a, b
    type(quantity_t) :: res

    ! TODO: 値を割る
    ! TODO: 次元を引く

    res%value = a%value / b%value
    res%dims = a%dims - b%dims

  end function quantity_divide

  function quantity_divide_scalar(q, scalar) result(res)
    type(quantity_t), intent(in) :: q
    real(dp), intent(in) :: scalar
    type(quantity_t) :: res

    ! TODO: スカラーで割る（次元は変わらない）

    res%value = q%value / scalar
    res%dims = q%dims

  end function quantity_divide_scalar

end module dimensions
