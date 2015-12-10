module class_MyMatrix
  !This module provides an interface
  !to a symmetric matrix.  I've chosen
  !ease of coding over the concerns of
  !memory.
  implicit none
  private :: setMyMatrixDimensions
  public ::  getMyMatrixDimensions
  public ::  setMyMatrixValue
  public ::  getMyMatrixValue
  public ::  initMyMatrix

  type MyMatrix
    integer :: dimensions
    double precision, allocatable :: value(:,:)
  end type MyMatrix

contains

  subroutine setMyMatrixDimensions(matrix,dimensions)
    type(MyMatrix), intent(inout) :: matrix
    integer, intent(in) :: dimensions
    matrix%dimensions = dimensions
  end subroutine setMyMatrixDimensions

  function getMyMatrixDimensions(matrix) result(dimensions)
    type(MyMatrix), intent(inout) :: matrix
    integer :: dimensions
    dimensions = matrix%dimensions
  end function getMyMatrixDimensions

  subroutine setMyMatrixValue(matrix,i,j,value)
    type(MyMatrix), intent(inout) :: matrix
    integer, intent(in) :: i
    integer, intent(in) :: j
    double precision, intent(in) :: value
    matrix%value(i,j) = value
  end subroutine setMyMatrixValue

  function getMyMatrixValue(matrix,i,j) result(value)
    type(MyMatrix), intent(inout) :: matrix
    integer, intent(in) :: i
    integer, intent(in) :: j
    double precision :: value
    value = matrix%value(i,j)
  end function getMyMatrixValue

  subroutine initMyMatrix(matrix,dimensions)
    !Set all values in the symmetric matrix to 0.
    type(MyMatrix), intent(inout) :: matrix
    integer, intent(in) :: dimensions
    integer :: i
    integer :: j
    call setMyMatrixDimensions(matrix,dimensions)
    allocate(matrix%value(dimensions,dimensions))
    do i = 1, dimensions, 1
      do j = 1, dimensions, 1
        call setMyMatrixValue(matrix,i,j,0.0D+00)
      end do
    end do
  end subroutine initMyMatrix

end module class_MyMatrix
