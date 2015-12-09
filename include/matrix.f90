module class_Matrix
  !This module provides an interface
  !to a symmetric matrix.  I've chosen
  !ease of coding over the concerns of
  !memory.
  implicit none
  private :: setMatrixDimensions
  public :: getMatrixDimensions
  public ::  setMatrixValue
  public ::  getMatrixValue
  public ::  initMatrix

  type Matrix
    integer :: dimensions
    double precision, allocatable :: value(:,:)
  end type Matrix

contains

  subroutine setMatrixDimensions(matrix,dimensions)
    type(Matrix), intent(inout) :: matrix
    integer, intent(in) :: dimensions
    matrix%dimensions = dimensions
  end subroutine setMatrixDimensions

  function getMatrixDimensions(matrix) result(dimensions)
    type(Matrix), intent(inout) :: matrix
    integer :: dimensions
    dimensions = matrix%dimensions
  end function getMatrixDimensions

  subroutine setMatrixValue(matrix,i,j,value)
    type(Matrix), intent(inout) :: matrix
    integer, intent(in) :: i
    integer, intent(in) :: j
    double precision, intent(in) :: value
    matrix%value(i,j) = value
  end subroutine setMatrixValue

  function getMatrixValue(matrix,i,j) result(value)
    type(Matrix), intent(inout) :: matrix
    integer, intent(in) :: i
    integer, intent(in) :: j
    double precision :: value
    value = matrix%value(i,j)
  end function getMatrixValue

  subroutine initMatrix(matrix,dimensions)
    !Set all values in the symmetric matrix to 0.
    type(Matrix), intent(inout) :: matrix
    integer, intent(in) :: dimensions
    integer :: i
    integer :: j
    call setMatrixDimensions(matrix,dimensions)
    allocate(matrix%values(dimensions,dimensions))
    do i = 1, dimensions, 1
      do j = 1, dimensions, 1
        setMatrixValue(matrix,i,j,0.0)
      end do
    end do
  end subroutine initMatrix

end module class_Matrix
