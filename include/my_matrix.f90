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

  subroutine setMyMatrixDimensions(this,dimensions)
    type(MyMatrix), intent(inout) :: this
    integer, intent(in) :: dimensions
    this%dimensions = dimensions
  end subroutine setMyMatrixDimensions

  function getMyMatrixDimensions(this) result(dimensions)
    type(MyMatrix), intent(in) :: this
    integer :: dimensions
    dimensions = this%dimensions
  end function getMyMatrixDimensions

  subroutine setMyMatrixValue(this,i,j,value)
    type(MyMatrix), intent(inout) :: this
    integer, intent(in) :: i
    integer, intent(in) :: j
    double precision, intent(in) :: value
    this%value(i,j) = value
  end subroutine setMyMatrixValue

  function getMyMatrixValue(this,i,j) result(value)
    type(MyMatrix), intent(in) :: this
    integer, intent(in) :: i
    integer, intent(in) :: j
    double precision :: value
    value = this%value(i,j)
  end function getMyMatrixValue

  subroutine initMyMatrix(this,dimensions)
    !Set all values in the symmetric matrix to 0.
    type(MyMatrix), intent(inout) :: this
    integer, intent(in) :: dimensions
    integer :: i
    integer :: j
    call setMyMatrixDimensions(this,dimensions)
    allocate(this%value(dimensions,dimensions))
    do i = 1, dimensions, 1
      do j = 1, dimensions, 1
        call setMyMatrixValue(this,i,j,0.0D+00)
      end do
    end do
  end subroutine initMyMatrix
 
  subroutine freeMyMatrix(this)
    type(MyMatrix), intent(inout) :: this
    deallocate(this%value)
  end subroutine freeMyMatrix

end module class_MyMatrix
