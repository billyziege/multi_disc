module class_DistributionFields
  !This module provides an interface
  !to the fields being generated
  !from different distributions on the
  !discs in a disc_pulse.  
  !This wraps a bunch of other 
  !modules, and provides the equations
  !derived from each of the distributions.
  use class_ChargedDisc
  use class_LinkedChargedDisc
  use class_DiscPulse
  use class_DistributionParameters
  use class_MyMatrix
  implicit none
  private :: calcFieldMyMatrix 
  public :: calcExtractionField
  public :: calcInFrontField
  public :: calcPositionDependentField

contains

  subroutine calcExtractionField(this, extraction_field, only_disc_i) 
    !Updates existing disc(s) with the extraction field value.  If 
    !only_disc_i is provided, then only the disc with index only_disc_i
    !is updated.
    type(DiscPulse), intent(inout) :: this
    double precision, intent(in) :: extraction_field
    integer, optional, intent(in) :: only_disc_i
    integer :: i
    type(LinkedChargedDisc), pointer :: linked_charged_disc

    if( .not. present(only_disc_i) ) then
      do i = 1, this%number_of_discs , 1
        linked_charged_disc => getLinkedChargedDisc(this%discs, i)
        call setChargedDiscExtractionField(linkedChargedDisc%disc,extraction_field)
      end do
    else
      linked_charged_disc => getLinkedChargedDisc(this%discs, only_disc_i)
      call setChargedDiscExtractionField(linkedChargedDisc%disc,extraction_field)
    end if
  end subroutine calcExtractionField

  subroutine calcInFrontField(disc_pulse,disc_i,distribution_parameters)
    !Calculates and stores within the charged disc object the "parallel plate" 
    !field due to discs in front of disc i and the cathode.
    type(DiscPulse), intent(inout) :: disc_pulse
    integer, intent(in) :: disc_i
    type(DistributionParameters), intent(in) :: distribution_parameters
    type(LinkedChargedDisc), pointer :: linked_charged_disc
    real :: q_in_front = 0
    double precision :: in_front_field
    integer :: j

    do j = 1, disc_i-1, 1 !These are the discs in front of disc i.
      linked_charged_disc => getLinkedChargedDisc(disc_pulse%discs, j)
      q_in_front = q_in_front + getChargedDiscCharge(linked_charged_disc%disc)
    end do
    linked_charged_disc => getLinkedChargedDisc(disc_pulse%discs, disc_i)
    q_in_front = 2*q_in_front + getChargedDiscCharge(linked_charged_disc%disc)
    in_front_field = getDistributionParametersCoefficient(distribution_parameters)*q_in_front
    call setChargedDiscInFrontField(linked_charged_disc%disc,in_front_field)
  end subroutine calcInFrontField

  subroutine calcFieldMyMatrix(disc_pulse,distribution_parameters,field_matrix)
    type(DiscPulse), intent(in) :: disc_pulse
    type(DistributionParameters), intent(in) :: distribution_parameters
    type(MyMatrix), intent(inout) :: field_matrix
    integer :: i
    integer :: j
    type(LinkedChargedDisc), pointer :: linked_charged_disc_i
    type(LinkedChargedDisc), pointer :: linked_charged_disc_j
    double precision :: value
    do i = 1, getMyMatrixDimensions(field_matrix), 1
      linked_charged_disc_i => getLinkedChargedDisc(disc_pulse%discs, i)
      do j = i, getMyMatrixDimensions(field_matrix), 1
        linked_charged_disc_j => getLinkedChargedDisc(disc_pulse%discs, j)
        value = getDistributionsFieldComponent(getChargedDiscPosition(linked_charged_disc_i%disc), &
                 getChargedDiscPosition(linked_charged_disc_j%disc), &
                 distribution_parameters)
        call setMyMatrixValue(field_matrix,i,j,value)!Sets both i,j and j,i
      end do
    end do
  end subroutine calcFieldMyMatrix

  subroutine calcPositionDependentField(disc_pulse,distribution_parameters)
    !Calculates and stores within the charged disc object the "position dependent" 
    !field due to interaction between all discs.
    type(DiscPulse), intent(inout) :: disc_pulse
    type(DistributionParameters), intent(in) :: distribution_parameters
    type(LinkedChargedDisc), pointer :: linked_charged_disc_i
    type(LinkedChargedDisc), pointer :: linked_charged_disc_j
    type(MyMatrix), pointer :: field_matrix
    integer :: i
    integer :: j
    double precision :: field
 
    call initMyMatrix(field_matrix,disc_pulse%number_of_discs)
    call calcFieldMyMatrix(disc_pulse,distribution_parameters,field_matrix)
    do i = 1, getDiscPulseNumberOfDiscs(disc_pulse), 1
      linked_charged_disc_i => getLinkedChargedDisc(disc_pulse%discs, i)
      field = 0
      do j = 1, getDiscPulseNumberOfDiscs(disc_pulse), 1
        linked_charged_disc_j => getLinkedChargedDisc(disc_pulse%discs, j)
        field = field + getMyMatrixValue(field_matrix,i,j) * &
                 getChargedDiscCharge(linked_charged_disc_j%disc)
      end do
      setChargedDiscPositionDependentField(linked_charged_disc_i%disc,field)
    end do
  end subroutine calcPositionDependentField

end module class_DistributionFields
