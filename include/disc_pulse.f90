module class_DiscPulse
  !The disc pulse class represents
  !an electron pulse as a series of 
  !discs.
  use class_LinkedChargedDisc
  implicit none
  public :: DiscPulse
  public :: initDiscPulse
  public :: addChargedDisc
  public :: subtractChargedDisc
  public :: reportDiscPulse
  public :: freeDiscPulse
  public :: selfConsistent
  public :: removeDiscsAbsorbedByCathode
  public :: copyDiscPulse

  type DiscPulse
    !Defines the object
    integer :: number_of_discs
    real :: number_of_electrons
    type(LinkedChargedDisc), pointer :: discs
  end type DiscPulse

contains

  subroutine initDiscPulse(this,number_of_discs)
    !Initializes the disc pulse with the original pointer 
    !charged disc pointer to an initialized disc.
    use class_ChargedDisc
    type(DiscPulse), intent(inout) :: this
    integer, intent(in) :: number_of_discs
    type(ChargedDisc) :: charged_disc
    integer :: i

    call initChargedDisc(charged_disc,0.0D+00) !Placeholder

    this%number_of_discs = 0
    this%number_of_electrons = 0.0
    allocate(this%discs)
    call initLinkedChargedDisc(this%discs)
    do i = 1, number_of_discs , 1
      call addChargedDisc(this,charged_disc) !All copy the placeholder disc
    end do
  end subroutine initDiscPulse

  subroutine addChargedDisc(this, charged_disc)
    !Inserts the disc and adds its electrons to the ongoing total.
    use stringFunctions
    type(DiscPulse), intent(inout) :: this
    type(ChargedDisc), intent(in), target :: charged_disc

    this%number_of_discs = this%number_of_discs + 1
    this%number_of_electrons = this%number_of_electrons + getChargedDiscNumberOfElectrons(charged_disc)
    call insertChargedDisc(this%discs, charged_disc, "append")
  end subroutine addChargedDisc

  subroutine subtractChargedDisc(this,linked_charged_disc)
    !Removes the disc and subtractss its electrons to the ongoing total.
    type(DiscPulse), intent(inout) :: this
    type(LinkedChargedDisc), pointer :: linked_charged_disc

    this%number_of_discs = this%number_of_discs - 1
    this%number_of_electrons = this%number_of_electrons - getChargedDiscNumberOfElectrons(linked_charged_disc%disc)
    call removeLinkedDisc(linked_charged_disc)
  end subroutine subtractChargedDisc

  subroutine reportDiscPulse(this,only_disc_i)
    type(DiscPulse), intent(inout) :: this
    integer, optional, intent(in) :: only_disc_i
    integer :: i
    if( .not. present(only_disc_i) ) then
      print *, 'number_of_discs = ', this%number_of_discs
      print *, 'number_of_electrons = ', this%number_of_electrons
      do i = 1, this%number_of_discs , 1
        call reportLinkedChargedDisc(this%discs,i)
      end do
    else
      call reportLinkedChargedDisc(this%discs,only_disc_i)
    end if
  end subroutine reportDiscPulse

  subroutine freeDiscPulse(this)
    type(DiscPulse), intent(inout) :: this
    call freeAllLinkedDiscs(this%discs)
    nullify(this%discs)
  end subroutine freeDiscPulse

  function selfConsistent(this) result(is_consistent)
    !Checks that the order of the discs is retained by comparing their
    !positions.  If a later disc has a larger position than a previous disc,
    !returns .FALSE.
    type(DiscPulse), intent(in) :: this
    logical :: is_consistent
    integer :: i
    type(LinkedChargedDisc), pointer :: linked_charged_disc
    real :: prev_position
    real :: current_position

    is_consistent = .TRUE.
    linked_charged_disc => getLinkedChargedDisc(this%discs, 1)
    prev_position = getChargedDiscPosition(linked_charged_disc%disc)
    do i = 2, this%number_of_discs , 1
      linked_charged_disc => getLinkedChargedDisc(this%discs, i)
      current_position = getChargedDiscPosition(linked_charged_disc%disc)
      if (current_position > prev_position) then
        is_consistent = .FALSE.
        exit
      end if
      prev_position = current_position
    end do
  end function selfConsistent

  subroutine removeDiscsAbsorbedByCathode(this)
    !If a disc's position is below 0, then it is absorbed by the cathode.
    !Since the discs should be ordered (see selfConsistent above), then
    !we can simply proceed backwards through the discs (removing them) until
    !we get to a disc with position above zero. 
    type(DiscPulse), intent(inout) :: this
    type(LinkedChargedDisc), pointer :: current_link
    type(LinkedChargedDisc), pointer :: prev_link
    
    current_link => getLinkedChargedDisc(this%discs, this%number_of_discs)
    prev_link => current_link%prev_link

    do while (current_link%disc%z <= 0)
      call subtractChargedDisc(this,current_link)
      current_link => prev_link
      prev_link => current_link%prev_link
    end do
  end subroutine removeDiscsAbsorbedByCathode

  function copyDiscPulse(this) result (disc_pulse_out)
    !Copies the hard data within the this to disc_pulse_out.
    !Since the set of charged discs needs to be in a different memory
    !location, an array of charged discs must also be provided (just like
    !the subroutine initDiscPulse).
    type(DiscPulse), intent(in) :: this
    type(DiscPulse) :: disc_pulse_out
    integer :: i
    type(LinkedChargedDisc), pointer :: linked_charged_disc

    call initDiscPulse(disc_pulse_out,0)
    do i = 1, this%number_of_discs , 1
      linked_charged_disc => getLinkedChargedDisc(this%discs, i)
      call addChargedDisc(disc_pulse_out, linked_charged_disc%disc)
    end do
  end function copyDiscPulse

end module class_DiscPulse
