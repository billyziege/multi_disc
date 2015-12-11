module class_InitialDiscPulse
  !This object is the "initial conditions"
  !pulse that is theoretically generated
  !from the physics of the situation.  It is
  !simply a way to store the pulse
  !before it enters the system (and
  !is then passed to the class_DynamicDisc_pulse.
  use class_DiscPulse
  implicit none
  private :: split_normal_distribution
  private :: distributeElectrons
  public :: InitialDiscPulse
  public :: initInitialDiscPulse
  public :: reportInitialDiscPulse 
  public :: freeInitialDiscPulse

  type InitialDiscPulse
    type(DiscPulse) :: disc_pulse
    double precision, allocatable :: insertion_times(:)
    double precision :: total_number_of_electrons !Set independent from the disc pulse (will set the electrons in disc pulse)
  end type InitialDiscPulse

contains

  function split_normal_distribution(N) result(probability)
    !Splits the normal distribution from -3*std
    !to 3*std into N equal pieces with length 6*std/N.  
    !An array of length N is returns with the proability 
    !volume of each piece normalized so that the sum of the
    !array is 1.
    !This function uses cdfnor from cdflib.
    integer, intent(in) :: N
    double precision :: probability(N)
    double precision :: total_probability
    real(kind=8) :: p_lower
    real(kind=8) :: p_upper
    real(kind=8) :: p_diff
    real(kind=8) :: q_lower
    real(kind=8) :: q_upper
    real(kind=8) :: x_lower
    real(kind=8) :: x_upper
    integer(kind=4) :: status
    real(kind=8) :: bound
    integer :: i
    total_probability = 0
    do i = 1, N , 1
      p_lower = huge(p_lower)
      q_lower = huge(q_lower)
      p_upper = huge(p_upper)
      q_upper = huge(q_upper)
      x_lower = -3.0 + 6.0/N * (i-1)
      x_upper = -3.0 + 6.0/N * i
      call cdfnor ( 1, p_lower, q_lower, x_lower, 0.0D+00, 1.0D+00, status, bound )
      call cdfnor ( 1, p_upper, q_upper, x_upper, 0.0D+00, 1.0D+00, status, bound )
      p_diff = p_upper - p_lower !the probability volume of the ith piece
      probability(i) = p_diff
      !print *, p_diff
      total_probability = total_probability + p_diff
    end do
    do i = 1, N , 1 !Normalizes the sum to 1.
      probability(i) = probability(i)/total_probability
    end do
  end function split_normal_distribution

  subroutine distributeElectrons(this)
    !Distributes the electrons between the discs
    !according to a normal distribution from +3
    !std to -3 std.
    use class_ChargedDisc
    use class_LinkedChargedDisc
    type(InitialDiscPulse), intent(inout) :: this
    type(LinkedChargedDisc), pointer :: linked_charged_disc
    double precision :: probability_array(this%disc_pulse%number_of_discs)
    double precision :: number_of_electrons
    integer :: i
    probability_array = split_normal_distribution(this%disc_pulse%number_of_discs)
    do i = 1, this%disc_pulse%number_of_discs , 1
      linked_charged_disc => getLinkedChargedDisc(this%disc_pulse%discs,i)
      number_of_electrons = probability_array(i)*this%total_number_of_electrons
      call setChargedDiscNumberOfElectrons(linked_charged_disc%disc,number_of_electrons)
      this%disc_pulse%number_of_electrons = this%disc_pulse%number_of_electrons + number_of_electrons
    end do
  end subroutine distributeElectrons

  subroutine initInitialDiscPulse(this, number_of_discs, number_of_electrons, &
               distribution_parameters, extraction_field)
    use class_DistributionFields
    type(InitialDiscPulse), intent(inout) :: this
    integer, intent(in) :: number_of_discs
    double precision, intent(in) :: number_of_electrons
    type(DistributionParameters), intent(in) :: distribution_parameters
    double precision, optional, intent(in) :: extraction_field
    double precision, parameter :: insertion_time_duration = 127D-15
    double precision :: time_step_size
    integer :: i

    call initDiscPulse(this%disc_pulse,number_of_discs)
    this%total_number_of_electrons = number_of_electrons
    call distributeElectrons(this)

    time_step_size = insertion_time_duration/(number_of_discs-1.0D+00)
    allocate(this%insertion_times(number_of_discs))
    this%insertion_times(1) = 0.0D+00
    if( present(extraction_field) ) then
      call calcExtractionField(this%disc_pulse, extraction_field, 1)
    end if 
    do i = 2, number_of_discs , 1
      this%insertion_times(i) = this%insertion_times(i-1) + time_step_size
      if( present(extraction_field) ) then
        call calcExtractionField(this%disc_pulse, extraction_field, i)
      end if 
      call calcInFrontField(this%disc_pulse,i,distribution_parameters)
    end do
  end subroutine initInitialDiscPulse
    
  subroutine reportInitialDiscPulse(this)
    !Prints out the insertion time, total number of electrons
    !as well as the contained discs.
    type(InitialDiscPulse), intent(inout) :: this
    integer :: i
    print *, "total number electrons = ", this%total_number_of_electrons
    do i = 1, this%disc_pulse%number_of_discs , 1
      call reportDiscPulse(this%disc_pulse,i)
      print *, "insertion time = ", this%insertion_times(i)
    end do
  end subroutine reportInitialDiscPulse

  subroutine freeInitialDiscPulse(this)
    type(InitialDiscPulse), intent(inout) :: this
    deallocate(this%insertion_times)
    call freeDiscPulse(this%disc_pulse)
  end subroutine freeInitialDiscPulse
end module class_InitialDiscPulse
