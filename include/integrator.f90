module class_Integrator
  !This module provides an interface
  !to control and perform integration
  !of disc velocity and position.
  implicit none
  use class_ChargedDisc
  use class_DiscPulse
  use class_DistributionParameters
  use class_DistributionFields
  private :: setIntegratorParametersIntegratorType
  private :: rungeKutta4DiscPulse
  private :: rungeKuttaStep
  private :: rungeKutta4CombineStep
  public :: IntegratorParameters
  public :: getIntegratorParametersIntegratorType
  public :: initIntegratorParameters
  public :: advanceDiscPulse

  type IntegratorParameters
    character(len=*) :: integrator_type
  end type IntegratorParameters

contains

  subroutine setIntegratorParametersIntegratorType(integrator_parameters, integrator_type)
    type(IntegratorParameters), intent(inout) :: integrator_parameters
    character(len=*), intent(in) :: integrator_type
    integrator_parameters%integrator_type = integrator_type
  end subroutine setIntegratorParametersIntegratorType

  function getIntegratorParametersIntegratorType(integrator_parameters) result(integrator_type)
    type(IntegratorParameters), intent(inout) :: integrator_parameters
    character(len=*):: integrator_type
    integrator_type = integrator_parameters%integrator_type
  end function getIntegratorParametersIntegratorType

  subroutine initIntegratorParameters(integrator_parameters, integrator_type)
    type(IntegratorParameters), intent(inout) :: integrator_parameters
    character(len=*), intent(in) :: integrator_type
    call setIntegratorParametersIntegratorType(integrator_parameters,integrator_type)
  end subroutine initDistributionParameters

  function advanceDiscPulse(integrator_parameters, disc_pulse_in, &
        distribution_parmaters, dt) result(disc_pulse_out)
    !Passes the control to a function specific for the integrator.
    !Currently supports runge kutta 4 only.
    type(IntegratorParameters), intent(inout) :: integrator_parameters
    type(DiscPulse), intent(in) :: disc_pulse_in
    type(DistributionParameters), intent(in) :: distribution_parameters
    double precision, intent(in) :: dt
    type(DiscPulse) :: disc_pulse_out

    call initDiscPulse(disc_pulse_out)
    disc_pulse_out = copyDiscPulse(disc_pulse_in)
    if( getIntegratorParametersIntegratorType(integrator_parameters) == "Runge-Kutta 4" ) then
      call rungaKutta4DiscPulse(disc_pulse_in, disc_pulse_out, distribution_parameters, dt)
    end if
  end function advanceDiscPulse

  subroutine rungeKutta4DiscPulse(disc_pulse_0, disc_pulse_3, distribution_parameters, dt)
    type(DiscPulse), intent(in) :: disc_pulse_0
    type(DiscPulse), intent(inout) :: disc_pulse_3
    type(DistributionParameters), intent(in) :: distribution_parameters
    double precision, intent(in) :: dt
    type(DiscPulse) :: disc_pulse_1
    type(DiscPulse) :: disc_pulse_2

    disc_pulse_1 = copyDiscPulse(disc_pulse_0)
    disc_pulse_2 = copyDiscPulse(disc_pulse_0)

    call rungeKuttatStep(disc_pulse_0,disc_pulse_0,disc_pulse_1,dt/2.0D+00)
    call calcPositionDependentField(disc_pulse_1,distribution_parameters)
    
    call rungeKuttatStep(disc_pulse_0,disc_pulse_1,disc_pulse_2,dt/2.0D+00)
    call calcPositionDependentField(disc_pulse_2,distribution_parameters)

    call rungeKuttatStep(disc_pulse_0,disc_pulse_2,disc_pulse_3,dt)
    call calcPositionDependentField(disc_pulse_2,distribution_parameters)
 
    call rungeKutta4Combine(disc_pulse_0, disc_pulse_1, disc_pulse_2, disc_pulse_3, dt)

    call freeDiscPulse(disc_pulse_1)
    call freeDiscPulse(disc_pulse_2)
  end subroutine rungeKutta4DiscPulse

  subroutine rungeKuttaStep(disc_pulse_in, disc_pulse_in_prime, disc_pulse_out, dt)
    type(DiscPulse), intent(in) :: disc_pulse_in !Supplies initial position and velocity
    type(DiscPulse), intent(in) :: disc_pulse_in_prime !Supplies an estimated change for the position and velocity
    type(DiscPulse), intent(inout) :: disc_pulse_out
    double precision, intent(in) :: dt
    integer :: i
    type(LinkedChargedDisc), pointer :: linked_charged_disc_in
    type(LinkedChargedDisc), pointer :: linked_charged_disc_in_prime
    type(LinkedChargedDisc), pointer :: linked_charged_disc_out
    double precision :: temp_z
    double precision :: temp_v
    double precision :: charge_mass_ratio = 1.7588200227239E11! = 1.60217662E-19 C / 9.10938356E-31 kg 

    do i = 1, disc_pulse_in%number_of_discs , 1
      linked_charged_disc_in => getLinkedChargedDisc(disc_pulse_in%discs, i)
      linked_charged_disc_in_prime => getLinkedChargedDisc(disc_pulse_in_prime%discs, i)
      temp_z = getChargedDiscPosition(linked_charged_disc_in%disc) + dt * &
                 getChargedDiscVelocity(linked_charged_disc_in_prime%disc)
      temp_v = getChargedDiscVelocity(linked_charged_disc_in%disc) + dt * &
                 getChargedDiscFullField(linked_charged_disc_in_prime%disc) * &
                 charge_mass_ratio
      linked_charged_disc_out => getLinkedChargedDisc(disc_pulse_out%discs, i)
      call setChargedDiscPosition(linked_charged_disc_out%disc, temp_z)
      call setChargedDiscVelocity(linked_charged_disc_out%disc, temp_v)
    end do
  end subroutine rungeKuttaStep

  subroutine rungeKutta4Combine(disc_pulse_0, disc_pulse_1, disc_pulse_2, disc_pulse_3, dt)
    type(DiscPulse), intent(in) :: disc_pulse_0 
    type(DiscPulse), intent(in) :: disc_pulse_1 
    type(DiscPulse), intent(in) :: disc_pulse_2 
    type(DiscPulse), intent(inout) :: disc_pulse_3 
    double precision, intent(in) :: dt
    integer :: i
    type(LinkedChargedDisc), pointer :: linked_charged_disc_0
    type(LinkedChargedDisc), pointer :: linked_charged_disc_1
    type(LinkedChargedDisc), pointer :: linked_charged_disc_2
    type(LinkedChargedDisc), pointer :: linked_charged_disc_3
    double precision :: temp_z
    double precision :: temp_v
    double precision :: charge_mass_ratio = 1.7588200227239E11! = 1.60217662E-19 C / 9.10938356E-31 kg 

    do i = 1, disc_pulse_in%number_of_discs , 1
      linked_charged_disc_0 => getLinkedChargedDisc(disc_pulse_0%discs, i)
      linked_charged_disc_1 => getLinkedChargedDisc(disc_pulse_1%discs, i)
      linked_charged_disc_2 => getLinkedChargedDisc(disc_pulse_2%discs, i)
      linked_charged_disc_3 => getLinkedChargedDisc(disc_pulse_3%discs, i)
      temp_z = getChargedDiscPosition(linked_charged_disc_0%disc) + dt * &
                 (getChargedDiscVelocity(linked_charged_disc_0%disc) +
                 2.0D+00 * getChargedDiscVelocity(linked_charged_disc_1%disc) + &
                 2.0D+00 * getChargedDiscVelocity(linked_charged_disc_2%disc) + &
                 getChargedDiscVelocity(linked_charged_disc_3%disc) ) / 6.0D+00
      temp_v = getChargedDiscVelocity(linked_charged_disc_0%disc) + dt * &
                 (getChargedDiscFullField(linked_charged_disc_in_0%disc) + &
                 2.0D+00 * getChargedDiscFullField(linked_charged_disc_in_1%disc) + &
                 2.0D+00 * getChargedDiscFullField(linked_charged_disc_in_2%disc) + &
                 getChargedDiscFullField(linked_charged_disc_in_3%disc) * &
                 charge_mass_ratio / 6.0D+00
      call setChargedDiscPosition(linked_charged_disc_3%disc, temp_z) !Overwrites, but this is the end
      call setChargedDiscVelocity(linked_charged_disc_3%disc, temp_v) !Overwrites, but this is the end
    end do
  end subroutine rungeKutta4CombineStep

end module class_Integrator
