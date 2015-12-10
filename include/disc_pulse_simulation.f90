module class_DiscPulseSimulation
  !This module provides functions
  !detailing the simulation.
  implicit none
  use class_DiscPulse
  use class_DistributionParameters
  use class_IntegratorParameters
  private :: advance_time_step
  private :: position_dependent_advance
  public :: SimulationParameters
  public :: initSimulationParameters
  public ::  emission_simulation
  public ::  post_emission_simulation


  type SimulationParameters
    type(IntegratorParameters) :: integrator_parameters
    type(DistributionParameters) :: distribution_parameters
    double precision :: increase_dt_factor
  end type SimulationParameters

contains

  subroutine initSimulationParameters(this,integrator_type,dist_type,dist_scale)
    type(SimulationParameters), intent(inout) :: this
    character(len=3), intent(in) :: integrator_type
    character(len=1), intent(in) :: dist_type
    double precision, intent(in) :: dist_scale
    call initIntegratorParameters(this%integrator_parameters,integrator_type)
    call initDistributionParameters(this%distribution_parameters,dist_type,dist_scale)
    this%increase_dt_factor = sqrt(sqrt(2.0D+00))
  end subroutine initSimulationParameters

  function getSimulationParametersIntegratorType(this) result(integrator_type)
    type(SimulationParameters), intent(inout) :: this
    character(len=3) :: integrator_type
    integrator_type = getIntegratorParametersIntegratorType(this%integrator_parameters)
  end function getSimulationParametersIntegratorType

  function getSimulationParametersDistType(this) result(dist_type)
    type(SimulationParameters), intent(inout) :: this
    character(len=1) :: dist_type
    dist_type = getDistributionParametersDistType(this%distribution_parameters)
  end function getSimulationParametersDistType

  function getSimulationParametersDistScale(this) result(dist_scale)
    type(SimulationParameters), intent(inout) :: this
    double precision :: dist_scale
    dist_scale = getDistributionParametersDistScale(this%distribution_parameters)
  end function getSimulationParametersDistScale

  subroutine advance_time_step(disc_pulse_in,dt,simulation_parameters)
    type(DiscPulse), intent(inout) :: disc_pulse_in
    double precision, intent(inout):: dt
    type(SimulationParameters, intent(in):: simultion_parameters
    type(DiscPulse) :: new_disc_pulse
    logical :: accept

    call initDiscPulse(new_disc_pulse,0)
    new_disc_pulse = copyDiscPulse(disc_pulse_in)
    dt = original_dt
    accept = .FALSE.

    do while( .not. accept)
      call advance_disc_pulse(simulation_parameters%integrator_parameters, disc_pulse_in, &
        simulation_parameters%distribution_parameters, dt, new_disc_pulse)
      if( selfConsistent(new_disc_pulse) ) then
        accept = .TRUE.
      else
        dt = dt / 2.0D+00
      end if
    end do
    call removeDiscsAbsorbedByCathode(new_disc_pulse)
    freeDiscPulse(disc_pulse_in)
    call initDiscPulse(disc_pulse_in)
    disc_pulse_in = copyDiscPulse(new_disc_pulse)
    freeDiscPulse(new_disc_pulse)
  end subroutine advance_time_step

  subroutine position_dependent_advance(disc_pulse_in,dt,simulation_parameters)
    type(DiscPulse), intent(inout) :: disc_pulse_in
    double precision, intent(inout):: dt
    type(SimulationParameters, intent(in):: simultion_parameters

    call calcPositionDependentField(disc_pulse_in,simulation_parameters%distribution_parameters)
    call advance_time_step(disc_pulse_in,dt,simulation_parameters)
  end subroutine position_dependent_advance

  function emission_simulation(initial_disc_pulse,disc_pulse_out,simulation_parameters) result(time)
    type(InitialDiscPulse), intent(in) :: intial_disc_pulse
    type(DiscPulse), intent(inout) :: disc_pulse_out
    type(SimulationParameters, intent(in):: simultion_parameters
    type(LinkedChargedDisc), pointer :: linked_charged_disc
    double precision :: dt
    double precision :: goal_dt
    double precision :: time
    integer :: i

    time =0.0D+00
    do i = 1, initial_disc_pulse%disc_pulse%number_of_discs - 1, 1
      linked_charged_disc => getLinkedChargedDisc(initial_disc_pulse%disc_pulse%discs, i)
      call addChargedDisc(disc_pulse_out, linked_charged_disc%disc)
      goal_dt = initial_disc_pulse%insertion_times(i+1)
      dt = goal_dt
      do while(time < goal_dt)
        call position_dependent_advance(disc_pulse_out,dt,simulation_parameters)
        time = time + dt
        dt = goal_dt - time
      end do
    end do
    linked_charged_disc => getLinkedChargedDisc(initial_disc_pulse%disc_pulse%discs, &
           initial_disc_pulse%disc_pulse%number_of_discs)
    call addChargedDisc(disc_pulse_out, linked_charged_disc%disc)
  end function emission_simulation

  subroutine post_emission_simulation(disc_pulse,simulation_parameters,time,dt,end_time)
    type(DiscPulse), intent(inout) :: disc_pulse
    type(SimulationParameters, intent(in):: simultion_parameters
    double precisionm intent(inout) :: time
    double precision, intent(in) :: dt
    double precision, intent(in) :: end_time

    do while(time < end_time)
      call position_dependent_advance(disc_pulse,dt,simulation_parameters)
      time = time + dt
      dt = dt * simulation_parameters%increase_dt_factor
    end do
  end subroutine post_emission_simulation

!Report on disc pulse.

end module class_DiscPulseSimulation
