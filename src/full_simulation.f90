program simulate_dynamic_disc_pulse
  use class_DiscPulse
  use class_InitialDiscPulse
  use class_DiscPulseSimulation
  implicit none
  
  !Input arguments
  character(len=6) :: number_of_discs_string
  character(len=9) :: number_of_electrons_string
  character(len=1) :: dist_type
  character(len=6) :: extraction_field_string
  integer :: number_of_discs
  real :: number_of_electrons
  double precision :: extraction_field

  !Objects necessary for simulation.
  type(SimulationParameters) :: simulation_parameters
  type(InitialDiscPulse) :: initial_disc_pulse
  type(DiscPulse) :: disc_pulse
  double precision :: time
  double precision :: initial_dt = 2D-15
  double precision :: end_time = 120D-12

  call getarg(1,number_of_discs_string)
  read (number_of_discs_string, *) number_of_discs
  call getarg(2,number_of_electrons_string)
  read (number_of_electrons_string, *) number_of_electrons
  call getarg(3,dist_type)
  call getarg(4,extraction_field_string)
  read (extraction_field_string, *) extraction_field
  extraction_field = extraction_field * 1D+06

  if(dist_type == "G") then
    call initSimulationParameters(simulation_parameters,"RK4",dist_type,1.00D-04)
  else if(dist_type == "E") then
    call initSimulationParameters(simulation_parameters,"RK4",dist_type,1.71D-04)
  end if

  call initInitialDiscPulse(initial_disc_pulse, number_of_discs, number_of_electrons, &
               simulation_parameters%distribution_parameters, extraction_field)
  call initDiscPulse(disc_pulse, 0)
  time = emission_simulation(initial_disc_pulse,disc_pulse,simulation_parameters)
  !call post_emission_simulation(disc_pulse,simulation_parameters,time,initial_dt,end_time)
  call reportInitialDiscPulse(initial_disc_pulse)
  call reportDiscPulse(disc_pulse)
  call freeInitialDiscPulse(initial_disc_pulse)
  call freeDiscPulse(disc_pulse)
end program simulate_dynamic_disc_pulse
