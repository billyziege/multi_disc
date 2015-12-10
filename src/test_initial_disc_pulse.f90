program test_initial_disc_pulse
  use class_InitialDiscPulse
  use class_DistributionParameters
  implicit none
  type(InitialDiscPulse) :: initial_disc_pulse
  type(DistributionParameters) :: distribution_parameters
  character(len=6) :: number_of_discs_string
  character(len=9) :: number_of_electrons_string
  character(len=1) :: dist_type
  integer :: number_of_discs
  real :: number_of_electrons

  call getarg(1,number_of_discs_string)
  read (number_of_discs_string, *) number_of_discs
  call getarg(2,number_of_electrons_string)
  read (number_of_electrons_string, *) number_of_electrons
  call getarg(3,dist_type)

  call initDistributionParameters(distribution_parameters,dist_type,1.0D-04)
  call initInitialDiscPulse(initial_disc_pulse,number_of_discs,number_of_electrons,distribution_parameters, 1.0D+06)
  call reportInitialDiscPulse(initial_disc_pulse)
  call freeInitialDiscPulse(initial_disc_pulse)
end program test_initial_disc_pulse
