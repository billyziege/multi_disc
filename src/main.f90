!Initial disc pulse
!Determine timesteps between discs
!~~~~
!Insert disc
!Calculate field
!Advance time
!Check consistency (if not, halve time and go back to advance)
!Remove absorbed discs
!Erase earlier disc pulse and promote current disc pulse
!~~~~
!Continue from calculate field until next disc time, then start again from Insert disc
!~~~
!Once all discs are in, start from calculate field.
!Increase time step after each step until we reach 120 ps
!Report on disc pulse.
program test_initial_disc_pulse
  use class_InitialDiscPulse
  implicit none
  type(InitialDiscPulse) :: initial_disc_pulse
  type(ChargedDisc), allocatable, target :: charged_discs(:)
  character(len=6) :: number_of_discs_string
  character(len=9) :: number_of_electrons_string
  integer :: number_of_discs
  real :: number_of_electrons

  call getarg(1,number_of_discs_string)
  read (number_of_discs_string, *) number_of_discs
  allocate(charged_discs(number_of_discs))
  call getarg(2,number_of_electrons_string)
  read (number_of_electrons_string, *) number_of_electrons

  call initInitialDiscPulse(initial_disc_pulse,number_of_discs,number_of_electrons,charged_discs)
  call reportInitialDiscPulse(initial_disc_pulse)
  call freeInitialDiscPulse(initial_disc_pulse)
end program test_initial_disc_pulse
