program test_charged_disc
  use class_ChargedDisc
  implicit none
  type(ChargedDisc) :: charged_disc
  real :: number_of_electrons = 100.0
  call initChargedDisc(charged_disc,number_of_electrons)
  call reportChargedDisc(charged_disc)
end program test_charged_disc
