program test_disc_pulse
  use class_ChargedDisc
  use class_LinkedChargedDisc
  use class_DiscPulse
  implicit none
  type(DiscPulse) :: disc_pulse
  type(LinkedChargedDisc), pointer :: linked_charged_disc_1
  type(LinkedChargedDisc), pointer :: linked_charged_disc_2
  character(len=6) :: number_string
  integer :: number_of_discs

  call getarg(1,number_string)
  read (number_string, *) number_of_discs

  call initDiscPulse(disc_pulse,number_of_discs)
  linked_charged_disc_1 => getLinkedChargedDisc(disc_pulse%discs,3)
  call setChargedDiscNumberOfElectrons(linked_charged_disc_1%disc,100.0)
  linked_charged_disc_2 => getLinkedChargedDisc(disc_pulse%discs,2)
  call setChargedDiscNumberOfElectrons(linked_charged_disc_2%disc,5.0)
  call reportDiscPulse(disc_pulse)
  call freeDiscPulse(disc_pulse)
end program test_disc_pulse
