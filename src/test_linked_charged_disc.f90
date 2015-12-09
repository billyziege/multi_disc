program test_linked_charged_disc
  use class_LinkedChargedDisc
  use class_ChargedDisc
  implicit none
  type(LinkedChargedDisc),pointer :: linked_charged_disc
  type(LinkedChargedDisc),pointer :: chosen_link
  type(ChargedDisc) :: charged_disc1
  type(ChargedDisc) :: charged_disc2
  type(ChargedDisc) :: charged_disc3
  type(ChargedDisc) :: charged_disc4
  type(ChargedDisc) :: charged_disc5
  call initLinkedChargedDisc(linked_charged_disc)
  call reportLinkedChargedDisc(linked_charged_disc)
  call initChargedDisc(charged_disc1,1.0)
  call initChargedDisc(charged_disc2,10.0)
  call initChargedDisc(charged_disc3,100.0)
  call initChargedDisc(charged_disc4,1000.0)
  call initChargedDisc(charged_disc5,10000.0)
  call setChargedDiscPosition(charged_disc3,1.0D+00)
  call insertChargedDisc(linked_charged_disc, charged_disc1, "after")
  call reportLinkedChargedDisc(linked_charged_disc)
  call insertChargedDisc(linked_charged_disc, charged_disc2, "after")
  call insertChargedDisc(linked_charged_disc, charged_disc3, "after")
  call reportLinkedChargedDisc(linked_charged_disc)
  call insertChargedDisc(linked_charged_disc, charged_disc4, "append")
  chosen_link => getLinkedChargedDisc(linked_charged_disc, 3)
  call insertChargedDisc(chosen_link, charged_disc5, "before")
  call reportLinkedChargedDisc(linked_charged_disc)
  call freeAllLinkedDiscs(linked_charged_disc)
end program test_linked_charged_disc
