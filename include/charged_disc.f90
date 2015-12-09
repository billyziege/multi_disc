module class_ChargedDisc
  !The charged disc class represents a disc 
  !of number_of_electrons.  This object
  !will also store important information
  !for propogation calculation
  implicit none
  private
  public :: ChargedDisc 
  public :: initChargedDisc
  public :: setChargedDiscNumberOfElectrons
  public :: getChargedDiscNumberOfElectrons
  public :: getChargedDiscCharge
  public :: getChargedDiscMass
  public :: setChargedDiscExtractionField
  public :: getChargedDiscExtractionField
  public :: setChargedDiscInFrontField
  public :: getChargedDiscInFrontField
  public :: setChargedDiscPositionDependentField
  public :: getChargedDiscPositionDependentField
  public :: getChargedDiscFullField
  public :: setChargedDiscVelocity
  public :: getChargedDiscVelocity
  public :: setChargedDiscPosition
  public :: getChargedDiscPosition
  public :: reportChargedDisc
  public :: copyChargedDisc

  type ChargedDisc
     !Defines the object
     real :: number_of_electrons !Need not be an integer
     double precision :: extraction_field !Always constant
     double precision :: in_front_field !Dependent only on charges in front
     double precision :: position_dependent_field !Dependent on charge and position --- must be calculated at each step.
     double precision :: v
     double precision :: z
  end type ChargedDisc

contains

  subroutine setChargedDiscNumberOfElectrons(this,number_of_electrons)
    type(ChargedDisc), intent(inout) :: this
    real, intent(in) :: number_of_electrons
    this%number_of_electrons = number_of_electrons
  end subroutine setChargedDiscNumberOfElectrons

  function getChargedDiscNumberOfElectrons(this) result(number_of_electrons)
    type(ChargedDisc), intent(in) :: this
    real :: number_of_electrons
    number_of_electrons = this%number_of_electrons
  end function getChargedDiscNumberOfElectrons

  function getChargedDiscCharge(this) result(charge)
    type(ChargedDisc), intent(in) :: this
    real :: charge
    real, parameter :: q_e = -1.60217662E-19 !charge of electron in C
    charge = getChargedDiscNumberOfElectrons(this)*q_e
  end function getChargedDiscCharge

  function getChargedDiscMass(this) result(mass)
    type(ChargedDisc), intent(in) :: this
    real :: mass
    real, parameter :: m_e = 9.10938356E-31 ! mass of electron in kg
    mass = getChargedDiscNumberOfElectrons(this)*m_e
  end function getChargedDiscMass

  subroutine setChargedDiscExtractionField(this,extraction_field)
    type(ChargedDisc), intent(inout) :: this
    double precision, intent(in) :: extraction_field
    this%extraction_field = extraction_field
  end subroutine setChargedDiscExtractionField

  function getChargedDiscExtractionField(this) result(extraction_field)
    type(ChargedDisc), intent(in) :: this
    double precision :: extraction_field
    extraction_field = this%extraction_field
  end function getChargedDiscExtractionField

  subroutine setChargedDiscInFrontField(this,in_front_field)
    type(ChargedDisc), intent(inout) :: this
    double precision, intent(in) :: in_front_field
    this%in_front_field = in_front_field
  end subroutine setChargedDiscInFrontField

  function getChargedDiscInFrontField(this) result(in_front_field)
    type(ChargedDisc), intent(in) :: this
    double precision :: in_front_field
    in_front_field = this%in_front_field
  end function getChargedDiscInFrontField

  subroutine setChargedDiscPositionDependentField(this,position_dependent_field)
    type(ChargedDisc), intent(inout) :: this
    double precision, intent(in) :: position_dependent_field
    this%position_dependent_field = position_dependent_field
  end subroutine setChargedDiscPositionDependentField

  function getChargedDiscPositionDependentField(this) result(position_dependent_field)
    type(ChargedDisc), intent(in) :: this
    double precision :: position_dependent_field
    position_dependent_field = this%position_dependent_field
  end function getChargedDiscPositionDependentField

  function getChargedDiscFullField(this) result(full_field)
    !Returns the sum of the field pieces.
    type(ChargedDisc), intent(in) :: this
    double precision :: full_field
    full_field = getChargedDiscExtractionField(this) &
                   + getChargedDiscInFrontField(this) &
                   + getChargedDiscPositionDependentField(this)
  end function getChargedDiscFullField

  subroutine setChargedDiscVelocity(this,v)
    type(ChargedDisc), intent(inout) :: this
    double precision, intent(in) :: v
    this%v = v
  end subroutine setChargedDiscVelocity

  function getChargedDiscVelocity(this) result(v)
    type(ChargedDisc), intent(in) :: this
    double precision :: v
    v = this%v
  end function

  subroutine setChargedDiscPosition(this,z)
    type(ChargedDisc), intent(inout) :: this
    double precision, intent(in) :: z
    this%z = z
  end subroutine setChargedDiscPosition

  function getChargedDiscPosition(this) result(z)
    type(ChargedDisc), intent(in) :: this
    double precision :: z
    z = this%z
  end function getChargedDiscPosition

  subroutine initChargedDisc(this,number_of_electrons)
    !Initializes the object to specific values (mostly 0)
    type(ChargedDisc), intent(inout) :: this
    real, intent(in) :: number_of_electrons
    double precision :: initial_velocity = 156918.7 !in m/s, corresponds to an energy of 0.07 eV, the expected kinetic energy
    call setChargedDiscNumberOfElectrons(this,number_of_electrons)
    call setChargedDiscExtractionField(this,0.0D+00)
    call setChargedDiscInFrontField(this,0.0D+00)
    call setChargedDiscVelocity(this,initial_velocity)
    call setChargedDiscPosition(this,0.0D+00)
  end subroutine initChargedDisc

  subroutine reportChargedDisc(this)
    type(ChargedDisc), intent(in) :: this
    print *, "Charged disc:"
    print *, "  number of electrons = ", getChargedDiscNumberOfElectrons(this)
    print *, "  charge = ", getChargedDiscCharge(this)
    print *, "  mass = ", getChargedDiscMass(this)
    print *, "  extraction field = ", getChargedDiscExtractionField(this)
    print *, "  in front field = ", getChargedDiscInFrontField(this)
    print *, "  full field = ", getChargedDiscFullField(this)
    print *, "  velocity = ", getChargedDiscVelocity(this)
    print *, "  position = ", getChargedDiscPosition(this)
  end subroutine reportChargedDisc

  function copyChargedDisc(this) result(charged_disc_out)
    !Copy the attributes of charged_disc_in to charged_disc_out
    type(ChargedDisc), intent(in) :: this
    type(ChargedDisc) :: charged_disc_out
    call initChargedDisc(charged_disc_out,getChargedDiscNumberOfElectrons(this))
    call setChargedDiscExtractionField(charged_disc_out,getChargedDiscExtractionField(this))
    call setChargedDiscInFrontField(charged_disc_out,getChargedDiscInFrontField(this))
    call setChargedDiscVelocity(charged_disc_out,getChargedDiscVelocity(this))
    call setChargedDiscPosition(charged_disc_out,getChargedDiscPosition(this))
  end function copyChargedDisc

end module class_ChargedDisc
