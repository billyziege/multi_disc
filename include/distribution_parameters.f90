module class_DistributionParameters
  !This module provides an interface
  !to control which form of the 
  !equation are applied to the 
  !fields.
  implicit none
  private ::  setDistributionParametersDistType
  private ::  setDistributionParametersDistScale
  private ::  calcDistributionParametersCoefficient
  private ::  calcUniformCoefficient
  private ::  calcGaussianCoefficient
  public :: DistributionParameters
  public :: initDistributionParameters
  public :: getDistributionsFieldComponent
  public :: getDistributionParametersCoefficient
  public ::  getDistributionParametersDistType
  public ::  getDistributionParametersDistScale
  public ::  reportDistributionParameters

  interface
    function derfce(x)
      double precision, intent(in) :: x
    end function
  end interface

  type DistributionParameters
    character(len=1) :: dist_type
    double precision :: dist_scale
    double precision :: coefficient
  end type DistributionParameters

contains

  subroutine setDistributionParametersDistType(distribution_parameters,dist_type)
    type(DistributionParameters), intent(inout) :: distribution_parameters
    character(len=1), intent(in) :: dist_type
    distribution_parameters%dist_type = dist_type
  end subroutine setDistributionParametersDistType

  function getDistributionParametersDistType(distribution_parameters) result(dist_type)
    type(DistributionParameters), intent(in) :: distribution_parameters
    character(len=1) :: dist_type
    dist_type = distribution_parameters%dist_type
  end function getDistributionParametersDistType

  subroutine setDistributionParametersDistScale(distribution_parameters,dist_scale)
    type(DistributionParameters), intent(inout) :: distribution_parameters
    double precision, intent(in) :: dist_scale
    distribution_parameters%dist_scale = dist_scale
  end subroutine setDistributionParametersDistScale

  function getDistributionParametersDistScale(distribution_parameters) result(dist_scale)
    type(DistributionParameters), intent(in) :: distribution_parameters
    double precision :: dist_scale
    dist_scale = distribution_parameters%dist_scale
  end function getDistributionParametersDistScale

  function getDistributionParametersCoefficient(distribution_parameters) result(coefficient)
    type(DistributionParameters), intent(in) :: distribution_parameters
    double precision :: coefficient
    coefficient = distribution_parameters%coefficient
  end function getDistributionParametersCoefficient

  subroutine calcDistributionParametersCoefficient(distribution_parameters)
    !Passes the control to a function specific for the distribution for
    !the porition of the field due to "parallel plate" setup.
    !Currently supports uniform and gaussian distributions.
    type(DistributionParameters), intent(inout) :: distribution_parameters
    if( getDistributionParametersDistType(distribution_parameters) == "U" ) then
      distribution_parameters%coefficient = calcUniformCoefficient(distribution_parameters)
    else if ( getDistributionParametersDistType(distribution_parameters) == "G" ) then
      distribution_parameters%coefficient = calcGaussianCoefficient(distribution_parameters)
    end if
  end subroutine calcDistributionParametersCoefficient

  function calcUniformCoefficient(distribution_parameters) result(coefficient)
    !Simply returns the coefficient --- nothing fancy.
    type(DistributionParameters), intent(in) :: distribution_parameters
    double precision :: k = 8.9875517873681764E9 !N m^2/C^2
    double precision :: coefficient
    coefficient = 2*k/( (getDistributionParametersDistScale(distribution_parameters))**2 )
  end function calcUniformCoefficient

  function calcGaussianCoefficient(distribution_parameters) result(coefficient)
    !Simply returns the coefficient --- nothing fancy.
    type(DistributionParameters), intent(in) :: distribution_parameters
    double precision :: k = 8.9875517873681764E9 !N m^2/C^2
    double precision :: coefficient
    coefficient = k/( (getDistributionParametersDistScale(distribution_parameters))**2 )
  end function calcGaussianCoefficient

  subroutine initDistributionParameters(distribution_parameters,dist_type,dist_scale)
    type(DistributionParameters), intent(inout) :: distribution_parameters
    character(len=1), intent(in) :: dist_type
    double precision, intent(in) :: dist_scale
    call setDistributionParametersDistType(distribution_parameters,dist_type)
    call setDistributionParametersDistScale(distribution_parameters,dist_scale)
    call calcDistributionParametersCoefficient(distribution_parameters)
  end subroutine initDistributionParameters

  function getDistributionsFieldComponent(position_i,position_j,distribution_parameters) result(value)
    double precision, intent(in) :: position_i
    double precision, intent(in) :: position_j
    type(DistributionParameters), intent(in) :: distribution_parameters
    double precision :: value
    if( getDistributionParametersDistType(distribution_parameters) == "U" ) then
      value = getUniformFieldComponent(position_i, position_j, distribution_parameters)
    else if ( getDistributionParametersDistType(distribution_parameters) == "G" ) then
      value = getGaussianFieldComponent(position_i, position_j, distribution_parameters)
    end if
  end function getDistributionsFieldComponent
    
  function getUniformFieldComponent(position_i,position_j,distribution_parameters) result(value)
    double precision, intent(in) :: position_i
    double precision, intent(in) :: position_j
    type(DistributionParameters), intent(in) :: distribution_parameters
    double precision :: value
    double precision :: denominator_1
    double precision :: denominator_2
    double precision :: r
    r = getDistributionParametersDistScale(distribution_parameters)
    denominator_1 = sqrt(r**2 + (position_i + position_j)**2)
    denominator_2 = sqrt(r**2 + (position_i - position_j)**2)
    value = (position_i + position_j)/denominator_1 - (position_i-position_j)/denominator_2
    value = getDistributionParametersCoefficient(distribution_parameters)*value
  end function getUniformFieldComponent
    
  function getGaussianFieldComponent(position_i,position_j,distribution_parameters) result(value)
    double precision, intent(in) :: position_i
    double precision, intent(in) :: position_j
    type(DistributionParameters), intent(in) :: distribution_parameters
    double precision :: value
    double precision :: add
    double precision :: sub
    double precision :: sigma_r
    double precision :: pi = 3.1415926535897932384626
    sigma_r = getDistributionParametersDistScale(distribution_parameters)
    add = (position_i + position_j)/(sqrt(2.0)*sigma_r)
    sub = (position_i - position_j)/(sqrt(2.0)*sigma_r)
    value = add*derfce(add)-sub*derfce(abs(sub))
    value = sqrt(pi)*getDistributionParametersCoefficient(distribution_parameters)*value
  end function getGaussianFieldComponent
  
  subroutine reportDistributionParameters(this)
    type(DistributionParameters), intent(in) :: this
    print *, "distribution type = ", this%dist_type
    print *, "distribution scale = ", this%dist_scale
    print *, "distribution coefficient = ", this%coefficient
  end subroutine reportDistributionParameters

end module class_DistributionParameters
