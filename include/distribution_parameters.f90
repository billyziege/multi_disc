module class_DistributionParameters
  !This module provides an interface
  !to control which form of the 
  !equation are applied to the 
  !fields.
  implicit none
  private ::  setDistributionParametersDistType
  private ::  setDistributionParametersDistScale
  private ::  calcDistributionParametersCoefficient
  private ::  getDistributionParametersDistType
  private ::  getDistributionParametersDistScale
  private ::  getDistributionParametersCoefficient
  private ::  calcUniformCoefficient
  private ::  calcGaussianCoefficient
  public :: DistributionParameters
  public :: initDistributionParameters
  public :: getDistributionsFieldComponent

  type DistributionParameters
    character(len=*) :: dist_type
    real :: dist_scale
    real :: coefficient
  end type DistributionParameters

contains

  subroutine setDistributionParametersDistType(distribution_parameters,dist_type)
    type(DistributionParameters), intent(inout) :: distribution_parameters
    character(len=*), intent(in) :: dist_type
    distribution_parameters%dist_type = dist_type
  end subroutine setDistributionParametersDistType

  function getDistributionParametersDistType(distribution_parameters) result(dist_type)
    type(DistributionParameters), intent(in) :: distribution_parameters
    character(len=*) :: dist_type
    dist_type = distribution_parameters%dist_type
  end function getDistributionParametersDistType

  subroutine setDistributionParametersDistScale(distribution_parameters,dist_scale)
    type(DistributionParameters), intent(inout) :: distribution_parameters
    real, intent(in) :: dist_scale
    distribution_parameters%dist_scale = dist_scale
  end subroutine setDistributionParametersDistScale

  function getDistributionParametersDistScale(distribution_parameters) result(dist_scale)
    type(DistributionParameters), intent(in) :: distribution_parameters
    real :: dist_scale
    dist_scale = distribution_parameters%dist_scale
  end function getDistributionParametersDistScale

  function getDistributionParametersCoefficient(distribution_parameters) result(coefficient)
    type(DistributionParameters), intent(in) :: distribution_parameters
    real :: coefficient
    coefficient = distribution_parameters%coefficient
  end function getDistributionParametersCoefficient

  subroutine calcDistributionParametersCoefficient(distribution_parameters)
    !Passes the control to a function specific for the distribution for
    !the porition of the field due to "parallel plate" setup.
    !Currently supports uniform and gaussian distributions.
    type(DistributionParameters), intent(inout) :: distribution_parameters
    if( getDistributionParametersDistScale(distribution_parameters) == "Uniform" ) then
      distribution_parameters%coefficient = getInFrontUniformCoefficient(distribution_parameters)
    else if ( getDistributionParametersDistScale(distribution_parameters) == "Gaussian" ) then
      distribution_parameters%coefficient = getInFrontGaussianCoefficient(distribution_parameters)
    end if
  end subroutine calcDistributionParametersCoefficient

  function calcUniformCoefficient(distribution_parameters) result(coefficient)
    !Simply returns the coefficient --- nothing fancy.
    type(DistributionParameters), intent(in) :: distribution_parameters
    real :: k = 8.9875517873681764E9 !N m^2/C^2
    real :: coefficient
    coefficient = 2*k/( (getDistributionParametersDistScale(distribution_parameters))^2 )
  end function calcUniformCoefficient

  function calcGaussianCoefficient(distribution_parameters) result(coefficient)
    !Simply returns the coefficient --- nothing fancy.
    type(DistributionParameters), intent(in) :: distribution_parameters
    real :: k = 8.9875517873681764E9 !N m^2/C^2
    real :: coefficient
    coefficient = k/( (getDistributionParametersDistScale(distribution_parameters))^2 )
  end function calcGaussianCoefficient

  subroutine initDistributionParameters(distribution_parameters,dist_type,dist_scale)
    type(DistributionParameters), intent(inout) :: distribution_parameters
    character(len=*), intent(in) :: dist_type
    real, intent(in) :: dist_scale
    call setDistributionParametersDistType(distribution_parameters,dist_type)
    call setDistributionParametersDistScale(distribution_parameters,dist_scale)
    call calcDistributionParametersCoefficient(distribution_parameters)
  end subroutine initDistributionParameters

  function getDistributionsFieldComponent(position_i,position_j,distribution_parameters) result(value)
    real :: position_i
    real :: position_j
    type(DistributionParameters), intent(in) :: distribution_parameters
    real :: value
    if( getDistributionParametersDistType(distribution_parameters) == "Uniform" ) then
      value = getUniformFieldComponent(position_i, position_j, distribution_parameters)
    else if ( getDistributionParametersDistType(distribution_parameters) == "Gaussian" ) then
      value = getGaussianFieldComponent(position_i, position_j, distribution_parameters)
    end if
  end function getDistributionsFieldComponent
    
  function getUniformFieldComponent(position_i,position_j,distribution_parameters) result(value)
    real :: position_i
    real :: position_j
    type(DistributionParameters), intent(in) :: distribution_parameters
    real :: value
    real :: denominator_1
    real :: denominator_2
    real :: r
    r = getDistributionParametersDistScale(distribution_parameters)
    denominator_1 = sqrt(r^2 + (position_1 + position_2)^2)
    denominator_2 = sqrt(r^2 + (position_1 - position_2)^2)
    value = (position_i + position_j)/denominator_1 - (position_1-position_2)/denominator_2
    value = getDistributionParametersCoefficient(distribution_parameters)*value
  end function getUniformFieldComponent
    
  function getGaussianFieldComponent(position_i,position_j,distribution_parameters) result(value)
    real :: position_i
    real :: position_j
    type(DistributionParameters), intent(in) :: distribution_parameters
    real :: value
    real :: add
    real :: sub
    real :: sigma_r
    real :: pi = 3.1415926535897932384626
    sigma_r = getDistributionParametersDistScale(distribution_parameters)
    add = (position_i + position_j)/(sqrt(2)*sigma_r)
    sub = (position_i - position_j)/(sqrt(2)*sigma_r)
    value = add*erfce(add)-sub*erfce(abs(sub))
    value = sqrt(pi)*getDistributionParametersCoefficient(distribution_parameters)*value
  end function getUniformFieldComponent

end module class_DistributionParameters
