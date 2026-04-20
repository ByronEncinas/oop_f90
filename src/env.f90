module environment

    use iso_fortran_env, only: real64, real128

    implicit none
!!
!! geometry: creates a mesh in the corresponding space, containing the other parameters
!! 
    private

    type, public :: Space
    !private
	character(len=20) :: geometry       ! suport for cartesian, cilindrical, spherical, toroidal
        real(kind=real64) :: mass_density   ! gas/fluid/plasma/solid
        real(kind=real64) :: charge_density ! quasineutral (0), constant (rho_E)
        real(kind=real64) :: electric_field
        real(kind=real64) :: magnetic_field
!    contains
!        procedure :: init   => init
    end type Space

contains


end module environment
