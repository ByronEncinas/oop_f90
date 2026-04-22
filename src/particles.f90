module particles

    use iso_fortran_env, only: real64, real128
    use constants
    implicit none

    private

    type, public :: Particle
    !private
        character(len=10) :: name
        real(kind=real64) :: mass
        real(kind=real64) :: charge
        real(kind=real64), dimension(3) :: position
        real(kind=real64), dimension(3) :: velocity
    contains
        procedure :: init   => init
        procedure :: displacement => displacement
        !procedure :: efield => electric_field
        !procedure :: gfield => gravitational_field
    end type Particle

    public :: three_body

contains


    subroutine init(self, name, mass, charge, position, velocity)

        class(Particle), intent(in out) :: self
        character(len=10), intent(in) :: name
        real(kind=real64), intent(in) ::  mass, charge
        real(kind=real64), intent(in), dimension(3) :: position, velocity

        self%name = name
        self%mass = mass
        self%charge = charge
        self%position = position
        self%velocity = velocity

    end subroutine init   

    subroutine displacement(self, new_position, displacement_vector)

        class(Particle), intent(in out) :: self
        real(kind=real64), intent(in), dimension(3) :: new_position
        real(kind=real64), intent(out), dimension(3) :: displacement_vector

        displacement_vector = new_position - self%position

    end subroutine displacement

    function efield(self, pos)result(ef)

        class(Particle), intent(in out) :: self
        real(kind=real64), intent(in), dimension(3) :: pos
        real(kind=real64), dimension(3) :: rel_pos
        real(kind=real64) :: ef, norm, epsilon = 1.0e-12_real64
        !! position must be initialized
        !! softened potential

        rel_pos = pos - self%position
        norm = sqrt(rel_pos(1)*rel_pos(1) + rel_pos(2)*rel_pos(2) +&
			 rel_pos(3)*rel_pos(3))
        ef = self%charge/(norm*norm*norm)

    end function efield

    subroutine three_body(interaction, p1, p2, p3, tf)

        class(Particle), intent(in out) :: p1, p2, p3
        real(kind=real64), intent(in) :: tf
        real(kind=real64), external :: interaction
	

    end subroutine three_body
    
end module particles
