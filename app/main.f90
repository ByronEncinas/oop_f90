program main

    use iso_fortran_env, only: real32,real64,real128
    use calculus
    use linalg
    use particles

    implicit none

    type(Integrate) :: integrator
    real(kind=real32) :: delta, x0, y0
    real(kind=real32), dimension(2) :: ab

    ! Interval [0, 5], step size 0.1
    ab(1) = 0.0_real32
    ab(2) = 5.0_real32
    delta  = 1.0e-4_real32

    ! Initial condition: y(0) = 1
    x0 = 0.0_real32
    y0 = 1.0_real32

    ! Call the implicit euler method through the class
    call integrator%ImpEuler(func, ab, delta, x0, y0)

    ! Print result
    print *, "Numerical Integral = ", integrator%Integral
    print *, "Exact Integral     = ", 1.0_real32 - exp(-5.0_real32)

contains

    ! dy/dx = -y,  exact solution: y = e^(-x)
    ! integral from 0 to 5 of e^(-x) dx = 1 - e^(-5) ≈ 0.9933
    real(kind=real32) function func(x, y)
        real(kind=real32), intent(in) :: x, y
        func = -y
    end function func

end program main
