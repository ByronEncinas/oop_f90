program main

    use iso_fortran_env, only: real32,real64,real128
    use calculus
    use linalg
    use particles

    implicit none

    type(Integrate) :: integrator
    real(kind=real64) :: delta, x0, y0
    real(kind=real64), dimension(2) :: ab

    ab(1) = 0.0_real64
    ab(2) = 5.0_real64
    delta  = 1.0e-6_real64

    y0 = 1.0_real64

    call integrator%ImpRKO2(func, ab, delta, y0)
    call integrator%AdpRKO4(func, ab, delta, y0)

    print *, "Implicit Runge Kutta O2 Integral  = ", integrator%Integral

    print *, "Adaptive Runge Kutta O2 Integral  = ", integrator%Integral

    print *, "Exact Integral                    = ", 1.0_real64 - exp(-5.0_real64)

contains

    ! dy/dx = -y,  exact solution: y = e^(-x)
    ! integral from 0 to 5 of e^(-x) dx = 1 - e^(-5) ≈ 0.9933
    real(kind=real64) function func(x, y)
        real(kind=real64), intent(in) :: x, y
        func = -y
    end function func

end program main
