program main

    use iso_fortran_env, only: real32,real64,real128
    use oopf

    implicit none

    type(Integrate) :: sol
    real(kind=real64) :: delta, x0, y0
    real(kind=real64), dimension(2) :: ab
    real(kind=real64) :: t, C
    integer :: nm
    real(kind=real64) :: y_analytical


    y0 = 1.0_real64
    C  = y0 + 1.0_real64/1000001.0_real64

    ab(1) = 0.0_real64
    ab(2) = 1.0_real64

    delta = 1.0e-6_real64

    t = ab(2)

    call sol%ImpRKO2(stiff, ab, delta, y0)

    nm = size(sol%Integral)
    print*, "Array Size: ", size(sol%Integral)
    print *, "Implicit Runge Kutta O2 Integral  = ", sol%Integral(nm)
    deallocate(sol%Integral)
    delta = 1.0e-6_real64
    call sol%AdpRKO4(stiff, ab, delta, y0)

    nm = size(sol%Integral)
    print*, "Array Size: ", size(sol%Integral)
    print *, "Adaptive Runge Kutta O2 Integral  = ", sol%Integral(nm)
    deallocate(sol%Integral)

contains

    ! dy/dx = -y,  exact solution: y = e^(-x)
    ! integral from 0 to 5 of e^(-x) dx = 1 - e^(-5) ≈ 0.9933
    real(kind=real64) function func(x, y)
        real(kind=real64), intent(in) :: x, y
        func = -y
    end function func


! dy/dx = -1000*y + sin(x)

    real(kind=real64) function stiff(x, y)
        real(kind=real64), intent(in) :: x, y
	stiff = -100.0_real64 * (y - cos(x)) - sin(x)
    end function stiff

end program main
