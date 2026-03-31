program main

    use iso_fortran_env, only: real32,real64,real128

    use calculus
    use linalg
    use particles

    implicit none

    real(kind=real32):: delta = 1.0e-4_real32
    type(Matrix) :: A
    real(kind=real32) :: det
    integer :: i, j, dim
    real(kind=real32) :: xi, xj, max_tolerance
    real(kind=real32), parameter :: tolerance_threshold = 1.0e-2_real32

    real(kind=real32) :: x0, y0, x_final
    real(kind=real32), dimension(2) :: ab
    type(Integrate) :: integrator


    ! Initial conditions and parameters
    x0 = 1.0_real32    ! Initial value of x (start time)
    y0 = 5.0_real32    ! Initial value of y
    x_final = 5.0_real32  ! Final value of x (end time)

    ab = [x0, x_final]

    ! Call the Implicit RK2 method to solve the differential equation
    call integrator%Euler(func, ab, delta)
    call integrator%Simpson(func, ab, "1/3", delta)
    call integrator%Simpson(func, ab, "3/8", delta)
    call integrator%Simpson(func, ab, "1/8", delta)
    call integrator%RK2(func, ab, delta)
    call integrator%RK4(func, ab, delta)

    xi = 2.0_real32  ! Starting guess
    xj = 3.0_real32
    max_tolerance = 1.0e-3_real32
    
    ! Call the fixed_point subroutine
    ! call Fixed_Point_Method(func, xi, xj, max_tolerance)

    ! Output the result
    !print *, "Fixed point found: ", xj
    !print *, "Initial guess: ", xi
    !print *, "Tolerance: ", max_tolerance

    !dim = 4  ! Size of the matrix

    ! Initialize matrix A
    !call A%init(dim)

    !A%M(1,1) = 1.0
    !A%M(1,2) = 0.0
    !A%M(1,3) = 0.0
    !A%M(2,1) = 0.0
    !A%M(2,2) = 1.0
    !A%M(2,3) = 0.0
    !A%M(3,1) = 0.0
    !A%M(3,2) = 0.0
    !A%M(3,3) = 1.0

    ! Print the matrix
    !print *, "Matrix A:"
    !do i = 1, dim
    !    print *, (A%M(i, j), j = 1, dim)
    !end do

    ! Compute determinant
    !call A%determinant(det)

    !print *, "Determinant of A:", det

contains

    real(kind=real32) function func(x)
        real(kind=real32), intent(in) :: x
        func =  (2.0*x + 5.0)**(1.0/3.0)
    end function func

    ! dy/dx = -y,  exact solution: y = e^(-x)
    ! integral from 0 to 5 of e^(-x) dx = 1 - e^(-5) ≈ 0.9933
    real(kind=real32) function func(x, y)
        real(kind=real32), intent(in) :: x, y
        func = -y
    end function func


end program main
