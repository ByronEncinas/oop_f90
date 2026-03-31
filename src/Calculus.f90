Module Calculus
    
    use iso_fortran_env, only: real32, real64, real128

    implicit none

    public :: Fixed_Point_Method
    public :: Implicit_RK2_Method

    private

    type, public :: Derivative
    !private
        real(kind=real32) :: Differential
    contains
        procedure :: Diff   => Fluxion
        procedure :: SecDiff => SecondFluxion
    end type Derivative

    type, public :: Integrate
    !private
        real(kind=real32) :: Integral
    contains
        procedure :: Euler   => Euler_Method
        procedure :: Simpson => Simpson_Method
        procedure :: RK2 => RK2_Method ! explicit
        procedure :: RK4 => RK4_Method ! explicit
    end type Integrate

contains

Subroutine Fluxion(self, func, x, delta, dydx)

    class(Derivative), intent(in out) :: self
    real(kind=real32), intent(in) :: delta
    real(kind=real32), external :: func
    real(kind=real32), intent(in) :: x
    real(kind=real32), intent(in out) :: dydx

    dydx = (func(x + delta) - func(x))/delta
    
End Subroutine Fluxion


Subroutine SecondFluxion(self,func, x, delta, d2ydx2) 
    
    class(Derivative), intent(in out) :: self
    real(kind=real32), intent(in) :: delta
    real(kind=real32), intent(in) :: x
    real(kind=real32), external :: func
    real(kind=real32), intent(in out) :: d2ydx2

    d2ydx2 =  (func(x + delta) + func(x - delta)  - 2*func(x))/(delta**2)

End Subroutine SecondFluxion

Subroutine Euler_Method(self, func, ab, delta)

    class(Integrate), intent(in out) :: self
    integer(kind=real32):: i, n
    real(kind=real32), intent(inout) :: delta
    real(kind=real32), external :: func
    real(kind=real32), intent(in), dimension(2) :: ab
    real(kind=real32):: xi

    write(*,*) ""
    write(*,'(A)') "Here is Euler Method"

    if (delta <= 0) Then
        delta = 1.0e-4_real32
    endif

    self%Integral = 0
    n = floor((ab(2) - ab(1))/delta) 
    
    write(*,'(A, I0)') "Iteration: ",n

    Do i = 1, n, 1 ! for midpoints

        xi = ab(1) + i*delta
        !write(*,*) self%Integral
        self%Integral = self%Integral + func(xi)*delta
    
    End do

    write(*,'(A, F20.10)') "Numerical Integration: ",self%Integral

End Subroutine Euler_Method

Subroutine Simpson_Method(self, func, ab, type, delta)
    
    class(Integrate), intent(in out) :: self
    integer:: i,j,k, n
    real, optional, intent(inout) :: delta
    character(len=3), intent(in) :: type ! options: 1/3, 1/8, 3/8
    real, external :: func
    real, intent(in), dimension(2):: ab
    real:: xi, xj, xk

    i = 0
    j = 0
    k = 0
    if (type == '1/3') Then

        write(*,'(A, F20.10)') "Here is Simpsons Method 1/3"

        self%Integral = func(ab(1))*delta/3.0
        n = floor((ab(2) - ab(1))/delta/2) 
        write(*,'(I0)') n

        Do i = 1, n, 1 ! i follow odd numbers  
            j = 2*i-1
            k = 2*i

            xi = ab(1) + delta*i
            xj = ab(1) + delta*j ! odds
            xk = ab(1) + delta*k ! even

            self%Integral = self%Integral + 4*func( xj )*delta/3.0 + 2*func( xk )*delta/3.0

        End do

        self%Integral =  self%Integral + func(ab(2))*delta/3.0
        write(*,'(A, F20.10)') "Simpsons Method 1/3"
        write(*,'(A, F20.10)') "Numerical Integration: ",self%Integral

    else if (type == '3/8') Then

        write(*,'(A, F20.10)') "Simpsons Method 3/8"


        self%Integral = (17.0*func(ab(1)) +17.0*func(ab(2)) + &
        & 59*func(ab(1)+delta) +59*func(ab(2)-delta) + &
        & 43*func(ab(1)+2*delta) + 43*func(ab(2)-2*delta) + &
        & 49*func(ab(1)+3*delta) + 49*func(ab(2)-3*delta) )*delta/48.0
        
        n = floor((ab(2) - ab(1))/delta) 

        xi = ab(1)

        Do i = 4, n-4, 1 ! i follow odd numbers  

            xi = ab(1) + delta*i

            self%Integral = self%Integral + func(xi)*delta

        End do

        write(*,'(A, F20.10)') "Numerical Integration: ",self%Integral

    else if (type == '1/8') Then

        write(*,'(A, F20.10)') "Simpsons Method 1/8 (it's a lie, I didn't code this)"


        self%Integral = (17.0*func(ab(1)) +17.0*func(ab(2)) + &
        & 59*func(ab(1)+delta) +59*func(ab(2)-delta) + &
        & 43*func(ab(1)+2*delta) + 43*func(ab(2)-2*delta) + &
        & 49*func(ab(1)+3*delta) + 49*func(ab(2)-3*delta) )*delta/48.0
        
        n = floor((ab(2) - ab(1))/delta) 

        xi = ab(1)

        Do i = 4, n-4, 1 ! i follow odd numbers  

            xi = ab(1) + delta*i

            self%Integral = self%Integral + func(xi)*delta

        End do

        write(*,'(A, F20.10)') "Numerical Integration: ",self%Integral

    end if

End Subroutine Simpson_Method

Subroutine RK2_Method(self, func, ab, delta, alpha_input)

    class(Integrate), intent(in out) :: self
    integer :: i, n
    real(kind=real32), intent(inout) :: delta
    real(kind=real32), optional, intent(in) :: alpha_input
    real(kind=real32), external :: func
    real(kind=real32), intent(in), dimension(2) :: ab
    real(kind=real32) :: xi, yi, k1, k2, alpha

    if (.not. present(alpha_input)) then
        ! Heun has alpha = 1
        ! Ralston method has alpha = 2/3
        ! Midpoint method has alpha = 1/2
        alpha = 1.0_real32
    else
        alpha = alpha_input
    endif

    if (delta <= 0.0_real32) then
        delta = 1.0e-4_real32
    endif

    self%Integral = func(ab(1))
    n = floor((ab(2) - ab(1)) / delta)
    xi = ab(1)
    yi = self%Integral

    do i = 1, n, 1
        k1 = func(xi)
        k2 = func(xi + (1 - 1 / (2 * alpha)) * delta)
        xi = ab(1) + i * delta
        self%Integral = yi + delta * ((1 - 1 / (2 * alpha)) * k1 + 1 / (2 * alpha) * k2)
        yi = self%Integral
    end do

    write(*, '(A, F20.10)') "Numerical Integration: ", self%Integral

End Subroutine RK2_Method

Subroutine Implicit_RK2_Method(func, ab, delta)

    ! Implicit 2nd-order Runge-Kutta method (RK2)
    ! Given a differential equation dy/dt = f(t, y), the RK2 method is:
    !
    ! 1. Calculate k_1 using the current state:
    !    k_1 = f(t_n, y_n)
    !
    ! 2. Solve for k_2 implicitly:
    !    k_2 = f(t_n + h, y_n + h * a21 * k_1)
    !
    ! 3. Update the solution using the weighted combination of k_1 and k_2:
    !    y_(n+1) = y_n + h * b1 * k_1 + h * b2 * k_2

    integer :: i, n
    real(kind=real32), intent(inout) :: delta
    real(kind=real32), external :: func
    real(kind=real32), intent(in), dimension(2) :: ab

    real(kind=real32) :: xi, yi, k1, k2, o


    if (delta <= 0.0_real32) then
        delta = 1.0e-4_real32
    endif

    n = floor((ab(2) - ab(1)) / delta)
    xi = ab(1)
    yi = func(xi)

    do i = 1, n, 1
        !print*, xi, yi
        k1 = func(xi, yi)

        !o = yi + k2*delta => k2 = (o - yi )/delta
        
        k2 = ImpRK2_fixed_point(func, xi, yi, 1.0e-2_real32, delta, yi) 
        k2 = (k2 - yi)/delta
        
        xi = ab(1) + i * delta
        yi = yi + delta * (k1 + k2)/2

    end do

    contains

    function ImpRK2_fixed_point(func, xi, yi, max_tolerance, delta, res0) result(yj)
        real(kind=real32), intent(inout) :: xi, yi, delta, res0
        real(kind=real32), intent(in) :: max_tolerance
        real(kind=real32), external :: func
        real(kind=real32) :: tolerance 
        real(kind=real32) :: yj
        integer :: i

        i = 0
        yj = func(xi, yi) * delta + res0

        tolerance = abs((yj - yi) / yi)

        do while (tolerance > max_tolerance)
            yi = yj
            yj = func(xi, yi) * delta + res0
            tolerance = abs((yj - yi) / yi)
            i = i + 1
        end do

    end function ImpRK2_fixed_point

End Subroutine Implicit_RK2_Method

Subroutine RK4_Method(self, func, ab, delta)

    class(Integrate), intent(in out) :: self
    integer :: i, n
    real(kind=real32), intent(inout) :: delta
    real(kind=real32), external :: func
    real(kind=real32), intent(in), dimension(2) :: ab
    real(kind=real32) :: xi, yi, k1, k2, k3, k4

    if (delta <= 0.0_real32) then
        delta = 1.0e-4_real32
    endif

    self%Integral = func(ab(1))
    n = floor((ab(2) - ab(1)) / delta)
    xi = ab(1)
    yi = self%Integral

    do i = 1, n, 1
        k1 = func(xi, yi)
        k2 = func(xi + 0.5 * delta, yi + 0.5 * delta * k1)
        k3 = func(xi + 0.5 * delta, yi + 0.5 * delta * k2)
        k4 = func(xi + delta, yi + delta * k3)

        xi = ab(1) + i * delta
        self%Integral = self%Integral + (1.0 / 6.0) * delta * (k1 + 2 * k2 + 2 * k3 + k4)
    end do

    write(*, '(A, F20.10)') "Numerical Integration: ", self%Integral

End Subroutine RK4_Method

subroutine Fixed_Point_Method(func, xi, xj, max_tolerance)

    integer :: i
    real(kind=real32), intent(inout) :: xi 
    real(kind=real32), intent(out) :: xj
    real(kind=real32), external :: func
    real(kind=real32) :: tolerance, max_tolerance

    i = 0

    xj = func(xi)
    tolerance = abs(xj - xi)
    
    do while (tolerance > max_tolerance)
        tolerance = abs((xj - xi)/xi)
        print*,xi,xj
        xi = xj
        xj = func(xi)
        i = i + 1
    end do
End subroutine Fixed_Point_Method

subroutine NewtonRapson(func, xi, xj, max_tolerance, delta) 

    type(Derivative):: Flux
    integer :: i
    real(kind=real32), intent(inout) :: max_tolerance, xi 
    real(kind=real32), intent(out) :: xj
    real(kind=real32), external :: func
    real(kind=real32), optional, intent(in out) :: delta
    real(kind=real32) :: dfdx, tolerance ! j = i+1

    if (delta <= 0.0_real32) then
        delta = 1.0e-4_real32
    endif

    i = 0

    call Flux%Diff(func, xi, delta, dfdx)
    xj = xi + func(xi)/dfdx
    tolerance = abs(xj - xi)
    do while (tolerance > max_tolerance)
        call Flux%Diff(func, xi, delta, dfdx)
        xj = xi + func(xi)/dfdx
        tolerance = abs(xj - xi)
        if (dfdx < 1.0e-10_real32) then
            print *, "derivative too small, stopping iteration"
            return
        endif
        xi = xj
        i = i + 1
    end do

End subroutine NewtonRapson


End Module calculus
