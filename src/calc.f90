Module calculus
    
    use iso_fortran_env, only: real32, real64, real128
    implicit none

    public :: Fixed_Point_Method

    private

    type, public :: Derivative
    !private
        real(kind=real64) :: Differential
    contains
        procedure :: Diff   => Fluxion
        procedure :: SecDiff => SecondFluxion
    end type Derivative

    type, public :: Integrate
    !private
        real(kind=real64) :: Integral
    contains
        procedure :: Euler    => Euler_Method
        procedure :: Simpson  => Simpson_Method
        procedure :: RK2      => RK2_Method         
        procedure :: RK4      => RK4_Method         
        procedure :: ImpRKO2  => Implicit_RK2
        procedure :: AdpRKO4  => Adaptive_RK4
    end type Integrate

contains

Subroutine Fluxion(self, func, x, delta, dydx)

    class(Derivative), intent(in out) :: self
    real(kind=real64), intent(in) :: delta
    real(kind=real64), external :: func
    real(kind=real64), intent(in) :: x
    real(kind=real64), intent(in out) :: dydx

    dydx = (func(x + delta) - func(x))/delta
    
End Subroutine Fluxion


Subroutine SecondFluxion(self,func, x, delta, d2ydx2) 
    
    class(Derivative), intent(in out) :: self
    real(kind=real64), intent(in) :: delta
    real(kind=real64), intent(in) :: x
    real(kind=real64), external :: func
    real(kind=real64), intent(in out) :: d2ydx2

    d2ydx2 =  (func(x + delta) + func(x - delta)  - 2*func(x))/(delta**2)

End Subroutine SecondFluxion

Subroutine Euler_Method(self, func, ab, delta)

    class(Integrate), intent(in out) :: self
    integer(kind=real64):: i, n
    real(kind=real64), intent(inout) :: delta
    real(kind=real64), external :: func
    real(kind=real64), intent(in), dimension(2) :: ab
    real(kind=real64):: xi

    write(*,*) ""
    write(*,'(A)') "Here is Euler Method"

    if (delta <= 0) Then
        delta = 1.0e-4_real64
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
    real(kind=real64), intent(inout) :: delta
    real(kind=real64), optional, intent(in) :: alpha_input
    real(kind=real64), external :: func
    real(kind=real64), intent(in), dimension(2) :: ab
    real(kind=real64) :: xi, yi, k1, k2, alpha

    if (.not. present(alpha_input)) then
        ! Heun has alpha = 1
        ! Ralston method has alpha = 2/3
        ! Midpoint method has alpha = 1/2
        alpha = 1.0_real64
    else
        alpha = alpha_input
    endif

    if (delta <= 0.0_real64) then
        delta = 1.0e-4_real64
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

Subroutine RK4_Method(self, func, ab, delta, y0)

    class(Integrate), intent(in out) :: self
    integer :: i, n
    real(kind=real64), intent(inout) :: delta, y0
    real(kind=real64), external :: func
    real(kind=real64), intent(in), dimension(2) :: ab
    real(kind=real64) :: xi, yi, k1, k2, k3, k4

    if (delta <= 0.0_real64) then
        delta = 1.0e-4_real64
    endif

    self%Integral = func(ab(1))
    n = floor((ab(2) - ab(1)) / delta)
    xi = ab(1)
    yi = y0

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
    real(kind=real64), intent(inout) :: xi 
    real(kind=real64), intent(out) :: xj
    real(kind=real64), external :: func
    real(kind=real64) :: tolerance, max_tolerance

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
    real(kind=real64), intent(inout) :: max_tolerance, xi 
    real(kind=real64), intent(out) :: xj
    real(kind=real64), external :: func
    real(kind=real64), optional, intent(in out) :: delta
    real(kind=real64) :: dfdx, tolerance ! j = i+1

    if (delta <= 0.0_real64) then
        delta = 1.0e-4_real64
    endif

    i = 0

    call Flux%Diff(func, xi, delta, dfdx)
    xj = xi + func(xi)/dfdx
    tolerance = abs(xj - xi)
    do while (tolerance > max_tolerance)
        call Flux%Diff(func, xi, delta, dfdx)
        xj = xi + func(xi)/dfdx
        tolerance = abs(xj - xi)
        if (dfdx < 1.0e-10_real64) then
            print *, "derivative too small, stopping iteration"
            return
        endif
        xi = xj
        i = i + 1
    end do

End subroutine NewtonRapson

Subroutine Implicit_RK2(self, func, ab, delta, y0)
    class(Integrate), intent(in out) :: self
    ! Implicit Euler

    integer :: i, n
    real(kind=real64), intent(inout) :: delta, y0
    real(kind=real64), external :: func
    real(kind=real64), intent(in), dimension(2) :: ab
    real(kind=real64) :: tolerance = 1.0e-2_real32
    real(kind=real64) :: xi, yi, k1, k2, o

    if (delta == 0.0_real64) then
        delta = 1.0e-6_real64
    endif

    !! Aproximate number of steps
    n = floor((ab(2) - ab(1)) / delta)

    !! Initial Values
    xi = ab(1)
    yi = y0

    !! initialize integral result

    self%Integral = 0.0
    do i = 1, n, 1

        k1 = func(xi, yi)

        !o = yi + k2*delta => k2 = (o - yi )/delta
	o = xi + delta

        k2 = imp_euler_fixed_point(func, o, yi, tolerance, delta)
        k2 = (k2 - yi)/delta

        xi = ab(1) + i * delta
        yi = yi + delta * (k1 + k2)/2

       self%Integral = self%Integral + yi * delta
    end do

    contains

    function imp_euler_fixed_point(func, xi, yi, max_tolerance, delta) result(yj)
        real(kind=real64), intent(inout) :: xi, yi, delta
        real(kind=real64), intent(in) :: max_tolerance
        real(kind=real64), external :: func
        real(kind=real64) :: tolerance
        real(kind=real64) :: yj, res0, aux_yi
        integer :: i

	aux_yi = yi

	if (aux_yi == 0.0_real64) then
	    aux_yi = 1.0e-6_real64
	endif

	res0 = aux_yi

        i = 0
        yj = func(xi, aux_yi) * delta + res0

        tolerance = abs((yj - aux_yi) / aux_yi)

        do while (tolerance > max_tolerance)
            aux_yi = yj
            yj = func(xi, aux_yi) * delta + res0
            tolerance = abs((yj - aux_yi) / aux_yi)
            i = i + 1
        end do

    end function imp_euler_fixed_point

End Subroutine Implicit_RK2

Subroutine Adaptive_RK4(self, func, ab, delta, y0)

    class(Integrate), intent(in out) :: self
    integer :: i, n
    real(kind=real64), intent(inout) :: delta
    real(kind=real64), intent(in) :: y0
    real(kind=real64), external :: func
    real(kind=real64), intent(in), dimension(2) :: ab
    real(kind=real64) :: xi, yi, k1, k2, k3, k4, yj, xj
    real(kind=real64) :: ystep, yhalf, xhalf, xstep
    real(kind=real64) :: variation
    real(kind=real64) :: epsilon = 1.0e-6_real64
    real(kind=real64) :: q = 1.0

    self%Integral = 0
    n = floor((ab(2) - ab(1)) / delta)
    xi = ab(1)
    yi = y0

    do while (xi <= ab(2))
	
        k1 = func(xi, yi)
        k2 = func(xi + 0.5 * 0.5 * delta, yi + 0.5 * 0.5 * delta * k1)
        k3 = func(xi + 0.5 * 0.5 * delta, yi + 0.5 * 0.5 * delta * k2)
        k4 = func(xi + 0.5 * delta, yi + 0.5 * delta * k3)

        xhalf = xi + delta/2
	yhalf = yi + (1.0 / 6.0) * 0.5 * delta * (k1 + 2 * k2 + 2 * k3 + k4)

        k1 = func(xhalf, yhalf)
        k2 = func(xhalf + 0.5 * 0.5 * delta, yhalf + 0.5 * 0.5 * delta * k1)
        k3 = func(xhalf + 0.5 * 0.5 * delta, yhalf + 0.5 * 0.5 * delta * k2)
        k4 = func(xhalf + 0.5 * delta, yhalf + 0.5 * delta * k3)

	!!xhalf = xi + delta/2
        yhalf = yhalf + (1.0 / 6.0) * 0.5 * delta * (k1 + 2 * k2 + 2 * k3 + k4)

        k1 = func(xi, yi)
        k2 = func(xi + 0.5 * delta, yi + 0.5 * delta * k1)
        k3 = func(xi + 0.5 * delta, yi + 0.5 * delta * k2)
        k4 = func(xi + delta, yi + delta * k3)

        !!xstep = xi + delta
	ystep = yi + (1.0 / 6.0) * delta * (k1 + 2 * k2 + 2 * k3 + k4)

	variation = (yhalf - ystep)

	q = 0.9_real64*(epsilon / abs(variation))**(0.25_real64)	
	q = min(5.0_real64, max(q, 0.1_real64))	

	if (abs(variation) > epsilon) then
		delta = q*delta
		cycle
	endif

        xi = xi + delta
        yi = yhalf + variation/15.0_real64

	!print*, epsilon, delta

        self%Integral = self%integral +  yi*delta
        delta = q*delta

    end do

End Subroutine Adaptive_RK4

End Module calculus
