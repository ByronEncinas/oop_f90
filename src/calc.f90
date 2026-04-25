Module calculus

    use iso_fortran_env, only: real32, real64, real128

    implicit none

    private

    type, public :: Derivative
        real(kind=real64) :: Differential
    contains
        procedure :: Diff   => Deriv
        procedure :: SecDiff => SecDeriv
    end type Derivative

    type, public :: Integrate
        real(kind=real64), dimension(:), allocatable :: Integral
    contains
        procedure :: EUL    => Euler_Method   !! EUL
        procedure :: SM8  => Simpson_Method !! SM8
        procedure :: RK2      => RK2_Method     !! RK2     
        procedure :: RK4      => RK4_Method     !! RK4
        procedure :: IRKO2  => Implicit_RK2   !! IRKO2
        procedure :: ARKO4  => Adaptive_RK4   !! ARKO2
    end type Integrate

contains

Subroutine Deriv(self, func, x, delta, dydx)
    !! func must be a function of x and y with y as optional
    class(Derivative), intent(in out) :: self
    real(kind=real64), intent(in) :: delta
    real(kind=real64), external :: func
    real(kind=real64), intent(in) :: x
    real(kind=real64), intent(in out) :: dydx

    dydx = (func(x + delta) - func(x))/delta
    
End Subroutine Deriv

Subroutine SecDeriv(self,func, x, delta, d2ydx2) 
    
    class(Derivative), intent(in out) :: self
    real(kind=real64), intent(in) :: delta
    real(kind=real64), intent(in) :: x
    real(kind=real64), external :: func
    real(kind=real64), intent(in out) :: d2ydx2

    d2ydx2 =  (func(x + delta) + func(x - delta)  - 2*func(x))/(delta**2)

End Subroutine SecDeriv

Subroutine Euler_Method(self, func, ab, delta)

    class(Integrate), intent(in out) :: self
    integer:: i, n
    real(kind=real64), intent(inout) :: delta
    real(kind=real64), external :: func
    real(kind=real64), intent(in), dimension(2) :: ab
    real(kind=real64):: xi

    write(*,*) ""
    write(*,'(A)') "Here is Euler Method"

    if (delta <= 0) Then
        delta = 1.0e-4_real64
    endif

    n = floor((ab(2) - ab(1))/delta) 

    allocate(self%Integral(n))
    
    write(*,'(A, I0)') "Iteration: ",n

    self%Integral(1) = func(ab(1))*delta

    xi = ab(1) + delta

    Do i = 2, n, 1 ! for midpoints

        self%Integral(i) = self%Integral(i-1) + func(xi)*delta
        xi = xi + delta
    
    End do

    !!write(*,'(A, F20.10)') "Numerical Integration: ", self%Integral(n)

End Subroutine Euler_Method

Subroutine Simpson_Method(self, func, ab, type, delta)
    
    class(Integrate), intent(in out) :: self
    integer:: i=0,j=0,k=0, n
    real(real64), intent(inout) :: delta
    character(len=3), intent(in) :: type ! options: 1/3, 1/8, 3/8
    real(real64), external :: func
    real(real64), intent(in), dimension(2):: ab
    real(real64):: xi, xj, xk
    
    if (type == '1/3') Then !! <---

        write(*,'(A, F20.10)') "Here is Simpsons Method 1/3"

        n = floor((ab(2) - ab(1))/(delta*2)) 

        allocate(self%Integral(n))

        self%Integral(1) = func(ab(1))*delta/3.0 !! f(x_0)

        write(*,'(I0)') n

    xi = ab(1)
        xj = ab(1)
        xk = ab(1)
    i = 1
        j = 2*i-1
        k = 2*i
        xj = xj + delta*j ! odds
        xk = xk + delta*k ! even

        Do i = 2, n-1, 1   

        self%Integral(i) = self%Integral(i-1) &
            + 4*func( xj )*delta/3.0 & !! f(x_(2*i-1))
            + 2*func( xk )*delta/3.0   !! f(x_(2*i)) 
            j = 2*i-1
            k = 2*i
            xj = ab(1) + delta*j ! odds
            xk = ab(1) + delta*k ! even

        End do

    self%Integral(n) = self%Integral(n-1)                 &
        + 4*func(xj)*delta/3.0                            &  ! 4f(x_5)
        + 2*func(xk)*delta/3.0                            &  ! 2f(x_6)
        + 4*func(ab(1) + (2*n-1)*delta)*delta/3.0         &  ! 4f(x_7)
        + func(ab(2))*delta/3.0    

    write(*,'(A)') "Here is Simpsons Method 1/3"
        write(*,'(A, F20.10)') "Numerical Integration: ",self%Integral(n)

    else if (type == '3/8') Then !! <---

        write(*,'(A, F20.10)') "Here is Simpsons Method 3/8"

        n = floor((ab(2) - ab(1))/(delta*3))*3
    delta = (ab(2) - ab(1)) / n

    if (mod(n,3) /= 0) then !! <---
        write(*,*) "Error: n must be multiple of 3"
        stop
    end if !! <---

        allocate(self%Integral(1)) !! this accumulates

        self%Integral(1) = func(ab(1)) + func(ab(2)) !! f(x_0)

        xi = ab(1)

        Do i = 1, n-1, 1

        xi = xi + delta
 
        if (mod(i, 3) == 0) then
                self%Integral(1) = self%Integral(1)+ 2.0*func( xi ) !! f(x_(2*i-1))
        else
                    self%Integral(1) = self%Integral(1) + 3.0*func( xi ) !! f(x_(2*i-1))        
        endif

        End do !! i = n-1 =>

        self%Integral(1) = (self%Integral(1))*3.0*delta/8.0

        write(*,'(A)') "Here is Simpsons Method 3/8"
        write(*,'(A, F20.10)') "Numerical Integration: ",self%Integral(1)

    endif !! <---

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

    n = floor((ab(2) - ab(1)) / delta)
    xi = ab(1)

    allocate(self%Integral(n)) !! this accumulates
    self%Integral(1) = func(ab(1))
    yi = self%Integral(1)

    do i = 2, n, 1

        k1 = func(xi)
        k2 = func(xi + alpha * delta)
        xi = xi + delta
    yi = yi + delta * ( ((1.0 - 1.0/(2.0*alpha)) * k1) + &
                        ( (1.0/(2.0*alpha)) * k2) )

    self%Integral(i) = yi

    end do

    write(*, '(A, F20.10)') "Numerical Integration: ", self%Integral(n)

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

    n = floor((ab(2) - ab(1)) / delta)

    allocate(self%Integral(n)) !! this accumulates

    xi = ab(1)
    yi = y0

    k1 = func(xi, yi)
    k2 = func(xi + 0.5 * delta, yi + 0.5 * delta * k1)
    k3 = func(xi + 0.5 * delta, yi + 0.5 * delta * k2)
    k4 = func(xi + delta, yi + delta * k3)

    yi = yi +  (1.0 / 6.0) * delta * (k1 + 2 * k2 + 2 * k3 + k4)
    self%Integral(1) = yi

    do i = 2, n, 1
        xi = xi + delta
        k1 = func(xi, yi)
        k2 = func(xi + 0.5 * delta, yi + 0.5 * delta * k1)
        k3 = func(xi + 0.5 * delta, yi + 0.5 * delta * k2)
        k4 = func(xi + delta, yi + delta * k3)
        yi = yi +  (1.0 / 6.0) * delta * (k1 + 2 * k2 + 2 * k3 + k4)
        self%Integral(i) = yi
    end do

    write(*, '(A, F20.10)') "Numerical Integration: ", self%Integral(n)

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
    if (abs(xi) < 1.0e-10_real64) then
        print *, "xi is too small, risk of zero division, __Fixed_Point_Method__"
        stop
    end if
        tolerance = abs((xj - xi)/xi)
        xi = xj
        xj = func(xi)
        i = i + 1
    end do

End subroutine Fixed_Point_Method

subroutine NewtonRaphson(func, xi, delta, maxt, yi)

        !! func must be f(x,y)
        real(kind=real64), intent(inout) :: xi, delta
        real(kind=real64), intent(in), optional :: yi
        real(kind=real64), external :: func
        real(kind=real64), intent(in) :: maxt
        real(kind=real64) :: y, dfdx, cond = 0.0
        integer :: i = 0

        if (delta <= 0.0_real64) then
                delta = 1.0e-4_real64
        endif

        if (present(yi)) then
            y = yi
        else
            y = 0.0  ! default value
        end if

        do while ((abs(cond) > maxt) .and. (i<1000))
            !! eval x_n+1 = x_n - f(x,y)/f'(x,y)
            !! normally, y is just a parameters
            dfdx = qdiff(func, xi, y, delta)
            if (abs(dfdx) < 1.0e-10_real64) then
                print*, "derivative too small, stopping..."
                stop
            endif
            cond = func(xi,y)

            if (abs(cond) > 1.0e10_real64) exit

            xi = xi - cond/dfdx

            i = i + 1
        end do

contains

    function qdiff(f, x, y, delta) result(Dx)

    real(kind=real64), external :: f
    real(kind=real64), intent(in) :: x, y, delta
    real(kind=real64) :: Dx

    Dx = (f(x + delta,y) - f(x,y))/delta

    end function

End subroutine NewtonRaphson

Subroutine Implicit_RK2(self, func, ab, delta, y0)

    !!   Crank–Nicolson (implicit trapezoidal rule) with 
    !!   fixed-point predictor

    class(Integrate), intent(in out) :: self
    integer :: i, n
    real(kind=real64), intent(inout) :: delta, y0
    real(kind=real64), external :: func
    real(kind=real64), intent(in), dimension(2) :: ab
    real(kind=real64) :: tolerance = 1.0e-2_real64
    real(kind=real64) :: xi, yi, k1, k2, yi_star

    if (abs(delta) < 1.0e-10_real64) then
        delta = 1.0e-6_real64
    endif

    !! Aproximate number of steps
    n = floor((ab(2) - ab(1)) / delta)

    !! Initial Values
    xi = ab(1)
    yi = y0

    !! initialize integral result
    allocate(self%Integral(n))
    self%Integral(1) = y0

    do i = 1, n, 1

        k1 = func(xi, yi)

        yi_star = imp_euler_fixed_point(func, xi + delta, yi, tolerance, delta)

        !k2 = (yi_star - yi)/delta
    k2 = func(xi + delta, yi_star)

        xi = xi + delta

        yi = yi + delta * (k1 + k2)/2

    self%Integral(i) =  yi

    end do

    contains

    function imp_euler_fixed_point(func, xi, yi, max_tolerance, delta) result(yj)
        real(kind=real64), intent(in) :: xi, yi, delta
        real(kind=real64), intent(in) :: max_tolerance
        real(kind=real64), external :: func
        real(kind=real64) :: tolerance
        real(kind=real64) :: yj, res0, aux_yi
        integer :: i

    aux_yi = yi

    if (abs(yi) < 1.0e-12_real64) then
        aux_yi = 1.0e-6_real64
    endif

    res0 = aux_yi

        i = 0
        yj = func(xi, aux_yi) * delta + res0

        tolerance = abs((yj - aux_yi) / aux_yi)

        do while ((tolerance > max_tolerance) .and. (i<1000))
            aux_yi = yj
            yj = func(xi, aux_yi) * delta + res0
            tolerance = abs((yj - aux_yi) / aux_yi)
            i = i + 1
        end do

    end function imp_euler_fixed_point

End Subroutine Implicit_RK2

Subroutine GaussLegendre(self, func, ab, delta, y0)

    !!class(Derivative) :: f
    class(Integrate), intent(in out) :: self
    integer :: i, n, j
    real(kind=real64), intent(inout) :: delta, y0
    real(kind=real64), external :: func
    real(kind=real64), intent(in), dimension(2) :: ab
    real(kind=real64), dimension(2):: c, K, X1, X2, FNC
    real(kind=real64), dimension(2):: b
    real(kind=real64), dimension(2,2):: GL, Jb, Jinv
    real(kind=real64) :: tolerance = 1.0e-2_real64
    real(kind=real64) :: xi, yi, k1, k2
    real(kind=real64) :: k2_star, k1_star, tol = 1.0e-8_real64
    real(kind=real64) :: det = 0, sqrt36 = sqrt(3.0_real64)/6.0_real64, norma = 0.0_real64 

    !! Butcher Tableu
    c(:) = [ 0.5 - sqrt36,  0.5 + sqrt36]
    c(:) = [ 0.5         , 0.5 ]

    GL(1,1) = 0.25
    GL(1,2) = 0.25 - sqrt36
    GL(2,2) = 0.25 
    GL(2,2) = 0.25 + sqrt36

    if (abs(delta) < 1.0e-10_real64) delta = 1.0e-6_real64

    !! Aproximate number of steps
    n = floor((ab(2) - ab(1)) / delta)

    !! Initial Values
    xi = ab(1)
    yi = y0

    K(:) = [func(xi, yi), func(xi, yi)]

    X1(:) = [xi + c(1)*delta, yi + delta*(GL(1,1)*K(1) + GL(1,2)*K(2))]
    X2(:) = [xi + c(2)*delta, yi + delta*(GL(2,1)*K(1) + GL(2,2)*K(2))]

    Jb(1,:) = [1 - delta*GL(1,1)*qdiff(func, X1(1), X1(2),delta),  - delta*GL(1,2)*qdiff(func, X1(1), X1(2), delta) ]
    Jb(2,:) = [ - delta*GL(2,1)*qdiff(func, X2(1), X2(2),delta), 1 - delta*GL(2,2)*qdiff(func, X2(1), X2(2),delta) ]
    
   det = Jb(1,1)*Jb(2,2) - Jb(1,2)*Jb(2,1)
 
   if (abs(det) < 1.0d-14) then
      print *, "Matrix is singular or nearly singular"
      stop
   end if

   !! initialize integral result
   allocate(self%Integral(n))
   self%Integral(1) = y0

   Jinv(1,1) =  Jb(2,2) / det
   Jinv(1,2) = -Jb(1,2) / det
   Jinv(2,1) = -Jb(2,1) / det
   Jinv(2,2) =  Jb(1,1) / det

   FNC(1) = K(1) - func(X1(1), X1(2))
   FNC(2) = K(2) - func(X2(1), X2(2))
   K = K - matmul(Jinv, FNC)
   norma = sqrt(FNC(1)*FNC(1) + FNC(2)*FNC(2))
   
    do i = 1, n, 1

        K(:) = [func(xi, yi), func(xi, yi)]
        norma = 1.0d0
        j = 1

        do while ((norma > tol) .and. (j<1000))

                X1(:) = [xi + c(1)*delta, yi + delta*(GL(1,1)*K(1) + GL(1,2)*K(2))]
                X2(:) = [xi + c(2)*delta, yi + delta*(GL(2,1)*K(1) + GL(2,2)*K(2))]

                Jb(1,:) = [1 - delta*GL(1,1)*qdiff(func, X1(1), X1(2), delta), &
                - delta*GL(1,2)*qdiff(func, X1(1), X1(2),delta) ]
                Jb(2,:) = [ - delta*GL(2,1)*qdiff(func, X2(1), X2(2),delta), &
                1 - delta*GL(2,2)*qdiff(func, X2(1), X2(2),delta) ]

                det = Jb(1,1)*Jb(2,2) - Jb(1,2)*Jb(2,1)

                if (abs(det) < 1.0d-14) then
                        print *, "Matrix is singular or nearly singular"
                        stop
                end if

                Jinv(1,1) =  Jb(2,2) / det
                Jinv(1,2) = -Jb(1,2) / det
                Jinv(2,1) = -Jb(2,1) / det
                Jinv(2,2) =  Jb(1,1) / det

                FNC(1) = K(1) - func(X1(1), X1(2))
                FNC(2) = K(2) - func(X2(1), X2(2))
                
                K = K - matmul(Jinv, FNC)
                norma = sqrt(FNC(1)*FNC(1) + FNC(2)*FNC(2))
                j = j + 1
        end do
        !! the loop ends and we have K
        k1 = K(1)
        k2 = K(2)

        xi = xi + delta
        yi = yi + delta * (k1 + k2)/2

        self%Integral(i) =  yi

    end do

    contains

    function qdiff(f, x, y, delta) result(Dx)

    real(kind=real64), external :: f
    real(kind=real64), intent(in) :: x, y, delta
    real(kind=real64) :: Dx

    Dx = (f(x + delta,y) - f(x,y))/delta

    end function

End subroutine GaussLegendre

Subroutine Adaptive_RK4(self, func, ab, delta, y0)

    class(Integrate), intent(in out) :: self
    real(kind=real64), intent(inout) :: delta
    real(kind=real64), intent(in) :: y0
    real(kind=real64), external :: func
    real(kind=real64), intent(in), dimension(2) :: ab
    real(kind=real64) :: xi, yi, k1, k2, k3, k4, yj, xj
    real(kind=real64) :: ystep, yhalf, xhalf, xstep, yhh, yf
    real(kind=real64) :: variation, delta2, err
    real(kind=real64) :: epsilon = 1.0e-12_real64
    real(kind=real64) :: q = 1.0
    real(kind=real64), dimension(:), allocatable :: aux
    integer:: n, os, alloc_err = 0, i = 1

    n = floor((ab(2) - ab(1)) / delta)
    allocate(self%Integral(n), stat=alloc_err)
    if (alloc_err /= 0) stop "Allocation failed"
    self%Integral(i) = y0
    xi = ab(1)
    yi = y0

    do while (xi <= ab(2))

        !! one full step
        k1 = func(xi, yi)
        k2 = func(xi + 0.5 * delta, yi + 0.5 * delta * k1)
        k3 = func(xi + 0.5 * delta, yi + 0.5 * delta * k2)
        k4 = func(xi +       delta, yi +  delta * k3)

        yf = yi + (1.0 / 6.0) * delta * (k1 + 2 * k2 + 2 * k3 + k4)

        !! two half steps    
        k1 = func(xi, yi)
        k2 = func(xi + 0.25 * delta, yi + 0.25 * delta * k1)
        k3 = func(xi + 0.25 * delta, yi + 0.25 * delta * k2)
        k4 = func(xi + 0.50 * delta, yi + 0.50 * delta * k3)
        
        xhalf = xi + delta * 0.5
        yhalf = yi + (1.0 / 6.0) * 0.5 * delta * (k1 + 2.0 * k2 + 2.0 * k3 + k4)
        
        k1 = func(xhalf, yhalf)
        k2 = func(xhalf + 0.25 * delta, yhalf + 0.25 * delta * k1)
        k3 = func(xhalf + 0.25 * delta, yhalf + 0.25 * delta * k2)
        k4 = func(xhalf + 0.50 * delta, yhalf + 0.50 * delta * k3)

        yhh = yhalf + (1.0 / 6.0) * 0.5 * delta * (k1 + 2.0 * k2 + 2.0 * k3 + k4)

        err = yhh - yf

        ystep = yhh + err/15.0

        if (abs(err) < epsilon) err = 1.0e-14_real64

        q = 0.9_real64*(epsilon/abs(err))**(0.25_real64)
        q = min(5.0_real64, max(q, 0.1_real64)) 

        err = max(abs(err), 1.0e-14_real64)

        if ((abs(err) > epsilon)) then
                delta = q*delta
                cycle
        endif
        xi = xi + delta
        !!delta = q*delta
        delta = min(q * delta, 0.028_real64)
        

        if (i + 1 >= n) then
                allocate(aux(n), stat=alloc_err)
                if (alloc_err /= 0) stop "Allocation failed"
                aux(1:n) = self%Integral(1:n)
                deallocate(self%Integral)
                os = n  
                n = 2*n
                allocate(self%Integral(n), stat=alloc_err)
                if (alloc_err /= 0) stop "Allocation failed"
                self%Integral(1:os) = aux(1:os)
                self%Integral(os+1:n) = 0.0_real64
                deallocate(aux)
        endif
        self%Integral(i) = yi
        yi = ystep
        i  = i + 1
    end do

    allocate(aux(i-1), stat=alloc_err)
    if (alloc_err /= 0) stop "Allocation failed"
    aux(1:i-1) = self%Integral(1:i-1)
    deallocate(self%Integral)
    allocate(self%Integral(i-1), stat=alloc_err)
    if (alloc_err /= 0) stop "Allocation failed"
    self%Integral(1:i-1) = aux(1:i-1)
    deallocate(aux)

End Subroutine Adaptive_RK4

Subroutine DDop(DD, N, dx)
    real(kind=real64), intent(in out), dimension(:,:) :: DD
    real(kind=real64), intent(in out) :: dx
    integer, intent(in out) :: N
    integer :: i, j

        DD(:,:) = 0.0_real64

        do i = 1, N, 1
                do j = 1, N, 1
                if (i == j) then
                        DD(i, j) = -2.0_real64
                else if (i == j + 1) then !! upper diagonal
                        DD(i, j) = 1.0_real64
                else if (i + 1 == j) then !! lower diagonal
                        DD(i, j) = 1.0_real64
                else
                        cycle
                endif
                end do
        end do

        DD(:,:)   =  DD(:,:) / (dx*dx)

End Subroutine Dop

Subroutine Dop(D, N, dx)
    real(kind=real64), intent(in out), dimension(:,:) :: D
    real(kind=real64), intent(in out) :: dx
    integer, intent(in out) :: N
    integer :: i, j

        D(:,:) = 0.0_real64
        do i = 1, N, 1
                do j = 1, N, 1
                        if (i == j) cycle
                        if (i == j + 1) then
                                D(i, j) = - 1.0_real64
                        endif
                        if (i + 1 == j) then
                                D(i, j) = 1.0_real64
                        endif
                end do
        end do

        D(:,:)   =  D(:,:) / (2.0_real64*dx)
End Subroutine Dop

End Module calculus
