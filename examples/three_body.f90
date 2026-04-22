program three_body

        use oopf

        implicit none
        
        !! declare three particle types: earth, sun, jwst
        type(Particle) :: p1, p2, p3

	!! placeholders for init velocity and init position
	real(real64):: m1, m2, m3
        real(real64):: vx, vy
        real(real64), dimension(3) :: x1, x2, x3
        real(real64), dimension(3) :: v1, v2, v3
	character(len=10) :: n
	real(real64) :: zero = 0.0_real64, tf = 20.0_real64 

	!! Using Mr. P Solver configuration from the papers
		!! https://arxiv.org/abs/1709.04775
		!! https://arxiv.org/abs/1303.0181

	!! m1 = m2 = 1, and m3 can vary
	!! x01 = -x02 = -1 and x03 = 0
	!! vx01 = vx02 we'll call it vx
	!! vy01 = vy02 we'll call it xy
	!! vx3 = -2*vx/m3 and vy03 = - 2vy/m3

	!! only evolving initial conditions are vx, vy, m3 (zero angular momentum)

	m3 = 1_real64
	vx = 0.39295_real64
	vy = 0.09758_real64

	m1 = 1
	m2 = 1

	x1 = [-1.0_real64, 0.0_real64, zero]
        x2 = [ 1.0_real64, 0.0_real64, zero]
        x3 = [ 0.0_real64, 0.0_real64, zero]

        v1 = [ vx, vy, zero]
        v2 = [ vx, vy, zero]
        v3 = [ -2.0 * vx / m3, -2.0 * vy /m3, zero]

	!! initialize mass, charge, initial positions and velocitys
	call p1%init(n, m1, zero, x1, v1)
        call p2%init(n, m2, zero, x2, v2)
        call p3%init(n, m3, zero, x3, v3)

        !! There is a N-body problem solver in the particles.f90 that uses several 
        !! methods from calculus.f90

	!! t0 is implicitly equal to 0       
end program three_body
