Module constants
    
	use iso_fortran_env, only: real64, real128

	!! https://physics.nist.gov/cuu/Constants/links.html

	implicit none

	!! real64 can go over to ~1e-300
	
	real(real64), parameter :: c_si  = 299792458_real64
        real(real64), parameter :: G_si  = 6.6743015e-11_real64   
        real(real64), parameter :: qe_si = 1.6021766634e-19_real64
        real(real64), parameter :: me_si = 9.109383713928e-31_real64

End Module constants
