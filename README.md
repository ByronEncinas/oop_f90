# OOP-F90 (Library for Numerical Methods)

I want to practice mathematical methods and I also don't want to forget FORTRAN (although I am married to the version after 1990). Right now, I am interested in the Calculus objects.

The idea is that all the results from the method that take large population samples as input will be compatible with the input of all the Plotting functions as well.

- Next in plan
  - [ ] Adaptive Euler Method
  - [ ] Migrate to fpm: this implies avoiding `external :: func`, and implementing abstract interfaces.

- [x] **Object Particle**: initialize(mass, init_pos, init_vel, charge, ...), displacement updates method

- [x] **Object Calculus**: 
  - [x] First Derivative, Second Derivative (Done)
  - [x] Euler Method (Done)
  - [x] Simpson's Method (1/3, 1/8, 3/8)
  - [x] Implicit Euler Method
References
---

- [4th edition: Chapman, S. J. (2017). Fortran for Scientists and Engineers. McGraw-Hill Education.](https://www.mheducation.com/highered/product/fortran-for-scientists-and-engineers-chapman.html?viewOption=student)
