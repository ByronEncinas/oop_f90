module LinAlg

    use iso_fortran_env, only: real32

    implicit none

    private

    type, public :: Matrix
    !private
        integer :: dim
        real(kind=real32), allocatable:: M(:,:)
        character(len=20):: data = "Square Matrices ONLY"
    contains
        procedure :: init
        procedure :: determinant
!        procedure :: trace
!        procedure :: eigenmethods
!        procedure :: lu_decomposition
!        procedure :: qr_decomposition
!        procedure :: inverse
    end type Matrix

    type, public :: Vector
    !private
        real(kind=real32):: vector(3)
        real(kind=real32):: norm
        character(len=20):: data = "Vector in Space"
    contains
        procedure :: set   => set
        procedure :: mag   => mag
    end type Vector

contains

    subroutine set(self)

        integer:: i
        class(Vector), intent(in out) :: self

        read*, (self%vector(i), i = 1, 3)

    end subroutine set   

    subroutine mag(self)

        class(Vector), intent(in out) :: self

        ! calculate magnitude of vector
        self%norm = sqrt(self%vector(1)*self%vector(1) + self%vector(2)*self%vector(2) + self%vector(3)*self%vector(3))

    end subroutine mag

    subroutine init(self, dim, zero)

        integer:: i, j
        integer, intent(in) :: dim
        integer, optional :: zero
        class(Matrix), intent(in out) :: self

        self%dim = dim
        
        allocate(self%M(dim,dim))

        if (present(zero)) then
            self%M(:,:) = 0.0
        else
            ! read the matrix here
            do j = 1, dim
                read*, (self%M(i,j), i = 1, dim)
            end do
        endif
    end subroutine init   

    subroutine levi_civita(Eijk)

        integer, intent(in out), dimension(3,3,3):: Eijk

        ! set up all zero
        Eijk(:,:,:) = 0

        ! even permutations
        Eijk(1,2,3) = 1
        Eijk(3,1,2) = 1
        Eijk(2,3,1) = 1

        ! odd permutations
        Eijk(1,3,2) = -1
        Eijk(3,2,1) = -1
        Eijk(2,1,3) = -1

    end subroutine levi_civita

    recursive function recursive_determinant(M, dim) result(deter)
        integer :: i, j, dim
        real(kind=real32) :: deter
        real(kind=real32) :: minor_deter
        real(kind=real32), dimension(:,:), intent(in) :: M 
        real(kind=real32), allocatable:: cof(:,:)

        if (dim == 1) then
            deter = M(1,1)
            return
        end if

        if (dim == 2) then
            deter = M(1,1) * M(2,2) - M(1,2) * M(2,1)
            return
        end if

        allocate(cof(dim-1,dim-1))

        deter = 0.0_real32

        do j = 1, dim ! expanding on row 1 
            call get_minor(M, dim, j, cof)
            minor_deter = recursive_determinant(cof, dim - 1)
            deter = deter + (-1)**(1 + j) * M(1, j) * minor_deter
        end do

        deallocate(cof)

    end function recursive_determinant

    subroutine get_minor(M, dim, j, cof)
        integer, intent(in) :: dim, j
        real(kind=real32), dimension(:,:), intent(in) :: M
        real(kind=real32), dimension(:,:), intent(out) :: cof
        integer :: i, k
        print*, dim
        
        ! removing row 1 and column j
        do i = 1, dim
            if (i==1)then
                cycle
            endif
            ! for dim = 3
            ! if j = 1, then k = 2, 3
            ! if j = 2, then k = 3, 1
            ! if j = 3, then k = 1, 2
            do k = 1, dim
                ! Skip the j-th column and first row
                if (k < j)then
                    cof(i-1, k) = M(i, k)
                elseif (k > j) then
                    cof(i-1, k-1) = M(i, k)
                end if
            end do
        end do

    end subroutine get_minor

    subroutine determinant(self, deter)
        class(Matrix), intent(in) :: self
        real(kind=real32), intent(out) :: deter
        deter = recursive_determinant(self%M, self%dim)
    end subroutine determinant


    ! Subroutine to calculate the trace of a matrix
    ! subroutine trace(self, trace)

    !     integer:: j   ! Loop index for summing diagonal elements
    !     real(kind=real32), intent(out) :: trace  ! Output variable to store the trace value
    !     class(Matrix), intent(in out) :: self  ! Matrix object (input and output)

    !     trace = 0.0_real32  ! Initialize trace to zero

    !     ! Loop over the diagonal elements of the matrix and sum them
    !     do j = 1, self%dim  ! Loop over each diagonal element (1 to dim)
    !         trace = trace + self%M(j,j)  ! Add the j-th diagonal element to the trace
    !     end do

    ! end subroutine trace  ! End of trace subroutine


    ! Subroutine for Gaussian elimination (partial implementation)
    ! subroutine gaussian_elimination(self)

    !     integer:: j   ! Loop index for rows of the matrix
    !     class(Matrix), intent(in out) :: self  ! Matrix object (input and output)
    !     real(kind=real32), dimension(self%dim):: row  ! Array to temporarily store a row of the matrix
    !     real(kind=real32):: elem  ! Variable to store individual matrix elements
        
    !     ! Loop over the rows of the matrix for Gaussian elimination
    !     do j = 1, self%dim  ! Loop from 1 to the number of rows (dim)
    !         vec  = self%M(j,:)  ! Extract the entire j-th row from the matrix and assign it to vec
    !     end do

    ! end subroutine gaussian_elimination  ! End of gaussian_elimination subroutine


    ! Subroutine for QR decomposition (partial implementation)
    ! subroutine qr_decomposition(self, eigenmatrix, eigenvalues)

    !     integer:: j   ! Loop index for the matrix dimension
    !     real(kind=real32), dimension(self%dim), intent(out) :: eigenvalues  ! Array to store eigenvalues
    !     class(Matrix), intent(in out) :: self  ! Matrix object (input and output)

    !     deter = 0.0_real32  ! Initialize determinant (although unused here, typically used in QR)

    ! end subroutine qr_decomp  ! End of qr_decomposition subroutine


    ! Subroutine for LU decomposition (partial implementation)
    ! subroutine lu_decomposition(self, eigenmatrix, eigenvalues)

    !     integer:: j   ! Loop index for matrix operations
    !     real(kind=real32), dimension(self%dim), intent(out) :: eigenvalues  ! Array to store eigenvalues
    !     class(Matrix), intent(in out) :: self  ! Matrix object (input and output)

    !     deter = 0.0_real32  ! Initialize determinant (although unused here, typically used in LU)

    ! end subroutine lu_decomp  ! End of lu_decomposition subroutine


    ! Subroutine for computing eigenvalues (partial implementation)
    ! subroutine eigenmethods(self, eigenmatrix, eigenvalues)

    !     integer:: j   ! Loop index for matrix operations
    !     real(kind=real32), dimension(self%dim), intent(out) :: eigenvalues  ! Array to store eigenvalues
    !     class(Matrix), intent(in out) :: self  ! Matrix object (input and output)

    !     deter = 0.0_real32  ! Initialize determinant (although unused here)

    ! end subroutine eigenmethods  ! End of eigenmethods subroutine


end module LinAlg
