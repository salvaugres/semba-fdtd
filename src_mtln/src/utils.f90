module utils_mod

   use iso_fortran_env, only: real64
   implicit none
   
   type :: entry
      real, dimension(:), allocatable :: x
   end type entry

   interface operator(+)
      module procedure add_entries
   end interface


contains

   elemental function add_entries(a,b) result(res)
      type(entry), intent(in) :: a,b
      type(entry) :: res
      res%x = a%x + b%x
   end function

   elemental function componentSum(a) result(res)
      type(entry), intent(in) :: a
      real :: res
      res = sum(a%x)
   end function

   function entryMatmul(a,b) result(res)
      type(entry), dimension(:,:,:), intent(in) :: a
      real, dimension(:,:), intent(in) :: b
      real, dimension(:,:,:),allocatable :: b_reshaped

      type(entry), dimension(size(a,1), size(a,2), 1) :: res_temp
      type(entry), dimension(size(a,1), size(a,2)) :: res
      integer :: i,j,k,nz
      type(entry) :: tmp
      
      b_reshaped = reshape(b, [size(b,1),size(b,2),1])
      do nz = 1, size(a,1)
         do j=1,size(b_reshaped,3)
            do i=1,size(A,2)
                  allocate(tmp%x(size(a(nz,i,1)%x)))
                  do k=1,size(A,3)
                     tmp%x = tmp%x + a(nz,i,k)%x * b_reshaped(k,nz,j)
                  enddo
                  res_temp(nz,i,j) = tmp
            enddo
         enddo
      enddo
      
      res = reshape(res_temp, [size(b,1),size(b,2)])

   end function



   function dotMatmul(a,b) result(res)
      type(entry), dimension(:,:,:), intent(in) :: a
      type(entry), dimension(:,:), intent(in) :: b
      type(entry), dimension(:,:,:),allocatable :: b_reshaped

      real, dimension(size(A,1), size(A,2), 1) :: res_temp
      real, dimension(size(A,1), size(A,2)) :: res
      integer :: i,j,k,nz
      real :: tmp
      
      b_reshaped = reshape(b, [size(b,1),size(b,2),1])
      do nz = 1, size(a,1)
         do j=1,size(b_reshaped,3)
            do i=1,size(A,2)
                  tmp = 0.0 
                  do k=1,size(A,3)
                     tmp = tmp + dot_product(a(nz,i,k)%x, b_reshaped(nz,k,j)%x)
                  enddo
                  res_temp(nz,i,j) = tmp
            enddo
         enddo
      enddo
      
      res = reshape(res_temp, [size(b,1),size(b,2)])

   end function

   function eye(dim) result(res)
      real, dimension(dim, dim) :: res
      integer, intent(in) :: dim
      integer :: i

      res = 0
      do i = 1, dim
         res(i,i) = 1.0
      end do

   end function eye

   function getEigenValues(matrix) result(eigvals)
      integer, parameter :: DP = real64
      real(DP), intent(in) :: matrix(:,:)
      real(DP), allocatable, dimension(:,:) :: m1, m2, vl, vr
      real(DP), allocatable, dimension(:) :: eigvals_real, eigvals_imag
      real(DP), allocatable, dimension(:) :: eigvals      
      real(DP) :: dummy(1,1)
      real(DP), allocatable, dimension(:) :: work
      integer :: info, n, lwork, nb = 64
      n = size(matrix,1)
      allocate(m1(n,n), m2(n,n),eigvals_real(n), eigvals_imag(n), eigvals(2*n), vl(n,n), vr(n,n))

      lwork = -1
      m1 = matrix
      m2 = matrix
      call dgeev('n','n', n, m1, n, eigvals_real, eigvals_imag, dummy,1,dummy,1,dummy, lwork, info)
      
      lwork = max((nb+2)*n, nint(dummy(1,1)))
      Allocate (work(lwork))
      
      call dgeev('n','n', n, m2, n, eigvals_real, eigvals_imag, vl,n,vr,n,work, lwork, info)
      eigvals = [eigvals_real, eigvals_imag]
      ! eigvals = cmplx(eigvals_real, eigvals_imag)
      ! eigvals(:)%re = eigvals_real
      ! eigvals(:)%im = eigvals_imag
      ! write(*,*) eigvals

   end function getEigenValues

   function inv(A) result(Ainv)
      real,intent(in) :: A(:,:)
      real            :: Ainv(size(A,1),size(A,2))
      real            :: work(size(A,1))            ! work array for LAPACK
      integer         :: n,info,ipiv(size(A,1))     ! pivot indices

      ! Store A in Ainv to prevent it from being overwritten by LAPACK
      Ainv = A
      n = size(A,1)
      ! SGETRF computes an LU factorization of a general M-by-N matrix A
      ! using partial pivoting with row interchanges.
      call SGETRF(n,n,Ainv,n,ipiv,info)
      if (info.ne.0) stop 'Matrix is numerically singular!'
      ! SGETRI computes the inverse of a matrix using the LU factorization
      ! computed by SGETRF.
      call SGETRI(n,Ainv,n,ipiv,work,n,info)
      if (info.ne.0) stop 'Matrix inversion failed!'
   end function inv

end module utils_mod
