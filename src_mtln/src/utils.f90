module utils

   implicit none

contains

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
      real, intent(in) :: matrix(:,:)
      real :: m(size(matrix,1),size(matrix,1))
      real :: eigvals(size(matrix,1))
      real :: wi(size(matrix,1))
      real :: vl(1,size(matrix,1)), vr(1,size(matrix,1))
      double precision :: work(50)
      integer :: info

      m = matrix
      call dgeev('n','n', size(matrix,1), m, size(matrix,1),eigvals,wi,vl,1,vr,work, 50, info)

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

end module utils
