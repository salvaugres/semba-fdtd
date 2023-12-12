program demierda
    use utils
    
    implicit none
    
    real, dimension(:,:), allocatable :: mat
    real, dimension(:), allocatable :: ev

    mat = eye(5)
    ev = getEigenValues(mat)


end program demierda