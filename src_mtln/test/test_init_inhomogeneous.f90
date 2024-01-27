integer function test_init_inhomogeneous() result(error_cnt)    
    use mtl_mod
    use testingTools_mod

    implicit none

    character(len=*), parameter :: name = 'line0'
    integer :: i,j

    
    real,dimension(2,2) :: lpul1 = reshape( &
        source = [ 4.4712610E-07, 1.4863653E-07, 1.4863653E-07, 4.4712610E-07 ], shape = [ 2,2 ] )
    real,dimension(2,2) :: lpul2 = reshape( &
        source = [ 2.4712610E-07, 0.4863653E-07, 0.4863653E-07, 2.4712610E-07 ], shape = [ 2,2 ] )
    real,dimension(2,2,2) :: lpul
    
    real,dimension(2,2) :: cpul1 = reshape( &
        source = [ 1.242e-10, -6.453e-11,-6.453e-11, 1.242e-10 ], shape = [ 2,2 ] )
    real,dimension(2,2) :: cpul2 = reshape( &
        source = [ 2.242e-10, -7.453e-11,-7.453e-11, 2.242e-10 ], shape = [ 2,2 ] )
    real,dimension(2,2) :: cpul3 = reshape( &
        source = [ 3.242e-10, -8.453e-11,-8.453e-11, 3.242e-10 ], shape = [ 2,2 ] )
    real,dimension(3,2,2) :: cpul
    
    real,dimension(2,2) :: rpul0 = reshape( source = [ 0.0, 0.0, 0.0, 0.0 ], shape = [ 2,2 ] )
    real,dimension(2,2,2) :: rpul
    
    real,dimension(2,2) :: gpul0 = reshape( source = [ 0.0, 0.0, 0.0, 0.0 ], shape = [ 2,2 ] )
    real,dimension(3,2,2) :: gpul
    
    real, dimension(2,3) :: node_positions = reshape( &
        source = [ 0.0, 0.0, 0.0, 100.0, 0.0, 0.0], shape = [2,3], order=(/2,1/) )
    integer, dimension(1) :: ndiv = (/2/)
    type(mtl_t) :: line 
    
    lpul(1,:,:) = lpul1
    lpul(2,:,:) = lpul2
    cpul(1,:,:) = cpul1
    cpul(2,:,:) = cpul2
    cpul(3,:,:) = cpul3
    rpul(1,:,:) = rpul0
    rpul(2,:,:) = rpul0
    gpul(1,:,:) = gpul0
    gpul(2,:,:) = gpul0
    gpul(3,:,:) = gpul0

    error_cnt = 0
    line = mtl_t(lpul, cpul, rpul, gpul, node_positions, ndiv, name)
    call comparePULMatricesIH(error_cnt, line%lpul, lpul)
    call comparePULMatricesIH(error_cnt, line%cpul, cpul)
    call comparePULMatricesIH(error_cnt, line%rpul, rpul)
    call comparePULMatricesIH(error_cnt, line%gpul, gpul)
    


end function