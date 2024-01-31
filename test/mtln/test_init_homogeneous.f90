integer function test_init_homogeneous() bind(C) result(error_cnt) 
    use mtl_mod
    use testingTools_mod

    implicit none

    character(len=*), parameter :: name = 'line0'
    integer :: i,j

    
    real,dimension(2,2) :: lpul = reshape( &
        source = [ 4.4712610E-07, 1.4863653E-07, 1.4863653E-07, 4.4712610E-07 ], shape = [ 2,2 ] )
    real,dimension(2,2) :: cpul = reshape( &
        source = [ 2.242e-10, -7.453e-11,-7.453e-11, 2.242e-10 ], shape = [ 2,2 ] )
    real,dimension(2,2) :: rpul = reshape( &
        source = [ 0.0, 0.0, 0.0, 0.0 ], shape = [ 2,2 ] )
    real,dimension(2,2) :: gpul = reshape( &
        source = [ 0.0, 0.0, 0.0, 0.0 ], shape = [ 2,2 ] )
    real, dimension(2,3) :: node_positions = reshape( &
        source = [ 0.0, 0.0, 0.0, 100.0, 0.0, 0.0], shape = [2,3], order=(/2,1/) )
    integer, dimension(1) :: ndiv = (/5/)
    type(mtl_t) :: line 
    
    
    error_cnt = 0
    line = mtl_t(lpul, cpul, rpul, gpul, node_positions, ndiv, name)
    call comparePULMatrices(error_cnt, line%lpul, lpul)
    call comparePULMatrices(error_cnt, line%cpul, cpul)
    call comparePULMatrices(error_cnt, line%rpul, rpul)
    call comparePULMatrices(error_cnt, line%gpul, gpul)
    


end function