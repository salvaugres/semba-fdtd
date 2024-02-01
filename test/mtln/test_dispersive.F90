integer function test_dispersive_q3phi() bind(C) result(error_cnt)    
    use dispersive_mod

    implicit none
    type(transfer_impedance_per_meter_t) :: zt
    complex, dimension(1) :: residues, poles
    type(pol_res_t):: connector
    ! residues(1)%real = 1e4
    ! residues(1)%imag = 1e3
    ! poles(1)%real = -1e6
    ! poles(1)%imag = 1e1
    
    ! zt%direction = "both"
    ! zt%resistive_term = 1e-2
    ! zt%inductive_term = 1e-6
    ! zt%poles = poles
    ! zt%residues = residues
    
    ! connector = pol_resCtor(zt, 1e-8)


    ! real, dimension(:,:,:,:), allocatable :: q3
    ! real, dimension(:), allocatable :: conn

    ! real, dimension(:,:,:), allocatable :: phi
    ! real, dimension(:,:), allocatable :: q3phi
    ! integer :: nr, ndiv, nc, i_div

    ! nr = 0
    ! ndiv = 3
    ! nc = 2



end function