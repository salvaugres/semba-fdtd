module mtl_mod

    ! use NFDETypes
    use utils_mod
    
    implicit none

    type, public :: mtl_t
        character (len=:), allocatable :: name
        integer  :: number_of_conductors
        real, allocatable, dimension(:,:,:) :: lpul, cpul, rpul, gpul
        real, allocatable, dimension(:,:) :: u, du
        real, allocatable, dimension(:,:,:) :: duNorm(:,:,:)
        real :: time, dt
        ! real, allocatable, dimension(:,:) :: v, i
        ! real, allocatable :: longitudinalE(:,:), transversalE(:,:)
        ! real, allocatable :: vTerm(:,:,:), iTerm(:,:,:)
        
    contains
        procedure :: setTimeStep
        procedure :: initLC
        procedure :: initRG
        procedure :: initDirections
        procedure :: getMaxTimeStep
        ! procedure :: get_time_range
        procedure :: getPhaseVelocities
        procedure :: checkPulDimensions
        !TODO
        ! procedure :: setResistanceInRegion
        ! procedure :: setResistanceAtPoint
        ! procedure :: setConductanceInRegion
        ! procedure :: setConductanceAtPoint
        ! procedure :: addDispersiveConnector

    end type mtl_t
    
    interface mtl_t
        module procedure mtlCtor
    end interface
            

contains

    function mtlCtor(lpul, cpul, rpul, gpul, node_positions, divisions, name) result(res)
        type(mtl_t) :: res
        real, intent(in), dimension(:,:) :: lpul, cpul, rpul, gpul
        real, intent(in), dimension(:,:) :: node_positions
        integer, intent(in), dimension(:) :: divisions
        character(len=*), intent(in) :: name
        real, allocatable, dimension(:,:) :: v, i
        res%name = name
        
        call res%checkPULDimensions(lpul, cpul, rpul, gpul)
        res%number_of_conductors = size(lpul, 1)

        call res%initDirections(divisions, node_positions)
        call res%initLC(lpul, cpul)
        call res%initRG(rpul, gpul)
        
        res%time = 0.0
        res%dt = res%getMaxTimeStep()
        
    end function
      
    subroutine initDirections(this, divisions, node_positions)
        class(mtl_t) :: this
        integer, intent(in), dimension(:) :: divisions
        real, intent(in), dimension(:,:) :: node_positions
        real, dimension(3) :: du
        integer :: i, j, ndiv
        
        allocate(this%u(sum(divisions) + 1,3))
        allocate(this%du(sum(divisions),3))
        allocate(this%duNorm(sum(divisions), this%number_of_conductors, this%number_of_conductors))
        ndiv = 0
        do i = 1, size(divisions)
            du = (node_positions(i+1,:) - node_positions(i,:))/divisions(i)
            this%u(ndiv + 1 : ndiv + divisions(i), :) = reshape(source = [(node_positions(i,:) + du*j, j = 1, divisions(i))], & 
                                                                shape = [divisions(i),3], & 
                                                                order=[2,1])
            
            this%du(ndiv + 1 : ndiv + divisions(i), :) = reshape(source = [( du, j = 1, divisions(i)) ], & 
                                                                 shape =[divisions(i), 3], & 
                                                                 order = [2,1])

            ndiv = ndiv + divisions(i)
        end do
        this%u(size(this%u, 1),:) = node_positions(size(node_positions, 1),:)
        this%duNorm = reshape(source = [(norm2(this%du(j,:))*eye(this%number_of_conductors) , j = 1, size(this%du, 1))], & 
                              shape = [sum(divisions), this%number_of_conductors, this%number_of_conductors], &
                              order=[2,3,1])
    
    end subroutine

    subroutine checkPULDimensions(this, lpul, cpul, rpul, gpul)
        class(mtl_t) :: this
        real, intent(in), dimension(:,:) :: lpul, cpul, rpul, gpul

        if ((size(lpul, 1) /= size(lpul, dim = 2)).or.& 
            (size(cpul, 1) /= size(cpul, dim = 2)).or.&
            (size(rpul, 1) /= size(rpul, dim = 2)).or.&
            (size(gpul, 1) /= size(gpul, dim = 2))) then
            error stop 'PUL matrices are not square'
        endif

        if ((size(lpul, 1) /= size(cpul, 1)).or.&
            (size(lpul, 1) /= size(rpul, 1)).or.&
            (size(lpul, 1) /= size(gpul, 1))) then
            error stop 'PUL matrices do not have the same dimensions'
        endif   
        
    end subroutine

    function getPhaseVelocities(this) result(res)
        class(mtl_t) :: this
        real, dimension(size(this%u,1) - 1, this%number_of_conductors) :: res
        real, dimension(2*this%number_of_conductors) :: ev
        real, dimension(this%number_of_conductors) :: phase_vels
        integer :: k
        
        do k = 1, size(this%u, 1) - 1
            ev = getEigenValues(dble(matmul(this%lpul(k,:,:), this%cpul(k+1,:,:))))
            res(k,:) = 1.0/sqrt(ev(1:this%number_of_conductors))
        enddo
        ! test = reshape(source = [(1.0/sqrt(getEigenValues(dble(matmul(this%lpul(k,:,:), this%cpul(k+1,:,:)))))(1:this%number_of_conductors) , k = 1, size(this%u, 1) -1)],shape = [size(this%u,1) - 1, this%number_of_conductors])

    end function

    function getMaxTimeStep(this) result(res)
        class(mtl_t) :: this
        real :: res
        
        res= minval(pack(this%duNorm, this%duNorm /= 0))/maxval(this%getPhaseVelocities())

    end function

    subroutine initLC(this, lpul, cpul)
        class(mtl_t) :: this
        real, intent(in), dimension(:,:) :: lpul, cpul
        integer :: i
        allocate(this%lpul(size(this%u, 1) - 1, size(lpul, 1), size(lpul, 1)))
        allocate(this%cpul(size(this%u, 1), size(cpul, 1), size(cpul, 1)))

        do i = 1, size(this%lpul, 1) 
            this%lpul(i,:,:) = lpul(:,:) 
        enddo
        do i = 1, size(this%cpul, 1)
            this%cpul(i,:,:) = cpul(:,:)
        enddo
    end subroutine

    subroutine initRG(this, rpul, gpul)
        class(mtl_t) :: this
        real, intent(in), dimension(:,:) :: rpul, gpul
        integer :: i
        allocate(this%rpul(size(this%u, 1) - 1, size(rpul, 1), size(rpul, 1)))
        allocate(this%gpul(size(this%u, 1), size(gpul, 1), size(gpul, 1)))

        do i = 1, size(this%rpul, 1) 
            this%rpul(i,:,:) = rpul(:,:) 
        enddo
        do i = 1, size(this%gpul, 1)
            this%gpul(i,:,:) = gpul(:,:)
        enddo
    end subroutine


    subroutine setTimeStep(this, numberOfSteps, finalTime)
        class(mtl_t) :: this
        integer, intent(in) :: numberOfSteps
        real, intent (in) ::finalTime
        
        this%dt = finalTime/numberOfSteps

    end subroutine        


end module mtl_mod