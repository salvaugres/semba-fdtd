module networks_mod

    implicit none
    type, public :: network_t

    contains
    ! procedure :: connectToGround
    ! procedure :: connectToGrounR
    ! procedure :: connectToGroundLCpRs
    ! procedure :: connectToGroundC
    ! procedure :: connectToGroundC
    ! procedure :: shortToGround
    ! procedure :: shortNodes
    ! procedure :: updatesources
    ! procedure :: connectNodes
    ! procedure :: advanceVoltage
    ! procedure :: updateVoltages
    ! procedure :: updateCurrents
    ! procedure :: computeVTerms
    ! private
    ! procedure :: getNumberOfStateVarsInConnnector
    ! procedure :: getNumberOfStateVars
    ! procedure :: addNodesInLine
    ! procedure :: addNode
    ! procedure :: iFactor

    end type network_t

    interface network_t
        module procedure networkCtor
    end interface network_t


contains

    function networkCtor() result(res)
        type(network_t) :: res
    end function networkCtor






end module networks_mod