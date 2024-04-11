
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Module thin wires from Wires paper
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module HollandWires_mtln

   use report
   use fdetypes
   use wiresHolland_constants
!!!   
   use mtln_solver_mod, mtln_solver_t => mtln_t 
!!!
   implicit none
   type(mtln_solver_t) :: mtln_solver
   private
!!!   public InitWires_mtln
   public AdvanceWiresE_mtln


contains

   !!subroutine InitWires_mtln()     
   !!     class(mtln_t), pointer :: mtln_wir
   !!!!!     call initNodes(mtln_wir)
   !!     return
   !!endsubroutine InitWires_mtln

   subroutine AdvanceWiresE_mtln(mtln_solver) 
        type (mtln_solver_t) :: mtln_solver
        !class(mtln_t), pointer :: mtln_wir      
        real, dimension(:,:), allocatable :: currents
        real, dimension(:,:), allocatable :: voltages
!       do n=1,HWires%NumCurrentSegments
         !segmento => Hwires%segment(n)
         !segmento%efield_wire2main = real(segmento%efield_wire2main,kind=rkind_wires) - segmento%cte5 * segmento%current
!!!...
         !Segmento%Current          = Segmento%Current + Segmento%cte2*real(Segmento%Efield_main2wire,KIND=RKIND_wires)
!      end do

   !!!     call mtln_step(mtln_wir, currents, voltages)
        
    return

   end subroutine AdvanceWiresE_mtln

end module HollandWires_mtln
