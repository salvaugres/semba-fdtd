
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Module thin wires from Wires paper
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module Wire_bundles_mtln_mod

   use report
   use fdetypes
   use mtln_solver_mod , mtln_solver_t => mtln_t 

   implicit none
   
   REAL (KIND=RKIND_wires)           ::  eps0,mu0
   private   
   public InitWires_mtln,AdvanceWiresE_mtln
     

contains


   subroutine InitWires_mtln(sgg,Ex,Ey,Ez,mtln_solver,thereAreMTLNbundles)         
      type (SGGFDTDINFO), intent(IN), target    :: sgg 
      REAL (KIND=RKIND), intent(inout), target :: &
         Ex(sgg%Alloc(iEx)%XI : sgg%Alloc(iEx)%XE,  &
            sgg%Alloc(iEx)%YI : sgg%Alloc(iEx)%YE,  &
            sgg%Alloc(iEx)%ZI : sgg%Alloc(iEx)%ZE), &
         Ey(sgg%Alloc(iEy)%XI : sgg%Alloc(iEy)%XE,  &
            sgg%Alloc(iEy)%YI : sgg%Alloc(iEy)%YE,  &
            sgg%Alloc(iEy)%ZI : sgg%Alloc(iEy)%ZE), &
         Ez(sgg%Alloc(iEz)%XI : sgg%Alloc(iEz)%XE,  &
            sgg%Alloc(iEz)%YI : sgg%Alloc(iEz)%YE,  &
            sgg%Alloc(iEz)%ZI : sgg%Alloc(iEz)%ZE)   
      class(mtln_solver_t) :: mtln_solver  
        logical :: thereAreMTLNbundles
      
      if (mtln_solver%number_of_bundles>=1) then 
           thereAreMTLNbundles=.true.
        else    
           thereAreMTLNbundles=.false.
           return
        endif
      
      block
         integer (kind=4) :: i, j, k, m, n
         do m = 1, mtln_solver%NUMBER_OF_BUNDLES
            do n = 1, ubound(mtln_solver%bundles(m)%external_field_segments,1)
               call readGridIndices(i, j, k, mtln_solver%bundles(m)%external_field_segments(n))                          
               select case (abs(mtln_solver%bundles(m)%external_field_segments(n)%direction))  
                case(1)                                
                  mtln_solver%bundles(m)%external_field_segments(n)%Efield_wire2main => Ex(i, j, k) 
                  mtln_solver%bundles(m)%external_field_segments(n)%Efield_main2wire => Ex(i, j, k) 
                case(2)     
                  mtln_solver%bundles(m)%external_field_segments(n)%Efield_wire2main => Ey(i, j, k)
                  mtln_solver%bundles(m)%external_field_segments(n)%Efield_main2wire => Ey(i, j, k)  
                case(3)     
                  mtln_solver%bundles(m)%external_field_segments(n)%Efield_wire2main => Ez(i, j, k)
                  mtln_solver%bundles(m)%external_field_segments(n)%Efield_main2wire => Ez(i, j, k)  
                end select
                 
            end do
        end do
      end block

   endsubroutine InitWires_mtln

   subroutine AdvanceWiresE_mtln(sgg,Idxh, Idyh, Idzh, eps00,mu00,mtln_solver)  
      class(mtln_solver_t) :: mtln_solver  
      type (SGGFDTDINFO), intent(IN), target    :: sgg      
       real (kind=RKIND), dimension (:), intent(in) :: &
         Idxh(sgg%ALLOC(iEx)%XI : sgg%ALLOC(iEx)%XE),&
         Idyh(sgg%ALLOC(iEy)%YI : sgg%ALLOC(iEy)%YE),&
         Idzh(sgg%ALLOC(iEz)%ZI : sgg%ALLOC(iEz)%ZE)  
        real(KIND=RKIND) :: cte,eps00,mu00
      integer (kind=4) :: i, j, k, direction, m, n
      real (kind= rkind) :: current
        REAL (KIND=RKIND),pointer:: punt
        
      eps0 = eps00 
      mu0 = mu00
      
      call mtln_solver%step()
        
      do m = 1, mtln_solver%number_of_bundles
         do n = 1, ubound(mtln_solver%bundles(m)%external_field_segments,1)
            call readGridIndices(i, j, k, mtln_solver%bundles(m)%external_field_segments(n))                          
            direction = mtln_solver%bundles(m)%external_field_segments(n)%direction    
            punt => mtln_solver%bundles(m)%external_field_segments(n)%Efield_wire2main
            current = mtln_solver%bundles(m)%i(1, n)
                select case (abs(direction))  
                case(1)   
                    cte=sgg%dt /eps0 *Idyh(j)*idzh(k)
                case(2)     
                    cte=sgg%dt /eps0 *Idzh(k)*idxh(i)
                case(3)   
                    cte=sgg%dt /eps0 *Idxh(i)*idyh(j)
                end select
            punt = punt - sign(cte,real(direction,kind=rkind)) * current
            end do
        end do

   end subroutine AdvanceWiresE_mtln


   subroutine readGridIndices(i, j, k, field_segment)
      type(external_field_segment_t), intent(in) :: field_segment
      integer, intent(inout) :: i, j, k
      i = field_segment%position(1)
      j = field_segment%position(2)
      k = field_segment%position(3)
   end subroutine


end module Wire_bundles_mtln_mod
