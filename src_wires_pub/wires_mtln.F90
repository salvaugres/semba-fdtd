
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Module thin wires from Wires paper
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module HollandWires_mtln

   use report
   use fdetypes
!!!   
   use mtln_solver_mod !, mtln_solver_t => mtln_t 
!!!
   implicit none
   
   private   
!!!variables globales del modulo
   REAL (KIND=RKIND_wires)           ::  eps0,mu0
!!!   
   public InitWires_mtln,AdvanceWiresE_mtln
     

contains

   subroutine InitWires_mtln(mtln_wir,thereAreMTLNbundles) 
        class(mtln_t) :: mtln_wir 
        logical :: thereAreMTLNbundles
        if (MTLN_WIR%NUMBER_OF_BUNDLES>=1) then 
           thereAreMTLNbundles=.true.
        else    
           thereAreMTLNbundles=.false.
        endif
        return
   endsubroutine InitWires_mtln

   subroutine AdvanceWiresE_mtln(sgg,Ex,Ey,Ez,Idxe,Idye,Idze,Idxh,Idyh,Idzh,eps00,mu00,mtln_wir)  
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
       real (kind=RKIND), dimension (:), intent(in) :: &
         Idxe(sgg%ALLOC(iHx)%XI : sgg%ALLOC(iHx)%XE),&
         Idye(sgg%ALLOC(iHy)%YI : sgg%ALLOC(iHy)%YE),&
         Idze(sgg%ALLOC(iHz)%ZI : sgg%ALLOC(iHz)%ZE),&
         Idxh(sgg%ALLOC(iEx)%XI : sgg%ALLOC(iEx)%XE),&
         Idyh(sgg%ALLOC(iEy)%YI : sgg%ALLOC(iEy)%YE),&
         Idzh(sgg%ALLOC(iEz)%ZI : sgg%ALLOC(iEz)%ZE)
        class(mtln_t) :: mtln_wir      
        real(KIND=RKIND)    ,  dimension(:,:,:), allocatable :: currents
        real(KIND=RKIND)     , dimension(:,:,:), allocatable :: voltages
        real(KIND=RKIND) :: cte,eps00,mu00
        integer (kind=4) :: i, j, k,direction,m,n,number_of_segments
        eps0=eps00; mu0=mu00; !chapuz para convertir la variables de paso en globales !necesario para permittivity scaling
        
        do m=1,MTLN_WIR%NUMBER_OF_BUNDLES
            number_of_segments=ubound(MTLN_WIR%bundles(m)%SEGMENT_RELATIVE_POSITIONS,1)
            do n=1,number_of_segments                          
                i = MTLN_WIR%bundles(n)%SEGMENT_RELATIVE_POSITIONs(m)%position(1)
                j = MTLN_WIR%bundles(n)%SEGMENT_RELATIVE_POSITIONs(m)%position(2)
                k = MTLN_WIR%bundles(n)%SEGMENT_RELATIVE_POSITIONs(m)%position(3)
                direction=MTLN_WIR%bundles(n)%SEGMENT_RELATIVE_POSITIONs(m)%direction
                select case (abs(direction))  
                case(1)
                    voltages(i,j,k) = sign(Ex(I,J,K),real(direction,kind=rkind)) / Idxe(i) 
                case(2)
                    voltages(i,j,k) = sign(Ey(I,J,K),real(direction,kind=rkind)) / Idye(j) 
                case(3)
                    voltages(i,j,k) = SIGN(Ez(I,J,K),real(direction,kind=rkind)) / Idze(k) 
                end select
            end do
        end do
        !
        call mtln_step(mtln_wir, currents, voltages)
        !
        do m=1,MTLN_WIR%NUMBER_OF_BUNDLES
            number_of_segments=ubound(MTLN_WIR%bundles(n)%SEGMENT_RELATIVE_POSITIONS,1)
            do n=1,number_of_segments                          
                i = MTLN_WIR%bundles(n)%SEGMENT_RELATIVE_POSITIONs(m)%position(1)
                j = MTLN_WIR%bundles(n)%SEGMENT_RELATIVE_POSITIONs(m)%position(2)
                k = MTLN_WIR%bundles(n)%SEGMENT_RELATIVE_POSITIONs(m)%position(3)
                direction=1 !MTLN_WIR%bundles(n)%SEGMENT_RELATIVE_POSITIONs(m)%position
                select case (abs(direction))  
                case(1)   
                    cte=sgg%dt /eps0 *Idyh(j)*idzh(k)
                    Ex(i,j,k) = Ex(I,J,K) - sign(cte,real(direction,kind=rkind))*currents(I,J,K)
                case(2)     
                    cte=sgg%dt /eps0 *Idzh(k)*idxh(i)
                    Ey(i,j,k) = Ey(I,J,K) - sign(cte,real(direction,kind=rkind))*currents(I,J,K)
                case(3)   
                    cte=sgg%dt /eps0 *Idxh(i)*idyh(j)
                    Ez(i,j,k) = Ez(I,J,K) - sign(cte,real(direction,kind=rkind))*currents(I,J,K)
                end select
               !segmento%efield_wire2main = real(segmento%efield_wire2main,kind=rkind_wires) - segmento%cte5 * segmento%current
            end do
        end do
    return

   end subroutine AdvanceWiresE_mtln

end module HollandWires_mtln
