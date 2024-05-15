
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Module thin wires from Wires paper
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module Wire_bundles_mtln_mod

   use report
   use fdetypes
   use mtln_solver_mod , mtln_solver_t => mtln_t 
   use mtln_types_mod, only: mtln_t
   use HollandWires
   use wiresHolland_constants
   use ilumina
   implicit none
   
   REAL (KIND=RKIND_wires)           ::  eps0,mu0
   private   
   public InitWires_mtln,AdvanceWiresE_mtln
   type(mtln_solver_t) :: mtln_solver
   integer, dimension(:,:), allocatable :: indexMap

contains


   subroutine InitWires_mtln(sgg,Ex,Ey,Ez, eps00, mu00, mtln_parsed,thereAreMTLNbundles)
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
      real(KIND=RKIND) :: eps00,mu00
   
      type(mtln_t) :: mtln_parsed
      logical :: thereAreMTLNbundles
      
      eps0 = eps00
      mu0 = mu00

      mtln_solver = mtlnCtor(mtln_parsed)

      if (mtln_solver%number_of_bundles>=1) then 
           thereAreMTLNbundles=.true.
        else    
           thereAreMTLNbundles=.false.
           return
        endif
      
      block
         integer (kind=4) :: i, j, k, m, n
         do m = 1, mtln_solver%number_of_bundles
            do n = 1, ubound(mtln_solver%bundles(m)%external_field_segments,1)
               call readGridIndices(i, j, k, mtln_solver%bundles(m)%external_field_segments(n))                          
               select case (abs(mtln_solver%bundles(m)%external_field_segments(n)%direction))  
                case(1)                                
                  mtln_solver%bundles(m)%external_field_segments(n)%field => Ex(i, j, k) 
                  ! mtln_solver%bundles(m)%external_field_segments(n)%Efield_wire2main => Ex(i, j, k) 
                  ! mtln_solver%bundles(m)%external_field_segments(n)%Efield_main2wire => Ex(i, j, k) 
                case(2)     
                  mtln_solver%bundles(m)%external_field_segments(n)%field => Ey(i, j, k) 
                  ! mtln_solver%bundles(m)%external_field_segments(n)%Efield_wire2main => Ey(i, j, k)
                  ! mtln_solver%bundles(m)%external_field_segments(n)%Efield_main2wire => Ey(i, j, k)  
                case(3)     
                  mtln_solver%bundles(m)%external_field_segments(n)%field => Ez(i, j, k) 
                  ! mtln_solver%bundles(m)%external_field_segments(n)%Efield_wire2main => Ez(i, j, k)
                  ! mtln_solver%bundles(m)%external_field_segments(n)%Efield_main2wire => Ez(i, j, k)  
                end select
                 
            end do
        end do
      end block
      block
         integer(kind=4) :: m, n, wIndex, init, end, sep
         type(Thinwires_t), pointer  ::  hwires
         character(20) :: charConductor
         hwires => GetHwires()
         indexMap = mapFieldToCurrentSegments(hwires, mtln_solver%bundles)
         do m = 1, mtln_solver%number_of_bundles
            do n = 1, ubound(mtln_solver%bundles(m)%lpul,1)
               wIndex = indexMap(m,n)

               mtln_solver%bundles(m)%lpul(n,1,1) = hwires%CurrentSegment(wIndex)%Lind
               mtln_solver%bundles(m)%cpul(n,1,1) = mu0*eps0/hwires%CurrentSegment(wIndex)%Lind
               
            end do
            mtln_solver%bundles(m)%cpul(ubound(mtln_solver%bundles(m)%cpul,1),1,1) = &
               mtln_solver%bundles(m)%cpul(ubound(mtln_solver%bundles(m)%cpul,1)-1,1,1)
         end do

         do m = 1, mtln_solver%number_of_bundles
            do n = 1, mtln_solver%bundles(m)%number_of_conductors
               write(charConductor, *) n
               init = lbound(mtln_solver%bundles(m)%cpul,1)
               end = ubound(mtln_solver%bundles(m)%cpul,1)
               sep = index(mtln_solver%bundles(m)%name,"_")
               call mtln_solver%network_manager%circuit%modifyLineCapacitorValue(&
                  trim(mtln_solver%bundles(m)%name(sep+1:))//"_"//trim(adjustl(charConductor))//"_initial",&
                  mtln_solver%bundles(m)%cpul(init,n,n)*mtln_solver%bundles(m)%step_size(init)*0.5)

               call mtln_solver%network_manager%circuit%modifyLineCapacitorValue(&
                  trim(mtln_solver%bundles(m)%name(sep+1:))//"_"//trim(adjustl(charConductor))//"_end",&
                  mtln_solver%bundles(m)%cpul(end,n,n)*mtln_solver%bundles(m)%step_size(end-1)*0.5)

               end do
         end do   

      end block

      call mtln_solver%updatePULTerms()

   contains


   function mapFieldToCurrentSegments(wires, bundles) result (indexMap)
      type(Thinwires_t), pointer  ::  wires
      type(mtl_bundle_t), allocatable, dimension(:) :: bundles
      integer, dimension(:,:), allocatable :: indexMap
      integer :: m, n, nmax, iw
      integer :: i, j, k
      nmax = 0
      do m = 1, mtln_solver%number_of_bundles
         if (ubound(mtln_solver%bundles(m)%lpul,1) > nmax) then 
            nmax = ubound(mtln_solver%bundles(m)%lpul,1)
         end if 
      end do
      allocate(indexMap(m,nmax))
      do m = 1, mtln_solver%number_of_bundles
         do n = 1, ubound(mtln_solver%bundles(m)%lpul,1)
            call readGridIndices(i, j, k, mtln_solver%bundles(m)%external_field_segments(n))                          
            do iw = 1, wires%NumCurrentSegments
               if ((i == wires%CurrentSegment(iw)%i) .and. &
                   (j == wires%CurrentSegment(iw)%j) .and. &
                   (k == wires%CurrentSegment(iw)%k)) then
                     indexMap(m,n) = iw
               end if
            end do
         end do
      end do
   end function


   endsubroutine InitWires_mtln

   subroutine AdvanceWiresE_mtln(sgg,Idxh, Idyh, Idzh, eps00,mu00, still_planewave_time)  
      type (SGGFDTDINFO), intent(IN), target    :: sgg      
      real (kind=RKIND), dimension (:), intent(in) :: &
         Idxh(sgg%ALLOC(iEx)%XI : sgg%ALLOC(iEx)%XE),&
         Idyh(sgg%ALLOC(iEy)%YI : sgg%ALLOC(iEy)%YE),&
         Idzh(sgg%ALLOC(iEz)%ZI : sgg%ALLOC(iEz)%ZE)  
      real(KIND=RKIND) :: cte,eps00,mu00
      logical :: still_planewave_time
      integer (kind=4) :: i, j, k, direction, m, n, wIndex
      real (kind= rkind) :: current
      REAL (KIND=RKIND),pointer:: punt
      type(Thinwires_t), pointer  ::  hwires
      type (CurrentSegments), pointer  ::  segment
      real(kind=rkind) :: dx, dy, dz
      real(kind=rkind) :: external_field

      eps0 = eps00 
      mu0 = mu00

      
      
      hwires => GetHwires()
      
      do m = 1, mtln_solver%number_of_bundles
         do n = 1, ubound(mtln_solver%bundles(m)%external_field_segments,1)
            current = mtln_solver%bundles(m)%i(1, n)
            direction = mtln_solver%bundles(m)%external_field_segments(n)%direction
            punt => mtln_solver%bundles(m)%external_field_segments(n)%field
            ! punt => mtln_solver%bundles(m)%external_field_segments(n)%Efield_wire2main
            ! segment => hwires%CurrentSegment(indexMap(m,n))
            
            call readGridIndices(i, j, k, mtln_solver%bundles(m)%external_field_segments(n))      

            ! external_field = Incid(sgg, 1, abs(direction), mtln_solver%time, i,j,k, still_planewave_time, .false.)
            ! mtln_solver%bundles(m)%external_field_segments(n)%prev_external_field = &
            !    mtln_solver%bundles(m)%external_field_segments(n)%external_field
            ! mtln_solver%bundles(m)%external_field_segments(n)%external_field = external_field


            cte = sgg%dt / eps0
            select case (abs(direction))  
            case(1)   
               cte=cte *(idyh(j)*idzh(k))
            case(2)     
               cte=cte *(idxh(i)*idzh(k))
            case(3)   
               cte=cte *(idxh(i)*idyh(j))
            end select
            
            ! mtln_solver%bundles(m)%external_field_segments(n)%Efield_wire2main = &
            !    mtln_solver%bundles(m)%external_field_segments(n)%Efield_wire2main - sign(cte,real(direction,kind=rkind)) * current
            ! punt = punt - segment%cte5 * current
            punt = real(punt, kind=rkind_wires) - sign(cte,real(direction,kind=rkind)) * current
            ! mtln_solver%bundles(m)%external_field_segments(n)%Efield_main2wire = punt

            hwires%CurrentSegment(indexMap(m,n))%CurrentPast = current
            ! hwires%CurrentSegment(indexMap(m,n))%Current = current
            ! hwires%CurrentSegment(indexMap(m,n))%Efield_wire2main = punt
            ! hwires%CurrentSegment(indexMap(m,n))%Efield_main2wire = &
            !    mtln_solver%bundles(m)%external_field_segments(n)%Efield_main2wire


         end do
      end do

      call mtln_solver%step()

      ! do m = 1, mtln_solver%number_of_bundles
      !    do n = 1, ubound(mtln_solver%bundles(m)%external_field_segments,1)
      !       call readGridIndices(i, j, k, mtln_solver%bundles(m)%external_field_segments(n))                          
            
      !       direction = mtln_solver%bundles(m)%external_field_segments(n)%direction
      !       external_field = Incid(sgg, 1, abs(direction), mtln_solver%time, i,j,k, still_planewave_time, .false.)
      !       mtln_solver%bundles(m)%external_field_segments(n)%prev_external_field = &
      !          mtln_solver%bundles(m)%external_field_segments(n)%external_field

      !       mtln_solver%bundles(m)%external_field_segments(n)%external_field = external_field
      !       hwires%CurrentSegment(indexMap(m,n))%CurrentPast = mtln_solver%bundles(m)%i(1, n)
      !    end do
      ! end do
      
      ! call mtln_solver%step()

      ! do m = 1, mtln_solver%number_of_bundles
      !    do n = 1, ubound(mtln_solver%bundles(m)%external_field_segments,1)
      !       call readGridIndices(i, j, k, mtln_solver%bundles(m)%external_field_segments(n))                          

      !       direction = mtln_solver%bundles(m)%external_field_segments(n)%direction
      !       ! external_field = Incid(sgg, 1, abs(direction), mtln_solver%time, i,j,k, still_planewave_time, .false.)
      !       ! mtln_solver%bundles(m)%external_field_segments(n)%external_field = external_field


      !       ! punt => mtln_solver%bundles(m)%external_field_segments(n)%Efield_wire2main
      !       current = mtln_solver%bundles(m)%i(1, n)
      !       dx = idxh(i)
      !       dy = idyh(j)
      !       dz = idzh(k)
      !       select case (abs(direction))  
      !       case(1)   
      !          cte=sgg%dt /eps0 /(dy*dz)
      !       case(2)     
      !          cte=sgg%dt /eps0 /(dx*dz)
      !       case(3)   
      !          cte=sgg%dt /eps0 /(dx*dy)
      !       end select
      !       ! punt = punt - sign(cte,real(direction,kind=rkind)) * current
      !       mtln_solver%bundles(m)%external_field_segments(n)%Efield_wire2main = &
      !          mtln_solver%bundles(m)%external_field_segments(n)%Efield_wire2main - cte * current
      !       mtln_solver%bundles(m)%external_field_segments(n)%Efield_main2wire = &
      !          mtln_solver%bundles(m)%external_field_segments(n)%Efield_main2wire - cte * current
      !       ! punt = punt - cte * current

      !       ! mtln_solver%bundles(m)%external_field_segments(n)%Efield_main2wire = punt
      !       ! Segmento => hwires%CurrentSegment(indexMap(m,n))
      !       ! Segmento%Efield_wire2main = &
      !       !    Segmento%Efield_wire2main - Segmento%cte5 * current
      !       ! Segmento%Efield_main2wire = &
      !       !    Segmento%Efield_main2wire - Segmento%cte5 * current
      !       ! hwires%CurrentSegment(indexMap(m,n))%Efield_main2wire = punt
      !       ! hwires%CurrentSegment(indexMap(m,n))%Current = current
      !       ! hwires%CurrentSegment(indexMap(m,n))%CurrentPast = current

      !       ! Segmento%Current = - sign(cte,real(direction,kind=rkind)) * current
      !       ! Segmento%CurrentPast = - sign(cte,real(direction,kind=rkind)) * current
      !       ! segmento%efield_wire2main=real(segmento%efield_wire2main,kind=rkind_wires) &
      !       !    - cte * segmento%current

      !       ! punt = cte * current
      !    end do
      ! end do

      ! do m = 1, mtln_solver%number_of_bundles
      !    do n = 1, ubound(mtln_solver%bundles(m)%external_field_segments,1)
      !       call readGridIndices(i, j, k, mtln_solver%bundles(m)%external_field_segments(n))                          

      !       direction = mtln_solver%bundles(m)%external_field_segments(n)%direction
      !       external_field = Incid(sgg, 1, abs(direction), mtln_solver%time, i,j,k, still_planewave_time, .false.)
      !       mtln_solver%bundles(m)%external_field_segments(n)%external_field = external_field


      !       ! punt => mtln_solver%bundles(m)%external_field_segments(n)%Efield_wire2main
      !       current = mtln_solver%bundles(m)%i(1, n)
      !       dx = idxh(i)
      !       dy = idyh(j)
      !       dz = idzh(k)
      !       select case (abs(direction))  
      !       case(1)   
      !          cte=sgg%dt /eps0 /(dy*dz)
      !       case(2)     
      !          cte=sgg%dt /eps0 /(dx*dz)
      !       case(3)   
      !          cte=sgg%dt /eps0 /(dx*dy)
      !       end select
      !       ! punt = punt - sign(cte,real(direction,kind=rkind)) * current
      !       ! punt = punt - cte * current

      !       ! mtln_solver%bundles(m)%external_field_segments(n)%Efield_main2wire = punt

      !       hwires%CurrentSegment(indexMap(m,n))%Efield_wire2main = &
      !          mtln_solver%bundles(m)%external_field_segments(n)%Efield_wire2main
      !       hwires%CurrentSegment(indexMap(m,n))%Efield_main2wire = &
      !          mtln_solver%bundles(m)%external_field_segments(n)%Efield_wire2main
      !       hwires%CurrentSegment(indexMap(m,n))%Current = current
      !       hwires%CurrentSegment(indexMap(m,n))%CurrentPast = current

      !       ! Segmento%Current = - sign(cte,real(direction,kind=rkind)) * current
      !       ! Segmento%CurrentPast = - sign(cte,real(direction,kind=rkind)) * current
      !       ! segmento%efield_wire2main=real(segmento%efield_wire2main,kind=rkind_wires) &
      !       !    - cte * segmento%current

      !       ! punt = cte * current
      !    end do
      ! end do

      ! call mtln_solver%step()

      ! do m = 1, mtln_solver%number_of_bundles
      !    do n = 1, ubound(mtln_solver%bundles(m)%external_field_segments,1)
      !       call readGridIndices(i, j, k, mtln_solver%bundles(m)%external_field_segments(n))                          
      !       current = -mtln_solver%bundles(m)%i(1, n)
      !       hwires%CurrentSegment(indexMap(m,n))%Current = current
      !       ! hwires%CurrentSegment(indexMap(m,n))%CurrentPast = current
      !    end do
      ! end do


   end subroutine AdvanceWiresE_mtln


   subroutine readGridIndices(i, j, k, field_segment)
      type(external_field_segment_t), intent(in) :: field_segment
      integer, intent(inout) :: i, j, k
      i = field_segment%position(1)
      j = field_segment%position(2)
      k = field_segment%position(3)
   end subroutine


end module Wire_bundles_mtln_mod
