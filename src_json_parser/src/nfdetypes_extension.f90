module NFDETypes_extension
   use NFDETypes

   implicit none

   public

   interface operator(==)
      module procedure NFDEGeneral_eq

      module procedure desplazamiento_eq

      module procedure frontera_eq
      module procedure fronteraPML_eq

      module procedure planewaves_eq
      module procedure planewave_eq

      module procedure sonda_eq
   end interface

contains
   subroutine initializeProblemDescription(pD)
      type(Parseador), intent(inout) :: pD

      allocate(pD%general)
      allocate(pD%matriz)
      allocate(pD%despl)
      allocate(pD%front)
      allocate(pD%Mats)
      allocate(pD%pecRegs)
      allocate(pD%pmcRegs)
      allocate(pD%DielRegs)
      allocate(pD%LossyThinSurfs)
      allocate(pD%frqDepMats)
      allocate(pD%aniMats)
      allocate(pD%nodSrc)
      allocate(pD%Sonda)
      allocate(pD%oldSONDA)
      allocate(pD%BloquePrb)
      allocate(pD%VolPrb)
      allocate(pD%tWires)
      allocate(pD%sWires)
      allocate(pD%tSlots)
      allocate(pD%boxSrc)
      allocate(pD%plnSrc)

   end subroutine

   logical function NFDEGeneral_eq(a, b) result (res)
      type(NFDEGeneral), intent(in) :: a, b
      res = .false.
      if (a%dt /= b%dt) return
      if (a%nmax /= b%nmax) return
      res = .true.
   end function

   logical function planewave_eq(a,b) result(res)
      type(PlaneWave), intent(in) :: a, b
      res = .false.
      if (a%nombre_fichero /= b%nombre_fichero) return
      if (a%atributo /= b%atributo) return
      if (any(a%coor1 /= b%coor1)) return
      if (any(a%coor2 /= b%coor2)) return
      if (a%theta /= b%theta) return
      if (a%phi /= b%phi) return
      if (a%alpha /= b%alpha) return
      if (a%beta /= b%beta) return
      if (a%isRC .neqv. b%isRC) return
      if (a%INCERTMAX /= b%INCERTMAX) return
      if (a%numModes /= b%numModes) return
      res = .true.
   end function

   logical function planewaves_eq(a,b) result(res)
      type(Planewaves), intent(in) :: a, b
      integer :: i
      res = .false.
      if (.not. associated(a%collection)) return
      if (.not. associated(b%collection)) return

      if (size(a%collection) /= size(b%collection)) return
      do i = 1, size(a%collection)
         if (.not. a%collection(i) == b%collection(i)) return
      end do

      if (a%nc /= b%nc) return
      if (a%nC_max /= b%nC_max) return
      res = .true.
   end function

   logical function FronteraPML_eq(a, b) result (res)
      type(FronteraPML), intent(in) :: a, b
      res = .false.
      if (a%orden    /= b%orden) return
      if (a%refl     /= b%refl) return
      if (a%numCapas /= b%numCapas) return
      res = .true.
   end function

   logical function frontera_eq(a, b) result (res)
      type(Frontera), intent(in) :: a, b
      integer :: i
      res = .false.
      if (any(a%tipoFrontera /= b%tipoFrontera)) return
      do i=1, size(a%propiedadesPML)
         if (.not. (a%propiedadesPML(i) == b%propiedadesPML(i))) return
      end do
      res = .true.
   end function

   logical function desplazamiento_eq(a, b) result (res)
      type(Desplazamiento), intent(in) :: a, b

      res = .false.

      if (.not. associated(a%desX)) return
      if (.not. associated(a%desY)) return
      if (.not. associated(a%desZ)) return
      if (.not. associated(b%desX)) return
      if (.not. associated(b%desY)) return
      if (.not. associated(b%desZ)) return

      if (any(a%desX /=  b%desX)) return
      if (any(a%desY /=  b%desY)) return
      if (any(a%desZ /=  b%desZ)) return

      if (a%nX /=  b%nX) return
      if (a%nY /=  b%nY) return
      if (a%nZ /=  b%nZ) return

      if (a%mx1 /=  b%mx1) return
      if (a%my1 /=  b%my1) return
      if (a%mz1 /=  b%mz1) return

      if (a%mx2 /=  b%mx2) return
      if (a%my2 /=  b%my2) return
      if (a%mz2 /=  b%mz2) return

      if (a%originx /=  b%originx) return
      if (a%originy /=  b%originy) return
      if (a%originz /=  b%originz) return

      res = .true.
   end function

   logical function sonda_eq(a, b) result (res)
      type(Sonda), intent(in) :: a, b
      res = .false.
      if (a%grname /= b%grname) return
      if (.not. associated(a%i) .or. .not. associated(b%i)) return
      if (.not. associated(a%j) .or. .not. associated(b%j)) return
      if (.not. associated(a%k) .or. .not. associated(b%k)) return
      if (.not. associated(a%node) .or. .not. associated(b%node)) return
      if (a%n_cord /= b%n_cord) return
      if (a%n_cord_max /= b%n_cord_max) return
      if (a%tstart /= b%tstart) return
      if (a%tstop /= b%tstop) return
      if (a%tstep /= b%tstep) return
      if (a%outputrequest /= b%outputrequest) return
      if (a%fstart     /= b%fstart) return
      if (a%fstop      /= b%fstop) return
      if (a%fstep      /= b%fstep) return
      if (a%phistart   /= b%phistart) return
      if (a%phistop    /= b%phistop) return
      if (a%phistep    /= b%phistep) return
      if (a%thetastart /= b%thetastart) return
      if (a%thetastop  /= b%thetastop) return
      if (a%thetastep  /= b%thetastep) return
      if (a%FileNormalize /= b%FileNormalize) return
      res = .true.
   end function
end module
