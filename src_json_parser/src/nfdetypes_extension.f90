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

      module procedure coords_eq
      module procedure abstractSonda_eq
      module procedure sonda_eq
      module procedure sondas_eq
      module procedure massonda_eq
      module procedure massondas_eq

      module procedure FarField_Sonda_eq
      module procedure Electric_Sonda_eq
      module procedure Magnetic_Sonda_eq
      module procedure NormalElectric_Sonda_eq
      module procedure NormalMagnetic_Sonda_eq
      module procedure SurfaceElectricCurrent_Sonda_eq
      module procedure SurfaceMagneticCurrent_Sonda_eq
      module procedure LossyThinSurface_eq
      module procedure LossyThinSurfaces_eq
      module procedure ThinWireComp_eq
      module procedure ThinWire_eq
      module procedure ThinWires_eq
      module procedure SlantedWireComp_eq
      module procedure SlantedWire_eq
      module procedure SlantedWires_eq
      module procedure ThinSlotComp_eq
      module procedure ThinSlot_eq
      module procedure ThinSlots_eq

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

   elemental logical function LossyThinSurface_eq(a, b)
      type(LossyThinSurface), intent(in) :: a, b

      LossyThinSurface_eq = all(a%c == b%c) .and. &
         all(a%sigma == b%sigma) .and. &
         all(a%eps == b%eps) .and. &
         all(a%mu == b%mu) .and. &
         all(a%sigmam == b%sigmam) .and. &
         all(a%thk == b%thk) .and. &
         all(a%sigma_devia == b%sigma_devia) .and. &
         all(a%eps_devia == b%eps_devia) .and. &
         all(a%mu_devia == b%mu_devia) .and. &
         all(a%sigmam_devia == b%sigmam_devia) .and. &
         all(a%thk_devia == b%thk_devia) .and. &
         (a%nc == b%nc) .and. &
         (a%files == b%files) .and. &
         (a%numcapas == b%numcapas)
   end function LossyThinSurface_eq

   elemental logical function LossyThinSurfaces_eq(a, b)
      type(LossyThinSurfaces), intent(in) :: a, b

      LossyThinSurfaces_eq = all(a%cs == b%cs) .and. &
         (a%length == b%length) .and. &
         (a%length_max == b%length_max) .and. &
         (a%nC_max == b%nC_max)
   end function LossyThinSurfaces_eq

   elemental logical function ThinWireComp_eq(a, b)
      type(ThinWireComp), intent(in) :: a, b

      ThinWireComp_eq = &
         (a%srctype == b%srctype) .and. &
         (a%srcfile == b%srcfile) .and. &
         (a%i == b%i) .and. &
         (a%j == b%j) .and. &
         (a%K == b%K) .and. &
         (a%nd == b%nd) .and. &
         (a%d == b%d) .and. &
         (a%m == b%m) .and. &
         (a%tag == b%tag)
   end function ThinWireComp_eq

   elemental logical function ThinWire_eq(a, b)
      type(ThinWire), intent(in) :: a, b

      ThinWire_eq = (a%rad == b%rad) .and. &
         (a%disp .eqv. b%disp) .and. &
         (a%dispfile == b%dispfile) .and. &
         (a%res == b%res) .and. &
         (a%ind == b%ind) .and. &
         (a%cap == b%cap) .and. &
         (a%P_res == b%P_res) .and. &
         (a%P_ind == b%P_ind) .and. &
         (a%P_cap == b%P_cap) .and. &
         (a%dispfile_LeftEnd == b%dispfile_LeftEnd) .and. &
         (a%R_LeftEnd == b%R_LeftEnd) .and. &
         (a%L_LeftEnd == b%L_LeftEnd) .and. &
         (a%C_LeftEnd == b%C_LeftEnd) .and. &
         (a%dispfile_RightEnd == b%dispfile_RightEnd) .and. &
         (a%R_RightEnd == b%R_RightEnd) .and. &
         (a%L_RightEnd == b%L_RightEnd) .and. &
         (a%C_RightEnd == b%C_RightEnd) .and. &
         (a%LeftEnd == b%LeftEnd) .and. &
         (a%RightEnd == b%RightEnd) .and. &
         (a%tl == b%tl) .and. &
         (a%tr == b%tr) .and. &
         (a%n_twc == b%n_twc) .and. &
         (a%n_twc_max == b%n_twc_max)
   end function ThinWire_eq

   elemental logical function ThinWires_eq(a, b)
      type(ThinWires), intent(in) :: a, b

      ThinWires_eq = all(a%tw == b%tw) .and. &
         (a%n_tw == b%n_tw) .and. &
         (a%n_tw_max == b%n_tw_max)
   end function ThinWires_eq

   elemental logical function SlantedWireComp_eq(a, b)
      type(SlantedWireComp), intent(in) :: a, b

      SlantedWireComp_eq = &
         (a%srctype == b%srctype) .and. &
         (a%srcfile == b%srcfile) .and. &
         (a%x == b%x) .and. &
         (a%y == b%y) .and. &
         (a%z == b%z) .and. &
         (a%nd == b%nd) .and. &
         (a%m == b%m) .and. &
         (a%tag == b%tag)
   end function SlantedWireComp_eq

   elemental logical function SlantedWire_eq(a, b)
      type(SlantedWire), intent(in) :: a, b

      SlantedWire_eq = &
         (a%rad == b%rad) .and. &
         (a%disp .eqv. b%disp) .and. &
         (a%dispfile == b%dispfile) .and. &
         (a%res == b%res) .and. &
         (a%ind == b%ind) .and. &
         (a%cap == b%cap) .and. &
         (a%P_res == b%P_res) .and. &
         (a%P_ind == b%P_ind) .and. &
         (a%P_cap == b%P_cap) .and. &
         (a%dispfile_LeftEnd == b%dispfile_LeftEnd) .and. &
         (a%R_LeftEnd == b%R_LeftEnd) .and. &
         (a%L_LeftEnd == b%L_LeftEnd) .and. &
         (a%C_LeftEnd == b%C_LeftEnd) .and. &
         (a%dispfile_RightEnd == b%dispfile_RightEnd) .and. &
         (a%R_RightEnd == b%R_RightEnd) .and. &
         (a%L_RightEnd == b%L_RightEnd) .and. &
         (a%C_RightEnd == b%C_RightEnd) .and. &
         (a%LeftEnd == b%LeftEnd) .and. &
         (a%RightEnd == b%RightEnd) .and. &
         (a%tl == b%tl) .and. &
         (a%tr == b%tr) .and. &
         (a%n_swc == b%n_swc) .and. &
         (a%n_swc_max == b%n_swc_max)
   end function SlantedWire_eq

   elemental logical function SlantedWires_eq(a, b)
      type(SlantedWires), intent(in) :: a, b

      SlantedWires_eq = all(a%sw == b%sw) .and. &
         (a%n_sw == b%n_sw) .and. &
         (a%n_sw_max == b%n_sw_max)
   end function SlantedWires_eq

   elemental logical function ThinSlotComp_eq(a, b)
      type(ThinSlotComp), intent(in) :: a, b

      ThinSlotComp_eq = (a%i == b%i) .and. &
         (a%j == b%j) .and. &
         (a%K == b%K) .and. &
         (a%node == b%node) .and. &
         (a%dir == b%dir) .and. &
         (a%Or == b%Or) .and. &
         (a%tag == b%tag)
   end function ThinSlotComp_eq

   elemental logical function ThinSlot_eq(a, b)
      type(ThinSlot), intent(in) :: a, b

      ThinSlot_eq = all(a%tgc == b%tgc) .and. &
         (a%width == b%width) .and. &
         (a%n_tgc == b%n_tgc) .and. &
         (a%n_tgc_max == b%n_tgc_max)
   end function ThinSlot_eq

   elemental logical function ThinSlots_eq(a, b)
      type(ThinSlots), intent(in) :: a, b

      ThinSlots_eq = all(a%tg == b%tg) .and. &
         (a%n_tg == b%n_tg) .and. &
         (a%n_tg_max == b%n_tg_max)
   end function ThinSlots_eq

   elemental logical function NFDEGeneral_eq(a, b) result (res)
      type(NFDEGeneral), intent(in) :: a, b
      res = .false.
      if (a%dt /= b%dt) return
      if (a%nmax /= b%nmax) return
      res = .true.
   end function

   elemental logical function planewave_eq(a,b) result(res)
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

   elemental logical function planewaves_eq(a,b) result(res)
      type(Planewaves), intent(in) :: a, b
      integer :: i
      res = .false.
      if (.not. associated(a%collection)) return
      if (.not. associated(b%collection)) return

      if (any(.not. a%collection == b%collection)) return

      if (a%nc /= b%nc) return
      if (a%nC_max /= b%nC_max) return
      res = .true.
   end function

   elemental logical function fronteraPML_eq(a, b) result (res)
      type(FronteraPML), intent(in) :: a, b
      res = .false.
      if (a%orden    /= b%orden) return
      if (a%refl     /= b%refl) return
      if (a%numCapas /= b%numCapas) return
      res = .true.
   end function

   elemental logical function frontera_eq(a, b) result (res)
      type(Frontera), intent(in) :: a, b
      integer :: i
      res = .false.
      if (any(a%tipoFrontera /= b%tipoFrontera)) return
      if (any(.not. a%propiedadesPML == b%propiedadesPML)) return
      res = .true.
   end function

   elemental logical function desplazamiento_eq(a, b) result (res)
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

   elemental logical function coords_eq(a, b) result(res)
      type(coords), intent(in) :: a, b
      res = .false.

      if (a%xi /= b%xi) return
      if (a%xe /= b%xe) return
      if (a%yi /= b%yi) return
      if (a%ye /= b%ye) return
      if (a%zi /= b%zi) return
      if (a%ze /= b%ze) return
      if (a%xtrancos /= b%xtrancos) return
      if (a%ytrancos /= b%ytrancos) return
      if (a%ztrancos /= b%ztrancos) return
      if (a%or /= b%or) return
      if (a%tag /= b%tag) return
      res = .true.
   end function

   elemental logical function FarField_Sonda_eq(a, b)
      type(FarField_Sonda), intent(in) :: a, b
      FarField_Sonda_eq = a%probe == b%probe
   end function FarField_Sonda_eq

   elemental logical function Electric_Sonda_eq(a, b)
      type(Electric_Sonda), intent(in) :: a, b
      Electric_Sonda_eq = a%probe == b%probe
   end function Electric_Sonda_eq

   elemental logical function Magnetic_Sonda_eq(a, b)
      type(Magnetic_Sonda), intent(in) :: a, b
      Magnetic_Sonda_eq = a%probe == b%probe
   end function Magnetic_Sonda_eq

   elemental logical function NormalElectric_Sonda_eq(a, b)
      type(NormalElectric_Sonda), intent(in) :: a, b
      NormalElectric_Sonda_eq = a%probe == b%probe
   end function NormalElectric_Sonda_eq

   elemental logical function NormalMagnetic_Sonda_eq(a, b)
      type(NormalMagnetic_Sonda), intent(in) :: a, b
      NormalMagnetic_Sonda_eq = a%probe == b%probe
   end function NormalMagnetic_Sonda_eq

   elemental logical function SurfaceElectricCurrent_Sonda_eq(a, b)
      type(SurfaceElectricCurrent_Sonda), intent(in) :: a, b
      SurfaceElectricCurrent_Sonda_eq = a%probe == b%probe
   end function SurfaceElectricCurrent_Sonda_eq

   elemental logical function SurfaceMagneticCurrent_Sonda_eq(a, b)
      type(SurfaceMagneticCurrent_Sonda), intent(in) :: a, b
      SurfaceMagneticCurrent_Sonda_eq = a%probe == b%probe
   end function SurfaceMagneticCurrent_Sonda_eq


   elemental logical function abstractSonda_eq(a, b) result(res)
      type(abstractSonda), intent(in) :: a, b
      integer(kind=4) :: i

      res = .false.

      if (a%n_FarField /= b%n_FarField) return
      if (a%n_Electric /= b%n_Electric) return
      if (a%n_Magnetic /= b%n_Magnetic) return
      if (a%n_NormalElectric /= b%n_NormalElectric) return
      if (a%n_NormalMagnetic /= b%n_NormalMagnetic) return
      if (a%n_SurfaceElectricCurrent /= b%n_SurfaceElectricCurrent) return
      if (a%n_SurfaceMagneticCurrent /= b%n_SurfaceMagneticCurrent) return

      if (.not. associated(a%FarField)               .or. .not. associated(b%FarField)) return
      if (.not. associated(a%Electric)               .or. .not. associated(b%Electric)) return
      if (.not. associated(a%Magnetic)               .or. .not. associated(b%Magnetic)) return
      if (.not. associated(a%NormalElectric)         .or. .not. associated(b%NormalElectric)) return
      if (.not. associated(a%NormalMagnetic)         .or. .not. associated(b%NormalMagnetic)) return
      if (.not. associated(a%SurfaceElectricCurrent) .or. .not. associated(b%SurfaceElectricCurrent)) return
      if (.not. associated(a%SurfaceMagneticCurrent) .or. .not. associated(b%SurfaceMagneticCurrent)) return

      if (any(.not. a%FarField == b%FarField)) return
      if (any(.not. a%Electric == b%Electric)) return
      if (any(.not. a%Magnetic == b%Magnetic)) return
      if (any(.not. a%NormalElectric == b%NormalElectric)) return
      if (any(.not. a%NormalMagnetic == b%NormalMagnetic)) return
      if (any(.not. a%SurfaceElectricCurrent == b%SurfaceElectricCurrent)) return
      if (any(.not. a%SurfaceMagneticCurrent == b%SurfaceMagneticCurrent)) return

      res = .true.
   end function abstractSonda_eq

   elemental logical function sondas_eq(a, b) result(res)
      type(Sondas), intent(in) :: a, b
      integer :: i

      res = .false.

      if (a%n_probes /= b%n_probes) return
      if (a%n_probes_max /= b%n_probes_max) return

      if (.not. associated(a%probes) .or. .not. associated(b%probes)) return
      if (any(.not. a%probes == b%probes)) return

      res = .true.
   end function sondas_eq

   elemental logical function sonda_eq(a, b) result (res)
      type(Sonda), intent(in) :: a, b
      res = .false.

      if (a%grname /= b%grname) return

      if (.not. associated(a%i) .or. .not. associated(b%i)) return
      if (.not. associated(a%j) .or. .not. associated(b%j)) return
      if (.not. associated(a%k) .or. .not. associated(b%k)) return
      if (.not. associated(a%node) .or. .not. associated(b%node)) return

      if (any(a%i /= b%i)) return
      if (any(a%j /= b%j)) return
      if (any(a%k /= b%k)) return
      if (any(a%node /= b%node)) return

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

   elemental logical function masSonda_eq(a, b) result (res)
      type(MasSonda), intent(in) :: a, b
      integer :: i
      res = .false.

      if (a%filename /= b%filename) return
      if (a%type1 /= b%type1) return
      if (a%type2 /= b%type2) return
      if (a%outputrequest /= b%outputrequest) return

      if (.not. associated(a%cordinates) .or. .not. associated(b%cordinates)) return
      if (all(.not. a%cordinates == b%cordinates)) return

      if (a%tstart /= b%tstart) return
      if (a%tstop /= b%tstop) return
      if (a%tstep /= b%tstep) return
      if (a%fstart /= b%fstart) return
      if (a%fstop/= b%fstop) return
      if (a%fstep /= b%fstep) return
      res = .true.
   end function

   logical function MasSondas_eq(a, b)
      type(MasSondas), intent(in) :: a, b
      integer(kind=4) :: i

      MasSondas_eq = .false.

      if (a%length /= b%length) return
      if (a%length_max /= b%length_max) return
      if (a%len_cor_max /= b%len_cor_max) return

      if (.not. associated(a%collection) .or. .not. associated(b%collection)) return
      if (any(.not. a%collection == b%collection)) return

      MasSondas_eq = .true.
   end function MasSondas_eq
end module
