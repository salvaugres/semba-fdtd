module smbjson

#ifdef CompileWithJSON
   use NFDETypes

   use NFDETypes_extension
   use labels_mod
   use mesh_mod
   use parser_tools_mod
   use idchildtable_mod

   use json_module
   use json_kinds

   use, intrinsic :: iso_fortran_env , only: error_unit

   implicit none

   character (len=*), parameter :: SMBJSON_NO_TAG = "X"

   type, public :: parser_t
      private
      character (len=:), allocatable :: filename
      type(json_file), pointer :: jsonfile => null()
      type(json_core), pointer :: core => null()
      type(json_value), pointer :: root => null()
      type(mesh_t) :: mesh
      type(IdChildTable_t) :: matTable
   contains
      procedure :: readProblemDescription

      ! private
      procedure :: readGeneral
      procedure :: readGrid
      procedure :: readMediaMatrix
      procedure :: readPECRegions
      procedure :: readPMCRegions
      procedure :: readBoundary
      procedure :: readPlanewaves
      procedure :: readNodalSources
      procedure :: readProbes
      procedure :: readMoreProbes
      procedure :: readBlockProbes
      procedure :: readThinWires
      procedure :: readSlantedWires
      procedure :: readThinSlots
      !
      procedure :: readMesh
      !
      procedure :: getIntAt
      procedure :: getIntsAt
      procedure :: getRealAt
      procedure :: getStrAt
      procedure :: getJSONValuePtrAt
      procedure :: existsAt
      procedure :: getCellRegionsWithMaterialType
      procedure :: getDomain
   end type
   interface parser_t
      module procedure parser_ctor
   end interface

   type, private :: termination_t
      integer :: connectorType
      real :: r, l, c
   end type

   type, private :: domain_t
      real :: tstart, tstop, tstep
      real :: fstart, fstop, fstep
      character(len=:), allocatable :: filename
      integer :: type1, type2
   end type
contains
   function parser_ctor(filename) result(res)
      type(parser_t) :: res
      character(len=*), intent(in) :: filename
      res%filename = filename
   end function

   function readProblemDescription(this) result (res)
      class(parser_t) :: this
      type(Parseador) :: res !! Problem Description

      allocate(this%jsonfile)
      call this%jsonfile%initialize()
      if (this%jsonfile%failed()) then
         call this%jsonfile%print_error_message(error_unit)
         return
      end if

      call this%jsonfile%load(filename = this%filename)
      if (this%jsonfile%failed()) then
         call this%jsonfile%print_error_message(error_unit)
         return
      end if

      allocate(this%core)
      call this%jsonfile%get_core(this%core)
      call this%jsonfile%get('.', this%root)

      this%mesh = this%readMesh()
      this%matTable = IdChildTable_t(this%core, this%root, J_MATERIALS)

      call initializeProblemDescription(res)

      ! Basics
      res%general = this%readGeneral()
      res%matriz = this%readMediaMatrix()
      res%despl = this%readGrid()
      res%front = this%readBoundary()

      ! Materials
      res%pecRegs = this%readPECRegions()
      res%pmcRegs = this%readPMCRegions()
      ! res%DielRegs = this%readDielectricRegions()
      ! res%LossyThinSurfs = this%readLossyThinSurfaces()
      ! res%frqDepMats = this%readFrequencyDependentMaterials()
      ! res%aniMats = this%readAnisotropicMaterials()

      ! Sources
      ! res%boxSrc = this%readBoxSources()
      res%plnSrc = this%readPlanewaves()
      res%nodSrc = this%readNodalSources()
      ! Probes
      res%oldSonda = this%readProbes()
      res%sonda = this%readMoreProbes()
      res%BloquePrb = this%readBlockProbes()
      ! res%VolPrb = this%readVolumicProbes()
      ! Thin elements
      res%tWires = this%readThinWires()
      res%sWires = this%readSlantedWires()
      res%tSlots = this%readThinSlots()

      ! Cleanup
      call this%core%destroy()
      call this%jsonfile%destroy()
      nullify(this%root)


   end function

   function readMesh(this) result(res)
      class(parser_t) :: this
      type(Mesh_t) :: res
      type(json_value), pointer :: jcs, jc
      integer :: id, i
      real, dimension(:), allocatable :: pos
      type(coordinate_t) :: c
      logical :: found

      call this%core%get(this%root, J_MESH//'.'//J_COORDINATES, jcs, found=found)
      if (found) then
         do i = 1, this%core%count(jcs)
            call this%core%get_child(jcs, i, jc)
            call this%core%get(jc, J_ID, id)
            call this%core%get(jc, J_COORDINATE_POS, pos)
            c%position = pos
            call res%addCoordinate(id, c)
         end do
      end if
      call addElements(res)

   contains
      subroutine addElements(mesh)
         type(mesh_t), intent(inout) :: mesh
         character (len=:), allocatable :: elementType
         type(json_value), pointer :: jes, je
         integer :: id, i
         type(node_t) :: e
         integer, dimension(:), allocatable :: coordIds
         logical :: found
         call this%core%get(this%root, J_MESH//'.'//J_ELEMENTS, jes, found=found)
         if (found) then
            do i = 1, this%core%count(jes)
               call this%core%get_child(jes, i, je)
               call this%core%get(je, J_ID, id)
               call this%core%get(je, J_TYPE, elementType)
               select case (elementType)
                case (J_ELEM_TYPE_NODE)
                  call this%core%get(je, J_COORDINATE_IDS, coordIds)
                  call mesh%addElement(id, node_t(coordIds))
                case (J_ELEM_TYPE_POLYLINE)
                  call this%core%get(je, J_COORDINATE_IDS, coordIds)
                  call mesh%addElement(id, polyline_t(coordIds))
                CASE (J_ELEM_TYPE_CELL_REGION)
                  block
                     type(cell_region_t) :: cR
                     type(cell_interval_t), dimension(:), allocatable :: intervals
                     cR%intervals = readCellIntervals(je, J_CELL_INTERVALS)
                     call mesh%addCellRegion(id, cR)
                  end block
                case default
                  write (error_unit, *) 'Invalid element type'
               end select
            end do
         end if
      end subroutine

      function readCellIntervals(place, path) result(res)
         type(json_value), pointer, intent(in) :: place
         character (len=*), intent(in) :: path
         type(cell_interval_t), dimension(:), allocatable :: res

         type(json_value), pointer :: intervalsPlace, interval
         integer :: i, nIntervals
         real, dimension(:), allocatable :: cellIni, cellEnd
         logical :: containsInterval

         call this%core%get(place, path, intervalsPlace, found=containsInterval)
         if (.not. containsInterval) then
            allocate(res(0))
            return
         end if
         nIntervals = this%core%count(intervalsPlace)
         allocate(res(nIntervals))
         do i = 1, nIntervals
            call this%core%get_child(intervalsPlace, i, interval)
            call this%core%get(interval, '(1)', cellIni)
            call this%core%get(interval, '(2)', cellEnd)
            res(i)%ini%cell = cellIni(1:3)
            res(i)%end%cell = cellEnd(1:3)
         end do
      end function
   end function

   function readGeneral(this) result (res)
      class(parser_t) :: this
      type(NFDEGeneral) :: res
      res%dt = this%getRealAt(this%root, J_GENERAL//'.'//J_TIME_STEP)
      res%nmax = this%getRealAt(this%root, J_GENERAL//'.'//J_NUMBER_OF_STEPS)
   end function

   function readMediaMatrix(this) result(res)
      class(parser_t) :: this
      type(MatrizMedios) :: res
      character (len=*), parameter :: P = J_MESH//'.'//J_GRID//'.'//J_NUMBER_OF_CELLS
      res%totalX = this%getIntAt(this%root, P//'(1)')
      res%totalY = this%getIntAt(this%root, P//'(2)')
      res%totalZ = this%getIntAt(this%root, P//'(3)')
   end function

   function readGrid(this) result (res)
      class(parser_t) :: this
      type(Desplazamiento) :: res
      real, dimension(:), allocatable :: vec
      character (len=*), parameter :: P = J_MESH//'.'//J_GRID

      res%nX = this%getIntAt(this%root, P//'.'//J_NUMBER_OF_CELLS//'(1)')
      res%nY = this%getIntAt(this%root, P//'.'//J_NUMBER_OF_CELLS//'(2)')
      res%nZ = this%getIntAt(this%root, P//'.'//J_NUMBER_OF_CELLS//'(3)')

      call assignDes(P//'.'//J_STEPS//'.x', res%desX, res%nX)
      call assignDes(P//'.'//J_STEPS//'.y', res%desY, res%nY)
      call assignDes(P//'.'//J_STEPS//'.z', res%desZ, res%nZ)

      res%mx1 = 0
      res%my1 = 0
      res%mz1 = 0
      res%mx2 = res%nX
      res%my2 = res%nY
      res%mz2 = res%nZ


   contains
      subroutine assignDes(path, dest, numberOfCells)
         character(kind=CK, len=*) :: path
         real, dimension(:), pointer :: dest
         real, dimension(:), allocatable :: vec
         integer, intent(in) :: numberOfCells
         logical :: found = .false.

         call this%core%get(this%root, path, vec, found)
         if (.not. found) then
            write(error_unit, *) 'Error reading grid: steps not found.'
         endif
         if (size(vec) /= 1 .and. size(vec) /= numberOfCells) then
            write(error_unit, *) 'Error reading grid: steps must be of size 1 (for regulr grids) or equal to the number of cells.'
         end if

         allocate(dest(0 : numberOfCells-1))
         if (size(vec) == 1) then
            dest(:) = vec(1)
         else
            dest = vec
         end if
      end subroutine
   end function

   function readBoundary(this) result (res)
      class(parser_t) :: this
      type(Frontera) :: res
      character(kind=json_CK,len=:), allocatable :: boundaryTypeLabel
      logical(LK) :: allLabelFound = .false.

      call this%core%get(this%root, J_BOUNDARY//'.'//J_BND_ALL//'.'//J_BOUNDARY_TYPE,  boundaryTypeLabel, allLabelFound)
      if (allLabelFound) then
         res%tipoFrontera(:) = labelToBoundaryType(boundaryTypeLabel)
         if (all(res%tipoFrontera == F_PML)) then
            res%propiedadesPML(:) = readPMLProperties(J_BOUNDARY//"."//J_BND_ALL)
         end if
         return
      else
         ! TODO Check every bound.
      end if
   contains
      function readPMLProperties(p) result(res)
         type(FronteraPML) :: res
         character(len=*), intent(in) :: p
         call this%core%get(this%root, p//'.'//J_BND_PML_LAYERS,     res%numCapas, default=8)
         call this%core%get(this%root, p//'.'//J_BND_PML_ORDER,      res%orden,    default=2.0)
         call this%core%get(this%root, p//'.'//J_BND_PML_REFLECTION, res%refl,     default=0.001)
      end function

      function labelToBoundaryType(str) result (type)
         character(len=:), allocatable :: str
         integer :: type
         select case (str)
          case (J_BND_PEC)
            type = F_PEC
          case (J_BND_PMC)
            type = F_PMC
          case (J_BND_PERIODIC)
            type = F_PER
          case (J_BND_MUR)
            type = F_MUR
          case (J_BND_PML)
            type = F_PML
         end select
      end function
   end function

   function readPECRegions(this) result (res)
      class(parser_t), intent(in) :: this
      type(PECRegions) :: res
      type(cell_region_t), dimension(:), allocatable :: cRs
      cRs = this%getCellRegionsWithMaterialType(J_MAT_TYPE_PEC)
      res = buildPECPMCRegion(cRs)
   end function

   function readPMCRegions(this) result (res)
      class(parser_t), intent(in) :: this
      type(PECRegions) :: res
      type(cell_region_t), dimension(:), allocatable :: cRs
      cRs = this%getCellRegionsWithMaterialType(J_MAT_TYPE_PMC)
      res = buildPECPMCRegion(cRs)
   end function

   function buildPECPMCRegion(cRs) result(res)
      type(PECRegions) :: res
      type(cell_region_t), dimension(:), allocatable, intent(in) :: cRs
      call addCellRegionsAsCoords(res%Lins, cRs, CELL_TYPE_LINEL)
      call addCellRegionsAsCoords(res%Surfs, cRs, CELL_TYPE_SURFEL)
      call addCellRegionsAsCoords(res%Vols, cRs, CELL_TYPE_VOXEL)
      res%nLins = size(res%lins)
      res%nSurfs = size(res%surfs)
      res%nVols = size(res%vols)
      res%nLins_max = size(res%Lins)
      res%nSurfs_max = size(res%Surfs)
      res%nVols_max = size(res%Vols)
   end function

   function readDielectricRegions() result (res)
      type(DielectricRegions) :: res
      ! TODO
   end function

   function readLossyThinSurfaces() result (res)
      type(LossyThinSurfaces) :: res
      ! TODO
   end function

   function readFrequencyDependentMaterials() result (res)
      type(FreqDepenMaterials) :: res


      ! TODO
   end function

   function readAnisotropicMaterials() result (res)
      type(ANISOTROPICelements_t) :: res


      ! TODO
   end function

   function readBoxSources() result (res)
      type(Boxes) :: res


      ! TODO
   end function

   function readPlanewaves(this) result (res)
      class(parser_t) :: this
      type(PlaneWaves) :: res
      type(json_value), pointer :: sources
      type(json_value_ptr), allocatable :: pws(:)
      integer :: i
      logical :: found

      call this%core%get(this%root, J_SOURCES, sources, found)
      if (found) then
         pws = jsonValueFilterByKeyValue(this%core, sources, J_TYPE, J_SRC_PW_TYPE)
      else
         allocate(pws(0))
      end if
      allocate(res%collection(size(pws)))
      do i=1, size(pws)
         res%collection(i) = readPlanewave(pws(i)%p)
      end do
      res%nc = size(res%collection)
      res%nc_max = size(res%collection)

   contains
      function readPlanewave(pw) result (res)
         type(PlaneWave) :: res
         type(json_value), pointer :: pw

         type(cell_region_t) :: cellRegion
         type(cell_interval_t), dimension(:), allocatable :: voxelIntervals
         character (len=:), allocatable :: label
         integer, dimension(:), allocatable :: elemIds
         logical :: found
         type(coords), dimension(:), allocatable :: nfdeCoords

         res%nombre_fichero = trim(adjustl( &
            this%getStrAt(pw,J_SRC_MAGNITUDE_FILE)))

         call this%core%get(pw, J_SRC_PW_ATTRIBUTE, label, found)
         if (found) then
            res%atributo = trim(adjustl(label))
         else
            res%atributo = ""
         endif

         call this%core%get(pw, J_SRC_PW_DIRECTION//'.'//J_SRC_PW_DIRECTION_THETA, res%theta)
         call this%core%get(pw, J_SRC_PW_DIRECTION//'.'//J_SRC_PW_DIRECTION_PHI, res%phi)
         call this%core%get(pw, J_SRC_PW_POLARIZATION//'.'//J_SRC_PW_POLARIZATION_ALPHA, res%alpha)
         call this%core%get(pw, J_SRC_PW_POLARIZATION//'.'//J_SRC_PW_POLARIZATION_BETA, res%beta)

         call this%core%get(pw, J_ELEMENTIDS, elemIds)
         if (size(elemIds) /= 1) write(error_unit, *) "Planewave must contain a single elementId."
         cellRegion = this%mesh%getCellRegion(elemIds(1), found)
         if (.not. found) write(error_unit, *) "Planewave elementId not found."
         voxelIntervals = cellRegion%getIntervalsOfType(CELL_TYPE_VOXEL)
         if (size(voxelIntervals) /= 1) write(error_unit, *) "Planewave must contain a single voxel region."

         nfdeCoords = cellIntervalsToCoords(voxelIntervals)
         res%coor1 = [nfdeCoords(1)%Xi, nfdeCoords(1)%Yi, nfdeCoords(1)%Zi]
         res%coor2 = [nfdeCoords(1)%Xe, nfdeCoords(1)%Ye, nfdeCoords(1)%Ze]

         res%isRC = .false.
         res%nummodes = 1
         res%incertmax = 0.0
      end function
   end function

   function readNodalSources(this) result (res)
      type(NodSource) :: res
      class(parser_t) :: this
      type(json_value), pointer :: sources
      type(json_value_ptr), dimension(:), allocatable :: nodSrcs
      logical :: found
      integer :: i

      call this%core%get(this%root, J_SOURCES, sources, found)
      if (.not. found) then
         allocate(res%NodalSource(0))
         return
      end if

      nodSrcs = jsonValueFilterByKeyValues(this%core, sources, J_TYPE, [J_SRC_NS_TYPE])
      if (size(nodSrcs) == 0) then
         allocate(res%NodalSource(0))
         return
      end if

      allocate(res%NodalSource(size(nodSrcs)))
      res%n_nodSrc = size(nodSrcs)
      res%n_nodSrc_max = size(nodSrcs)
      do i = 1, size(nodSrcs)
         res%NodalSource(i) = readCurrentFieldSource(nodSrcs(i)%p)
      end do
      do i = 1, size(nodSrcs)
         res%n_C2P_max = max(res%n_C2P_max, res%NodalSource(i)%n_C2P)
      end do

   contains
      function readCurrentFieldSource(jns) result(res)
         type(Curr_Field_Src) :: res
         type(json_value), pointer :: jns, entry

         select case (this%getStrAt(jns, J_SRC_NS_FIELD))
          case (J_SRC_NS_FIELD_CURRENT)
            res%isField = .false.
            res%isElec = .false.
            res%isMagnet = .false.
            res%isCurrent = .true.
          case default
            write(error_unit, *) 'Error reading current field source. Field label not recognized.'
         end select
         res%isInitialValue = .false.
         res%nombre = trim(adjustl(this%getStrAt(jns, J_SRC_MAGNITUDE_FILE)))

         allocate(res%c1P(0))
         res%n_C1P = 0

         call addCellRegionsAsScaledCoords(res%c2P, &
            this%mesh%getCellRegions(&
            this%getIntsAt(jns, J_ELEMENTIDS)), CELL_TYPE_LINEL)

         res%n_C2P = size(res%C2p)

      end function
   end function

   function readProbes(this) result (res)
      class(parser_t) :: this
      type(Sondas) :: res
      type(json_value), pointer :: probes
      type(json_value_ptr), dimension(:), allocatable :: ps
      integer :: i
      character (len=*), dimension(1), parameter :: validTypes = &
         (/J_PR_TYPE_FARFIELD/)

      call this%core%get(this%root, J_PROBES, probes)
      ps = jsonValueFilterByKeyValues(this%core, probes, J_TYPE, validTypes)
      allocate(res%probes(size(ps)))
      do i=1, size(ps)
         res%probes(i) = readProbe(ps(i)%p)
      end do

      res%n_probes = size(res%probes)
      res%n_probes_max = size(res%probes)
   contains
      function readProbe(p) result (res)
         type(abstractSonda) :: res
         type(json_value), pointer :: p

         write(error_unit, *) 'read abstract sonda is TBD. TODO'

      end function
   end function

   function readMoreProbes(this) result (res)
      class(parser_t) :: this
      type(MasSondas) :: res
      type(json_value), pointer :: probes
      type(json_value_ptr), allocatable :: ps(:)
      integer :: i
      character (len=*), dimension(4), parameter :: validTypes = &
         [J_PR_TYPE_ELECTRIC, J_PR_TYPE_MAGNETIC, J_PR_TYPE_CURRENT, J_PR_TYPE_VOLTAGE]

      call this%core%get(this%root, J_PROBES, probes)
      ps = jsonValueFilterByKeyValues(this%core, probes, J_TYPE, validTypes)
      allocate(res%collection(size(ps)))
      do i=1, size(ps)
         res%collection(i) = readProbe(ps(i)%p)
      end do

      res%length = size(res%collection)
      res%length_max = size(res%collection)
      res%len_cor_max = 0
   contains
      function readProbe(p) result (res)
         type(MasSonda) :: res
         type(json_value), pointer :: p
         integer :: i, j, k
         character (len=:), allocatable :: typeLabel, outputName
         character(kind=CK,len=1), dimension(:), allocatable :: dirLabels
         type(pixel_t), dimension(:), allocatable :: pixels

         integer, dimension(:), allocatable :: elemIds
         logical :: elementIdsFound

         call this%core%get(p, J_PR_OUTPUT_NAME, outputName)
         res%outputrequest = trim(adjustl(outputName))

         call this%core%get(p, J_TYPE, typeLabel)

         call setDomain(res, this%getDomain(p, J_PR_DOMAIN))

         call this%core%get(p, J_ELEMENTIDS, elemIds, found=elementIdsFound)
         pixels = getPixelsFromElementIds(this%mesh, elemIds)

         if (typeLabel == J_PR_TYPE_CURRENT .or. typeLabel == J_PR_TYPE_VOLTAGE) then
            allocate(res%cordinates(size(pixels)))
            do i = 1, size(pixels)
               res%cordinates(i)%tag = pixels(i)%tag
               read(pixels(i)%tag,*) res%cordinates(i)%Xi
               res%cordinates(i)%Yi = 0
               res%cordinates(i)%Zi = 0
               res%cordinates(i)%Or = strToProbeType(typeLabel)
            end do
         else
            call this%core%get(p, J_PR_DIRECTIONS, dirLabels)
            allocate(res%cordinates(size(pixels) * size(dirLabels)))
            do i = 1, size(pixels)
               k = (i-1) * size(dirLabels)
               do j = 1, size(dirLabels)
                  res%cordinates(k+j)%tag = pixels(i)%tag
                  res%cordinates(k+j)%Xi = int (pixels(i)%cell(1))
                  res%cordinates(k+j)%Yi = int (pixels(i)%cell(2))
                  res%cordinates(k+j)%Zi = int (pixels(i)%cell(3))
                  res%cordinates(k+j)%Or = strToProbeType(typeLabel, dirLabels(j))
               end do
            end do
         end if

         res%len_cor = size(res%cordinates)
      end function

      subroutine setDomain(res, domain)
         type(MasSonda), intent(inout) :: res
         type(domain_t), intent(in) :: domain

         res%tstart = domain%tstart
         res%tstep = domain%tstep
         res%tstop = domain%tstop
         res%fstart = domain%fstart
         res%fstep = domain%fstep
         res%fstop = domain%fstop
         res%filename = domain%filename
         res%type1 = domain%type1
         res%type2 = domain%type2
      end subroutine

      function strToProbeType(typeLabel, dirLabel) result(res)
         integer (kind=4) :: res
         character (len=:), allocatable :: typeLabel
         character (len=1), optional :: dirLabel
         select case (typeLabel)
          case (J_PR_TYPE_ELECTRIC)
            if (.not. present(dirLabel)) then
               write(error_unit, *) "Dir label must be present"
            end if
            select case (dirLabel)
             case (J_DIR_X)
               res = NP_COR_EX
             case (J_DIR_Y)
               res = NP_COR_EY
             case (J_DIR_Z)
               res = NP_COR_EZ
            end select
          case (J_PR_TYPE_MAGNETIC)
            if (.not. present(dirLabel)) then
               write(error_unit, *) "Dir label must be present"
            end if
            select case (dirLabel)
             case (J_DIR_X)
               res = NP_COR_HX
             case (J_DIR_Y)
               res = NP_COR_HY
             case (J_DIR_Z)
               res = NP_COR_HZ
            end select
          case (J_PR_TYPE_CURRENT)
            res = NP_COR_WIRECURRENT
          case (J_PR_TYPE_VOLTAGE)
            res = NP_COR_DDP
         end select
      end function
   end function

   function readBlockProbes(this) result (res)
      class(parser_t) :: this
      type(BloqueProbes) :: res
      type(json_value_ptr), dimension(:), allocatable :: bps
      type(json_value), pointer :: probes
      logical :: found
      integer :: i

      call this%core%get(this%root, J_PROBES, probes, found)
      if (.not. found) then
         allocate(res%bp(0))
         return
      end if

      bps = jsonValueFilterByKeyValues(this%core, probes, J_TYPE, [J_PR_TYPE_BULK_CURRENT])
      if (size(bps) == 0) then
         allocate(res%bp(0))
         return
      end if

      res%n_bp = size(bps)
      res%n_bp_max = size(bps)
      allocate(res%bp(size(bps)))
      do i = 1, size(bps)
         res%bp(i) = readBlockProbe(bps(i)%p)
      end do
   contains
      function readBlockProbe(bp) result(res)
         type(BloqueProbe) :: res
         type(json_value), pointer :: bp
         type(coords), dimension(:), allocatable :: cs

         call setDomain(res, this%getDomain(bp, J_PR_DOMAIN))

         block
            type(cell_region_t), dimension(:), allocatable :: cRs

            cRs = this%mesh%getCellRegions(this%getIntsAt(bp, J_ELEMENTIDS))
            if (size(cRs) /= 1) write(error_unit, *) "Block probe must be defined by a single cell region."

            if (size(cRs(1)%intervals) /= 1) write(error_unit, *) "Block probe must be defined by a single cell interval."
            cs = cellIntervalsToCoords(cRs(1)%intervals)

            res%i1  = cs(1)%xi
            res%i2  = cs(1)%xe
            res%j1  = cs(1)%yi
            res%j2  = cs(1)%ye
            res%k1  = cs(1)%zi
            res%k2  = cs(1)%ze
            res%nml = cs(1)%Or
         end block

         res%outputrequest = trim(adjustl( &
            this%getStrAt(bp, J_PR_OUTPUT_NAME)))
         res%skip = 1
         res%tag = ''
         res%t = BcELECT

      end function

      subroutine setDomain(res, domain)
         type(BloqueProbe), intent(inout) :: res
         type(domain_t), intent(in) :: domain

         res%tstart = domain%tstart
         res%tstep = domain%tstep
         res%tstop = domain%tstop
         res%fstart = domain%fstart
         res%fstep = domain%fstep
         res%fstop = domain%fstop
         res%FileNormalize = domain%filename
         res%type2 = domain%type2
      end subroutine
   end function

   function readVolumicProbes() result (res)
      type(VolProbes) :: res
      ! TODO
   end function

   function readThinWires(this) result (res)
      class(parser_t) :: this
      type(ThinWires) :: res
      type(json_value), pointer :: cables, cable
      integer :: i, j
      logical :: cablesFound

      call this%core%get(this%root, J_CABLES, cables, found=cablesFound)
      if (.not. cablesFound) then
         allocate(res%tw(0))
         res%n_tw = 0
         res%n_tw_max = 0
         return
      end if

      ! Allocates thin wires.
      block
         integer :: nTw = 0
         do i = 1, this%core%count(cables)
            call this%core%get_child(cables, i, cable)
            if (isThinWire(cable)) nTw = nTw+1
         end do

         allocate(res%tw(nTw))
         res%n_tw = size(res%tw)
         res%n_tw_max = size(res%tw)
      end block

      j = 1
      do i = 1, this%core%count(cables)
         call this%core%get_child(cables, i, cable)
         if (isThinWire(cable)) then
            res%tw(j) = readThinWire(cable)
            j = j+1
         end if
      end do

   contains
      function readThinWire(cable) result(res)
         type(ThinWire) :: res
         type(json_value), pointer, intent(in) :: cable

         character (len=:), allocatable :: entry
         type(json_value), pointer :: je, je2
         integer :: i
         block
            type(json_value_ptr) :: mat
            mat = this%matTable%getId(this%getIntAt(cable, J_CAB_MAT_ID))
            select case (this%getStrAt(mat%p, J_TYPE))
             case (J_MAT_TYPE_WIRE)
               call this%core%get(mat%p,J_MAT_WIRE_RADIUS, res%rad, default = 0.0)
               call this%core%get(mat%p,J_MAT_WIRE_RESISTANCE, res%res, default = 0.0)
               call this%core%get(mat%p,J_MAT_WIRE_INDUCTANCE, res%ind, default = 0.0)
               res%dispfile = trim(adjustl(" "))
            end select
         end block

         block
            type(json_value_ptr) :: mat
            type(termination_t) :: conn
            character (len=:), allocatable :: label
            mat = this%matTable%getId(this%getIntAt(cable, J_CAB_INI_CONN_ID))
            conn = readWireTerminalMaterial(mat%p)
            res%tl = conn%connectorType
            res%R_LeftEnd = conn%r
            res%L_LeftEnd = conn%l
            res%C_LeftEnd = conn%c
            res%dispfile_LeftEnd = trim(adjustl(" "))
         end block

         block
            type(json_value_ptr) :: mat
            type(termination_t) :: conn
            mat = this%matTable%getId(this%getIntAt(cable, J_CAB_END_CONN_ID))
            conn = readWireTerminalMaterial(mat%p)
            res%tr = conn%connectorType
            res%R_RightEnd = conn%r
            res%L_RightEnd = conn%l
            res%C_RightEnd = conn%c
            res%dispfile_RightEnd = trim(adjustl(" "))
         end block

         block
            type(linel_t), dimension(:), allocatable :: linels
            integer :: i

            integer, dimension(:), allocatable :: elementIds
            type(polyline_t) :: polyline
            call this%core%get(cable, J_ELEMENTIDS, elementIds)
            if (size(elementIds) /= 1) then
               write(error_unit, *) "Thin wires must be defined by a single polyline element."
            end if
            polyline = this%mesh%getPolyline(elementIds(1))
            linels = convertPolylineToLinels(this%mesh, polyline)
           
            res%n_twc = size(linels)
            res%n_twc_max = size(linels)
            allocate(res%twc(size(linels)))
            do i = 1, size(linels)
               res%twc(i)%srcfile = 'None'
               res%twc(i)%srctype = 'None'
               res%twc(i)%i = linels(i)%cell(1)
               res%twc(i)%j = linels(i)%cell(2)
               res%twc(i)%k = linels(i)%cell(3)
               if (len_trim( adjustl( linels(i)%tag ) ) == 0) then
                  res%twc(i)%tag = SMBJSON_NO_TAG
               else 
                  res%twc(i)%tag = linels(i)%tag
               end if
            end do
         end block
      end function

      function readWireTerminalMaterial(mat) result(res)
         type(termination_t) :: res
         type(json_value), pointer :: mat
         character (len=:), allocatable :: label
         logical :: found
         label = this%getStrAt(&
            mat, J_MAT_WIRETERM_TERMINATION//'.'//J_TYPE, found)
         if (.not. found) then
            write(error_unit, *) "Error reading wire terminal. Termination type not found."
         end if
         res%connectorType = strToTermination(label)
         call this%core%get(mat, J_MAT_WIRETERM_RESISTANCE, res%r, default=0.0)
         call this%core%get(mat, J_MAT_WIRETERM_INDUCTANCE, res%l, default=0.0)
         call this%core%get(mat, J_MAT_WIRETERM_CAPACITANCE, res%c, default=0.0)
      end function

      function strToTermination(label) result(res)
         character (len=:), allocatable, intent(in) :: label
         integer :: res
         select case (label)
          case (J_MAT_WIRETERM_TYPE_OPEN)
            res = MATERIAL_CONS
          case (J_MAT_WIRETERM_TYPE_SERIES)
            res = SERIES_CONS
          case (J_MAT_WIRETERM_TYPE_SHORT)
            res = MATERIAL_CONS
         end select
      end function

      logical function isThinWire(cable)
         type(json_value), pointer :: cable
         type(json_value_ptr) :: mat
         integer, dimension(:), allocatable :: elementIds
         logical :: found
         isThinWire = .false.

         mat = this%matTable%getId(this%getIntAt(cable, J_CAB_MAT_ID, found=found))
         if (.not. found) return
         if (this%getStrAt(mat%p, J_TYPE) /= J_MAT_TYPE_WIRE) return

         mat = this%matTable%getId(this%getIntAt(cable, J_CAB_INI_CONN_ID, found=found))
         if (.not. found) return
         if (this%getStrAt(mat%p, J_TYPE) /= J_MAT_TYPE_WIRE_TERMINAL) return

         mat = this%matTable%getId(this%getIntAt(cable, J_CAB_END_CONN_ID, found=found))
         if (.not. found) return
         if (this%getStrAt(mat%p, J_TYPE) /= J_MAT_TYPE_WIRE_TERMINAL) return

         call this%core%get(cable, J_ELEMENTIDS, elementIds, found=found)
         if (.not. found) return
         if (size(elementIds) /= 1) return

         isThinWire = .true.
      end function
   end function

   function readSlantedWires(this) result (res)
      class(parser_t) :: this
      type(SlantedWires) :: res
      ! TODO
   end function

   function getDomain(this, place, path) result(res)
      class(parser_t) :: this
      type(domain_t) :: res
      type(json_value), pointer :: place
      character(len=*), intent(in) :: path

      type(json_value), pointer :: domain
      character (len=:),allocatable :: fn, domainType
      logical :: found
      real :: val

      call this%core%get(place, path, domain, found)
      if (.not. found) return

      res%type1 = NP_T1_PLAIN

      call this%core%get(domain, J_TYPE, domainType)
      res%type2 = strToDomainType(domainType)

      call this%core%get(domain, J_PR_DOMAIN_TIME_START, res%tstart, default=0.0)
      call this%core%get(domain, J_PR_DOMAIN_TIME_STOP,  res%tstop,  default=0.0)
      call this%core%get(domain, J_PR_DOMAIN_TIME_STEP,  res%tstep,  default=0.0)
      call this%core%get(domain, J_PR_DOMAIN_FREQ_START, res%fstart, default=0.0)
      call this%core%get(domain, J_PR_DOMAIN_FREQ_STOP,  res%fstop,  default=0.0)
      call this%core%get(domain, J_PR_DOMAIN_FREQ_STEP,  res%fstep,  default=0.0)

      call this%core%get(domain, J_PR_DOMAIN_FILENAME, fn, found)
      if (found) then
         res%filename = trim(adjustl(fn))
      else
         res%filename = " "
      endif
   contains
      function strToDomainType(typeLabel) result(res)
         integer (kind=4) :: res
         character (len=:), allocatable :: typeLabel
         select case (typeLabel)
          case (J_PR_DOMAIN_TIME)
            res = NP_T2_TIME
          case (J_PR_DOMAIN_FREQ)
            res = NP_T2_FREQ
          case (J_PR_DOMAIN_TRANSFER)
            res = NP_T2_TRANSFER
          case (J_PR_DOMAIN_TIMEFREQ)
            res = NP_T2_TIMEFREQ
          case (J_PR_DOMAIN_TIMETRANSF)
            res = NP_T2_TIMETRANSF
          case (J_PR_DOMAIN_FREQTRANSF)
            res = NP_T2_FREQTRANSF
          case (J_PR_DOMAIN_TIMEFREQTRANSF)
            res = NP_T2_TIMEFRECTRANSF
         end select
      end function
   end function

   function readThinSlots(this) result (res)
      class(parser_t) :: this
      type(ThinSlots) :: res
      ! TODO
   end function

   function getIntAt(this, place, path, found) result(res)
      integer :: res
      class(parser_t) :: this
      type(json_value), pointer :: place
      character(len=*) :: path
      logical, intent(out), optional :: found
      call this%core%get(place, path, res, found)
   end function

   function getIntsAt(this, place, path, found) result(res)
      integer, dimension(:), allocatable :: res
      class(parser_t) :: this
      type(json_value), pointer :: place
      character(len=*) :: path
      logical, intent(out), optional :: found
      call this%core%get(place, path, res, found)
   end function

   function getjsonValuePtrAt(this, place, path, found) result(res)
      type(json_value), pointer :: res
      class(parser_t) :: this
      type(json_value), pointer :: place
      character(len=*) :: path
      logical, intent(out), optional :: found
      call this%core%get(place, path, res, found)
   end function

   function getRealAt(this, place, path, found) result(res)
      real :: res
      class(parser_t) :: this
      type(json_value), pointer :: place
      character(len=*) :: path
      logical, intent(out), optional :: found
      call this%core%get(place, path, res, found)
   end function

   function getStrAt(this, place, path, found) result(res)
      character (len=:), allocatable :: res
      class(parser_t) :: this
      type(json_value), pointer :: place
      character(len=*) :: path
      logical, intent(out), optional :: found
      call this%core%get(place, path, res, found)
   end function

   function existsAt(this, place, path) result(res)
      logical :: res
      class(parser_t) :: this
      type(json_value), pointer :: place
      character(len=*) :: path
      call this%core%info(place, path, found=res)
   end function

   function getCellRegionsWithMaterialType(this, matType) result(res)
      class(parser_t) :: this
      character (len=*), intent(in) :: matType
      type(cell_region_t), dimension(:), allocatable :: res

      logical :: found
      type(json_value), pointer :: jmrs, jmr
      type(json_value_ptr) :: jm
      integer, dimension(:), allocatable :: eIds
      type(cell_region_t) :: cR
      integer :: i, j
      integer :: numCellRegions

      call this%core%get(this%root, J_MATERIAL_REGIONS, jmrs, found)
      allocate(res(0))
      if (.not. found) then
         return
      end if

      do i = 1, this%core%count(jmrs)
         call this%core%get_child(jmrs, i, jmr)
         jm = this%matTable%getId(this%getIntAt(jmr, J_MATERIAL_ID, found))
         if (.not. found) &
            write(error_unit, *) "Error reading material region: materialId label not found."

         if (matType == this%getStrAt(jm%p, J_TYPE)) then
            eIds = this%getIntsAt(jmr, J_ELEMENTIDS)
            do j = 1, size(eIds)
               cR = this%mesh%getCellRegion(eIds(j), found)
               if (found) then
                  res = [res, cR]
               end if
            end do
         end if
      end do
   end function
#endif
end module
