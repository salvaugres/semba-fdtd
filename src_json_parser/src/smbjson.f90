module smbjson

   use NFDETypes

   use NFDETypes_extension
   use mesh_mod

   use json_module
   use json_kinds

   use, intrinsic :: iso_fortran_env , only: error_unit

   implicit none

   ! Typical use.
   public :: readProblemDescription

   ! Set to public for testing.
   public :: readMoreProbes

   private

   integer, parameter :: J_ERROR_NUMBER = 1
   ! LABELS
   ! -- shared labels
   character (len=*), parameter :: J_VOXEL_REGION = "voxelRegion"
   character (len=*), parameter :: J_VOXELS = "voxels"
   character (len=*), parameter :: J_SURFELS = "surfels"
   character (len=*), parameter :: J_LINELS = "linels"
   character (len=*), parameter :: J_PIXELS = "pixels"
   character (len=*), parameter :: J_ELEMENTIDS = "elementIds"

   character (len=*), parameter :: J_DIR_X = "x"
   character (len=*), parameter :: J_DIR_Y = "y"
   character (len=*), parameter :: J_DIR_Z = "z"

   character (len=*), parameter :: J_PROPERTIES = "properties"

   character (len=*), parameter :: J_MATERIALS = "materials"
   character (len=*), parameter :: J_CABLES = "cables"

   ! type(NFDEGeneral)
   character (len=*), parameter :: J_GENERAL = "general"
   character (len=*), parameter :: J_TIME_STEP = "timeStep"
   character (len=*), parameter :: J_NUMBER_OF_STEPS = "numberOfSteps"

   ! -- Mesh
   character (len=*), parameter :: J_MESH = "mesh"
   character (len=*), parameter :: J_COORDINATES = "coordinates"
   character (len=*), parameter :: J_COORD_ID = "id"
   character (len=*), parameter :: J_COORD_POS = "position"
   character (len=*), parameter :: J_ELEMENTS = "elements"
   character (len=*), parameter :: J_ELEM_ID = "id"
   character (len=*), parameter :: J_ELEM_COORD_IDS = "coordinateIds"
   character (len=*), parameter :: J_NODES = "nodes"
   character (len=*), parameter :: J_POLYLINES = "polylines"

   ! type(Desplazamiento)
   character (len=*), parameter :: J_GRID = "grid"
   character (len=*), parameter :: J_NUMBER_OF_CELLS = "numberOfCells"
   character (len=*), parameter :: J_STEPS = "steps"

   ! type(Frontera)
   character (len=*), parameter :: J_BOUNDARY = "boundary"
   character (len=*), parameter :: J_BOUNDARY_TYPE = "type"
   character (len=*), parameter :: J_ALL = "all"
   character (len=*), parameter :: J_PEC = "pec"
   character (len=*), parameter :: J_PMC = "pmc"
   character (len=*), parameter :: J_PERIODIC = "periodic"
   character (len=*), parameter :: J_MUR = "mur"
   character (len=*), parameter :: J_PML = "pml"
   character (len=*), parameter :: J_PML_LAYERS = "layers"
   character (len=*), parameter :: J_PML_ORDER = "order"
   character (len=*), parameter :: J_PML_REFLECTION = "reflection"

   ! -- source types
   character (len=*), parameter :: J_SOURCES = "sources"
   character (len=*), parameter :: J_MAGNITUDE_FILE = "magnitudeFile"
   character (len=*), parameter :: J_SRC_TYPE = "type"
   ! type(Planewave)
   character (len=*), parameter :: J_PW_TYPE = "planewave"
   character (len=*), parameter :: J_PW_ATTRIBUTE = "attribute"
   character (len=*), parameter :: J_PW_DIRECTION = "direction"
   character (len=*), parameter :: J_PW_DIRECTION_THETA = "theta"
   character (len=*), parameter :: J_PW_DIRECTION_PHI = "phi"
   character (len=*), parameter :: J_PW_POLARIZATION = "polarization"
   character (len=*), parameter :: J_PW_POLARIZATION_ALPHA = "alpha"
   character (len=*), parameter :: J_PW_POLARIZATION_BETA  = "beta"

   ! --- probe types
   character (len=*), parameter :: J_PROBES = "probes"
   character (len=*), parameter :: J_PR_TYPE = "type"
   character (len=*), parameter :: J_PR_OUTPUT_NAME = "outputName"
   character (len=*), parameter :: J_PR_DIRECTIONS = "directions"

   ! domain stuff
   character (len=*), parameter :: J_PR_DOMAIN = "domain"
   character (len=*), parameter :: J_PR_DOMAIN_FILENAME = "filename"
   character (len=*), parameter :: J_PR_DOMAIN_TYPE = "type"
   character (len=*), parameter :: J_PR_DOMAIN_TIME = "time"
   character (len=*), parameter :: J_PR_DOMAIN_FREQ = "frequency"
   character (len=*), parameter :: J_PR_DOMAIN_TRANSFER = "transfer"
   character (len=*), parameter :: J_PR_DOMAIN_TIMEFREQ = "timeFrequency"
   character (len=*), parameter :: J_PR_DOMAIN_TIMETRANSF = "timeTransfer"
   character (len=*), parameter :: J_PR_DOMAIN_FREQTRANSF = "frequencyTransfer"
   character (len=*), parameter :: J_PR_DOMAIN_TIMEFREQTRANSF = "all"
   character (len=*), parameter :: J_PR_DOMAIN_TIME_START = "timeStart"
   character (len=*), parameter :: J_PR_DOMAIN_TIME_STOP   = "timeEnd"
   character (len=*), parameter :: J_PR_DOMAIN_TIME_STEP  = "timeStep"
   character (len=*), parameter :: J_PR_DOMAIN_FREQ_START = "frequencyStart"
   character (len=*), parameter :: J_PR_DOMAIN_FREQ_STOP   = "frequencyEnd"
   character (len=*), parameter :: J_PR_DOMAIN_FREQ_STEP  = "frequencyStep"

   ! type(Sonda)
   character (len=*), parameter :: J_PR_FARFIELD = "farField"
   ! type(MasSonda)
   character (len=*), parameter :: J_PR_ELECTRIC = "electric"
   character (len=*), parameter :: J_PR_MAGNETIC = "magnetic"
   character (len=*), parameter :: J_PR_CURRENT = "current"
   character (len=*), parameter :: J_PR_VOLTAGE = "voltage"

   type, private :: Cell
      real, dimension(3) :: v
   end type Cell

   type, private :: CellRegion
      type(Cell), dimension(2) :: coords
   end type CellRegion

   type, private :: json_value_ptr
      type(json_value), pointer :: p
   end type

   type(json_core) :: core
   type(json_value), pointer :: root => null()

contains
   function readProblemDescription(filename) result (res)
      character (len=*), intent(in) :: filename
      type(Parseador) :: res !! Problem Description

      type(json_file) :: json

      call json%initialize()
      if (json%failed()) then
         call json%print_error_message(error_unit)
         stop
      end if

      call json%load(filename = filename)
      if (json%failed()) then
         call json%print_error_message(error_unit)
         stop
      end if

      if (associated(root)) then
         write(error_unit, *) 'smbjson module is in use. Aborting.'
         stop J_ERROR_NUMBER
      end if
      call json%get_core(core)
      call json%get('.', root)

      call initializeProblemDescription(res)
      ! Basics
      res%general = readGeneral()
      res%matriz = readMediaMatrix()
      res%despl = readGrid()
      res%front = readBoundary()
      ! Materials
      ! res%mats = readMaterials()
      ! res%pecRegs = readPECRegions()
      ! res%pmcRegs = readPMCRegions()
      ! res%DielRegs = readDielectricRegions()
      ! res%LossyThinSurfs = readLossyThinSurfaces()
      ! res%frqDepMats = readFrequencyDependentMaterials()
      ! res%aniMats = readAnisotropicMaterials()
      ! Sources
      ! res%boxSrc = readBoxSources()
      res%plnSrc = readPlanewaves()
      ! res%nodSrc = readNodalSources()
      ! Probes
      res%oldSonda = readProbes()
      res%sonda = readMoreProbes()
      ! res%BloquePrb = readBlockProbes()
      ! res%VolPrb = readVolumicProbes()
      ! Thin elements
      res%tWires = readThinWires()
      ! res%sWires = readSlantedWires()
      ! res%tSlots = readThinSlots()

      nullify(root)
   end function

   subroutine getRealVec(place, path, dest)
      type(json_value), pointer :: place
      character(kind=CK, len=*) :: path
      real (kind=RK), pointer :: dest(:)

      real(RK), allocatable :: vec(:)
      logical :: found = .false.

      call core%get(place, path, vec, found)
      if (found) then
         allocate(dest(size(vec)))
         dest = vec
      endif
   end subroutine

   function getCellRegion(place) result (res)
      type(json_value), pointer :: place
      type(CellRegion) :: res

      integer :: i, n
      type(json_value), pointer :: coordEntry, voxelRegionEntry
      real (kind=RK), dimension(:), allocatable :: vec

      call core%get(place, J_VOXEL_REGION, voxelRegionEntry)
      do i = 1, core%count(voxelRegionEntry)
         call core%get_child(voxelRegionEntry, i, coordEntry)
         call core%get(coordEntry, '.', vec)
         if (size(vec) /= 3) then
            write(error_unit, *) "Voxel regions are defined by two numerical vectors of size 3."
            stop J_ERROR_NUMBER
         end if
         res%coords(i)%v(:) = vec(1:3)
      end do
   end function

   function getSimpleCells(place, path) result(res)
      type(json_value), pointer :: place
      character (len=*), intent(in) :: path
      type(Cell), dimension(:), allocatable :: res

      integer :: i, n
      type(json_value), pointer :: cellsEntry, coordEntry
      real (kind=RK), pointer :: vec(:)
      logical :: cellsFound = .false.

      call core%get(place, path, cellsEntry, found=cellsFound)
      if (.not. cellsFound) then
         allocate(res(0))
         return
      end if
      allocate(res(core%count(cellsEntry)))
      do i = 1, core%count(cellsEntry)
         call core%get_child(cellsEntry, i, coordEntry)
         call getRealVec(coordEntry, '.', vec)
         if (size(vec) /= 3) then
            stop "Cells are defined by a vector of size 3."
         end if
         res(i)%v = vec
      end do
   end function

   function readMesh() result(res)
      type(Mesh_t) :: res

      block
         type(json_value), pointer :: jcs, jc
         integer :: id, i
         real, dimension(:), allocatable :: pos
         type(coordinate_t) :: c
         call core%get(root, J_MESH//'.'//J_COORDINATES, jcs)
         do i = 1, core%count(jcs)
            call core%get_child(jcs, i, jc)
            call core%get(jc, J_COORD_ID, id)
            call core%get(jc, J_COORD_POS, pos)
            c%position = pos
            call res%addCoordinate(id, c)
         end do
      end block

      call addElementsOfType(res, J_NODES)
      call addElementsOfType(res, J_POLYLINES)

   contains
      subroutine addElementsOfType(mesh, elementType)
         type(mesh_t), intent(inout) :: mesh
         character(len=*), intent(in) :: elementType
         type(json_value), pointer :: jes, je
         integer :: id, i
         type(node_t) :: e
         integer, dimension(:), allocatable :: coordIds
         logical :: found
         call core%get(root, J_MESH//'.'//J_ELEMENTS//'.'//elementType, jes, found=found)
         if (found) then
            do i = 1, core%count(jes)
               call core%get_child(jes, i, je)
               call core%get(je, J_ELEM_ID, id)
               call core%get(je, J_ELEM_COORD_IDS, coordIds)
               select case (elementType)
                case (J_NODES)
                  call mesh%addElement(id, node_t(coordIds))
                case (J_POLYLINES)
                  call mesh%addElement(id, polyline_t(coordIds))
                case default
                  write (error_unit, *) 'Invalid element type'
                  stop
               end select
            end do
         end if
      end subroutine
   end function

   function getCellsFromNodeElementIds(place, nodeIds) result(res)
      type(Cell), dimension(:), allocatable :: res
      type(json_value), pointer :: place
      integer, dimension(:), allocatable, optional, intent(out) :: nodeIds
      integer, dimension(:), allocatable :: ids

      type(Mesh_t) :: mesh
      type(node_t) :: node
      type(coordinate_t) :: coordinate
      logical :: idsFound, nodeFound, coordinateFound
      integer :: i

      call core%get(place, J_ELEMENTIDS, ids, found=idsFound)
      if (.not. idsFound) then
         allocate(res(0))
         return
      end if

      mesh = readMesh()

      allocate(res(size(ids)))
      if (present(nodeIds)) nodeIds = ids
      do i = 1, size(ids)
         nodeFound = .false.
         coordinateFound = .false.
         node = mesh%getNode(ids(i), nodeFound)
         if (nodeFound) coordinate = mesh%getCoordinate(node%coordIds(1), coordinateFound)
         if (coordinateFound) res(i)%v = coordinate%position
      end do
   end function

   function jsonValueFilterByKeyValues(srcs, key, values) result (out)
      type(json_value_ptr), allocatable :: out(:)
      type(json_value), pointer :: srcs

      character (kind=JSON_CK, len=*) :: key
      character (kind=JSON_CK, len=*), dimension(:) :: values

      type(json_value_ptr), dimension (:), allocatable :: foundEntries, tmp
      integer :: i, lastEntry, nEntries

      do i = 1, size(values)
         if (allocated(foundEntries)) deallocate(foundEntries)
         foundEntries = jsonValueFilterByKeyValue(srcs, key, values(i))
         if (size(foundEntries) == 0) continue
         if (allocated(out)) then
            allocate(tmp(size(out)))
            tmp = out
            deallocate(out)
            allocate(out(size(tmp) + size(foundEntries)))
            out(:size(tmp)) = tmp
            out((size(tmp)+1):) = foundEntries
            deallocate(tmp)
         else
            allocate(out(size(foundEntries)))
            out = foundEntries
         end if
      end do

   end function

   function jsonValueFilterByKeyValue(place, key, value) result (out)
      type(json_value_ptr), allocatable :: out(:)
      character (kind=JSON_CK, len=*) :: key, value
      type(json_value), pointer :: place, src
      character (kind=JSON_CK, len=:), allocatable :: type
      integer :: i, j, n
      logical :: found

      n = 0
      do i = 1, core%count(place)
         call core%get_child(place, i, src)
         call core%get(src, key, type, found)
         if(found .and. type == value) then
            n = n + 1
         end if
      end do

      allocate(out(n))
      j = 1
      do i = 1, core%count(place)
         call core%get_child(place, i, src)
         call core%get(src, key, type, found)
         if(found .and. type == value) then
            out(j)%p => src
            j = j + 1
         end if
      end do

   end function

   function readGeneral() result (res)
      type(NFDEGeneral) :: res
      call core%get(root, J_GENERAL//'.'//J_TIME_STEP,       res%dt)
      call core%get(root, J_GENERAL//'.'//J_NUMBER_OF_STEPS, res%nmax)
   end function

   function readMediaMatrix() result(res)
      type(MatrizMedios) :: res
      character (len=*), parameter :: P = J_MESH//'.'//J_GRID
      call core%get(root, P//'.'//J_NUMBER_OF_CELLS//'(1)',res%totalX)
      call core%get(root, P//'.'//J_NUMBER_OF_CELLS//'(2)',res%totalY)
      call core%get(root, P//'.'//J_NUMBER_OF_CELLS//'(3)',res%totalZ)
   end function

   function readGrid() result (res)
      type(Desplazamiento) :: res

      character (len=*), parameter :: P = J_MESH//'.'//J_GRID

      call core%get(root, P//'.'//J_NUMBER_OF_CELLS//'(1)',res%nX)
      call core%get(root, P//'.'//J_NUMBER_OF_CELLS//'(2)',res%nY)
      call core%get(root, P//'.'//J_NUMBER_OF_CELLS//'(3)',res%nZ)

      call getRealVec(root, P//'.'//J_STEPS//'.x', res%desX)
      call getRealVec(root, P//'.'//J_STEPS//'.y', res%desY)
      call getRealVec(root, P//'.'//J_STEPS//'.z', res%desZ)
   end function

   function readBoundary() result (res)
      type(Frontera) :: res
      character(kind=json_CK,len=:), allocatable :: boundaryTypeLabel
      logical(LK) :: allLabelFound = .false.

      call core%get(root, J_BOUNDARY//'.'//J_ALL//'.'//J_BOUNDARY_TYPE,  boundaryTypeLabel, allLabelFound)
      if (allLabelFound) then
         res%tipoFrontera(:) = labelToBoundaryType(boundaryTypeLabel)
         if (all(res%tipoFrontera == F_PML)) then
            res%propiedadesPML(:) = readPMLProperties(J_BOUNDARY//"."//J_ALL)
         end if
         return
      else
         ! TODO Check every bound.
      end if
   contains
      function readPMLProperties(path) result(res)
         type(FronteraPML) :: res
         character(len=*), intent(in) :: path
         call core%get(root, path//'.'//J_PML_LAYERS, res%numCapas, default=8)
         call core%get(root, path//'.'//J_PML_ORDER, res%orden, default=2.0)
         call core%get(root, path//'.'//J_PML_REFLECTION, res%refl, default=0.001)
      end function

      function labelToBoundaryType(str) result (type)
         character(kind=json_CK, len=:), allocatable :: str
         integer(kind=4) :: type
         select case (str)
          case (J_PEC)
            type = F_PEC
          case (J_PMC)
            type = F_PMC
          case (J_PERIODIC)
            type = F_PER
          case (J_MUR)
            type = F_MUR
          case (J_PML)
            type = F_PML
         end select
      end function
   end function

   function readMaterials() result (res)
      type(Materials) :: res
      ! TODO
   end function

   function readPECRegions() result (res)
      type(PECRegions) :: res
      ! TODO
   end function

   function readPMCRegions() result (res)
      type(PECRegions) :: res
      ! TODO
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

   function readPlanewaves() result (res)
      type(PlaneWaves) :: res
      type(json_value), pointer :: sources
      type(json_value_ptr), allocatable :: pws(:)
      integer :: i

      call core%get(root, J_SOURCES, sources)
      pws = jsonValueFilterByKeyValue(sources, J_SRC_TYPE, J_PW_TYPE)
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

         character (len=:), allocatable :: label
         type(CellRegion) :: region
         logical :: found

         call core%get(pw, J_MAGNITUDE_FILE, label)
         res%nombre_fichero = trim(adjustl(label))

         call core%get(pw, J_PW_ATTRIBUTE, label, found)
         if (found) then
            res%atributo = trim(adjustl(label))
         else
            res%atributo = ""
         endif

         call core%get(pw, J_PW_DIRECTION//'.'//J_PW_DIRECTION_THETA, res%theta)
         call core%get(pw, J_PW_DIRECTION//'.'//J_PW_DIRECTION_PHI, res%phi)
         call core%get(pw, J_PW_POLARIZATION//'.'//J_PW_POLARIZATION_ALPHA, res%alpha)
         call core%get(pw, J_PW_POLARIZATION//'.'//J_PW_POLARIZATION_BETA, res%beta)

         region = getCellRegion(pw)
         res%coor1 = region%coords(1)%v(:)
         res%coor2 = region%coords(2)%v(:)

         res%isRC = .false.
         res%nummodes = 1
         res%incertmax = 0.0
      end function
   end function

   function readNodalSources() result (res)
      type(NodSource) :: res
      ! TODO
   end function

   function readProbes() result (res)
      type(Sondas) :: res
      type(json_value), pointer :: probes
      type(json_value_ptr), allocatable :: ps(:)
      integer :: i
      character (len=*), dimension(1), parameter :: validTypes = &
         (/J_PR_FARFIELD/)

      call core%get(root, J_PROBES, probes)
      ps = jsonValueFilterByKeyValues(probes, J_PR_TYPE, validTypes)
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

         stop 'read abstract sonda is TBD. TODO'
         ! integer :: i, j, k
         ! character (len=:), allocatable :: typeLabel, tag
         ! character(kind=CK,len=1), dimension(:), allocatable :: dirLabels
         ! type(Cell), dimension(:), allocatable :: cells

         ! call core%get(p, J_PR_OUTPUT_NAME, tag)
         ! res%outputrequest = trim(adjustl(tag))

         ! call core%get(p, J_PR_TYPE, typeLabel)

         ! call getDomain(p, res)

         ! cells = getCells(p, J_PIXELS)
         ! call core%get(p, J_PR_DIRECTIONS, dirLabels)
         ! allocate(res%cordinates(size(cells) * size(dirLabels)))
         ! do i = 1, size(cells)
         !    k = (i-1) * size(dirLabels)
         !    do j = 1, size(dirLabels)
         !       res%cordinates(k+j)%tag = tag
         !       res%cordinates(k+j)%Xi = int (cells(i)%v(1))
         !       res%cordinates(k+j)%Yi = int (cells(i)%v(2))
         !       res%cordinates(k+j)%Zi = int (cells(i)%v(3))
         !       res%cordinates(k+j)%Or = strToProbeType(typeLabel, dirLabels(j))
         !    end do
         ! end do
      end function
   end function

   function readMoreProbes() result (res)
      type(MasSondas) :: res
      type(json_value), pointer :: probes
      type(json_value_ptr), allocatable :: ps(:)
      integer :: i
      character (len=*), dimension(4), parameter :: validTypes = &
         [J_PR_ELECTRIC, J_PR_MAGNETIC, J_PR_CURRENT, J_PR_VOLTAGE]

      call core%get(root, J_PROBES, probes)
      ps = jsonValueFilterByKeyValues(probes, J_PR_TYPE, validTypes)
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
         type(Cell), dimension(:), allocatable :: cells
         integer, dimension(:), allocatable :: nodeIds

         call core%get(p, J_PR_OUTPUT_NAME, outputName)
         res%outputrequest = trim(adjustl(outputName))

         call core%get(p, J_PR_TYPE, typeLabel)

         call getDomain(p, res)
         if (typeLabel == J_PR_CURRENT .or. typeLabel == J_PR_VOLTAGE) then 
            cells = getCellsFromNodeElementIds(p, nodeIds)
            allocate(res%cordinates(size(cells)))
            do i = 1, size(cells)
               res%cordinates(i)%tag = ' '
               res%cordinates(i)%Xi = nodeIds(i)
               res%cordinates(i)%Yi = 0
               res%cordinates(i)%Zi = 0
               res%cordinates(i)%Or = strToProbeType(typeLabel)
            end do
         else 
            cells = [ getCellsFromNodeElementIds(p), getSimpleCells(p, J_PIXELS) ]
            call core%get(p, J_PR_DIRECTIONS, dirLabels)
            allocate(res%cordinates(size(cells) * size(dirLabels)))
            do i = 1, size(cells)
               k = (i-1) * size(dirLabels)
               do j = 1, size(dirLabels)
                  res%cordinates(k+j)%tag = ' '
                  res%cordinates(k+j)%Xi = int (cells(i)%v(1))
                  res%cordinates(k+j)%Yi = int (cells(i)%v(2))
                  res%cordinates(k+j)%Zi = int (cells(i)%v(3))
                  res%cordinates(k+j)%Or = strToProbeType(typeLabel, dirLabels(j))
               end do
            end do
   
         end if

      end function

      subroutine getDomain(p, res)
         type(MasSonda), intent(inout) :: res
         type(json_value), pointer :: p, domain

         character (len=:),allocatable :: fn, domainType
         logical :: found
         real :: val

         call core%get(p, J_PR_DOMAIN, domain)

         res%type1 = NP_T1_PLAIN

         call core%get(domain, J_PR_TYPE, domainType)
         res%type2 = strToDomainType(domainType)


         call core%get(domain, J_PR_DOMAIN_TIME_START, res%tstart, default=0.0)
         call core%get(domain, J_PR_DOMAIN_TIME_STOP,  res%tstop,  default=0.0)
         call core%get(domain, J_PR_DOMAIN_TIME_STEP,  res%tstep,  default=0.0)
         call core%get(domain, J_PR_DOMAIN_FREQ_START, res%fstart, default=0.0)
         call core%get(domain, J_PR_DOMAIN_FREQ_STOP,  res%fstop,  default=0.0)
         call core%get(domain, J_PR_DOMAIN_FREQ_STEP,  res%fstep,  default=0.0)

         call core%get(domain, J_PR_DOMAIN_FILENAME, fn, found)
         if (found) then
            res%filename = trim(adjustl(fn))
         else
            res%filename = " "
         endif

      end subroutine

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

      function strToProbeType(typeLabel, dirLabel) result(res)
         integer (kind=4) :: res
         character (len=:), allocatable :: typeLabel
         character (len=1), optional :: dirLabel
         select case (typeLabel)
          case (J_PR_ELECTRIC)
            if (.not. present(dirLabel)) then
               stop "Dir label must be present"
            end if
            select case (dirLabel)
             case (J_DIR_X)
               res = NP_COR_EX
             case (J_DIR_Y)
               res = NP_COR_EY
             case (J_DIR_Z)
               res = NP_COR_EZ
            end select
          case (J_PR_MAGNETIC)
            if (.not. present(dirLabel)) then
               stop "Dir label must be present"
            end if
            select case (dirLabel)
             case (J_DIR_X)
               res = NP_COR_HX
             case (J_DIR_Y)
               res = NP_COR_HY
             case (J_DIR_Z)
               res = NP_COR_HZ
            end select
          case (J_PR_CURRENT)
            res = NP_COR_WIRECURRENT
          case (J_PR_VOLTAGE)
            res = NP_COR_DDP
         end select
      end function

   end function

   function readBlockProbes() result (res)
      type(BloqueProbes) :: res


      ! TODO
   end function

   function readVolumicProbes() result (res)
      type(VolProbes) :: res


      ! TODO
   end function

   function readThinWires() result (res)
      type(ThinWires) :: res
      type(json_value), pointer :: mats
      logical :: materialsFound

      call core%get(root, J_MATERIALS, mats)
      if (.not. materialsFound) then
         allocate(res%tw(0))
         res%n_tw = 0
         res%n_tw_max = 0
         return
      end if

   end function

   function readSlantedWires() result (res)
      type(SlantedWires) :: res
      ! TODO
   end function

   function readThinSlots() result (res)
      type(ThinSlots) :: res

      type(json_value), pointer :: root
      ! TODO
   end function
end module
