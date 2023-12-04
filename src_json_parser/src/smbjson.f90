module smbjson

   use NFDETypes

   use NFDETypes_extension
   use labels_mod
   use mesh_mod
   use tools_mod
   use idchildtable_mod

   use json_module
   use json_kinds

   use, intrinsic :: iso_fortran_env , only: error_unit

   implicit none

   type, public :: parser_t
      private
      character (len=:), allocatable :: filename
      type(json_file) :: jsonfile
      type(json_core) :: core
      type(json_value), pointer :: root => null()
      type(mesh_t) :: mesh
   contains
      procedure :: readProblemDescription

      ! private
      procedure :: readGeneral
      procedure :: readGrid
      procedure :: readBoundary
      procedure :: readPlanewaves
      procedure :: readMoreProbes
      procedure :: readThinWires
      !
      procedure :: readMesh
   end type
   interface parser_t
      module procedure parser_ctor
   end interface

contains
   function parser_ctor(filename) result(res)
      type(parser_t) :: res
      character(len=*), intent(in) :: filename
      
      res%filename = filename

      call res%jsonfile%initialize()
      if (res%jsonfile%failed()) then
         call res%jsonfile%print_error_message(error_unit)
         stop
      end if

      call res%jsonfile%load(filename = filename)
      if (res%jsonfile%failed()) then
         call res%jsonfile%print_error_message(error_unit)
         stop
      end if

      call res%jsonfile%get_core(res%core)
      call res%jsonfile%get('.', res%root)

   end function

   function readProblemDescription(this) result (res)
      class(parser_t) :: this
      type(Parseador) :: res !! Problem Description
   
      call initializeProblemDescription(res)

      this%mesh = this%readMesh()


      ! Basics
      res%general = this%readGeneral()
      ! res%matriz = readMediaMatrix()
      res%despl = this%readGrid()
      res%front = this%readBoundary()
      ! Materials
      ! res%mats = this%readMaterials()
      ! res%pecRegs = this%readPECRegions()
      ! res%pmcRegs = this%readPMCRegions()
      ! res%DielRegs = this%readDielectricRegions()
      ! res%LossyThinSurfs = this%readLossyThinSurfaces()
      ! res%frqDepMats = this%readFrequencyDependentMaterials()
      ! res%aniMats = this%readAnisotropicMaterials()
      ! Sources
      ! res%boxSrc = this%readBoxSources()
      res%plnSrc = this%readPlanewaves()
      ! res%nodSrc = this%readNodalSources()
      ! Probes
      ! res%oldSonda = this%readProbes()
      res%sonda = this%readMoreProbes()
      ! res%BloquePrb = this%readBlockProbes()
      ! res%VolPrb = this%readVolumicProbes()
      ! Thin elements
      res%tWires = this%readThinWires()
      ! res%sWires = this%readSlantedWires()
      ! res%tSlots = this%readThinSlots()
   end function

   function readMesh(this) result(res)
      class(parser_t) :: this
      type(Mesh_t) :: res
      type(json_value), pointer :: jcs, jc
      integer :: id, i
      real, dimension(:), allocatable :: pos
      type(coordinate_t) :: c

      call this%core%get(this%root, J_MESH//'.'//J_COORDINATES, jcs)
      do i = 1, this%core%count(jcs)
         call this%core%get_child(jcs, i, jc)
         call this%core%get(jc, J_ID, id)
         call this%core%get(jc, J_COORD_POS, pos)
         c%position = pos
         call res%addCoordinate(id, c)
      end do
      
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
         call this%core%get(this%root, J_MESH//'.'//J_ELEMENTS//'.'//elementType, jes, found=found)
         if (found) then
            do i = 1, this%core%count(jes)
               call this%core%get_child(jes, i, je)
               call this%core%get(je, J_ID, id)
               call this%core%get(je, J_ELEM_COORD_IDS, coordIds)
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

   function readGeneral(this) result (res)
      class(parser_t) :: this
      type(NFDEGeneral) :: res
      call this%core%get(this%root, J_GENERAL//'.'//J_TIME_STEP,       res%dt)
      call this%core%get(this%root, J_GENERAL//'.'//J_NUMBER_OF_STEPS, res%nmax)
   end function

   ! function readMediaMatrix() result(res)
   !    type(MatrizMedios) :: res
   !    character (len=*), parameter :: P = J_MESH//'.'//J_GRID
   !    call this%core%get(root, P//'.'//J_NUMBER_OF_CELLS//'(1)',res%totalX)
   !    call this%core%get(root, P//'.'//J_NUMBER_OF_CELLS//'(2)',res%totalY)
   !    call this%core%get(root, P//'.'//J_NUMBER_OF_CELLS//'(3)',res%totalZ)
   ! end function

   function readGrid(this) result (res)
      class(parser_t) :: this
      type(Desplazamiento) :: res
      real, dimension(:), allocatable :: vec
      character (len=*), parameter :: P = J_MESH//'.'//J_GRID

      call this%core%get(this%root, P//'.'//J_NUMBER_OF_CELLS//'(1)',res%nX)
      call this%core%get(this%root, P//'.'//J_NUMBER_OF_CELLS//'(2)',res%nY)
      call this%core%get(this%root, P//'.'//J_NUMBER_OF_CELLS//'(3)',res%nZ)

      call this%core%get(this%root, P//'.'//J_STEPS//'.x', vec)
      res%desX = vec
      call this%core%get(this%root, P//'.'//J_STEPS//'.y', vec)
      res%desY = vec
      call this%core%get(this%root, P//'.'//J_STEPS//'.z', vec)
      res%desZ = vec
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
      function readPMLProperties(path) result(res)
         type(FronteraPML) :: res
         character(len=*), intent(in) :: path
         call this%core%get(this%root, path//'.'//J_BND_PML_LAYERS, res%numCapas, default=8)
         call this%core%get(this%root, path//'.'//J_BND_PML_ORDER, res%orden, default=2.0)
         call this%core%get(this%root, path//'.'//J_BND_PML_REFLECTION, res%refl, default=0.001)
      end function

      function labelToBoundaryType(str) result (type)
         character(kind=json_CK, len=:), allocatable :: str
         integer(kind=4) :: type
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

   ! function readMaterials() result (res)
   !    type(Materials) :: res
   !    ! TODO
   ! end function

   ! function readPECRegions() result (res)
   !    type(PECRegions) :: res
   !    ! TODO
   ! end function

   ! function readPMCRegions() result (res)
   !    type(PECRegions) :: res
   !    ! TODO
   ! end function

   ! function readDielectricRegions() result (res)
   !    type(DielectricRegions) :: res
   !    ! TODO
   ! end function

   ! function readLossyThinSurfaces() result (res)
   !    type(LossyThinSurfaces) :: res
   !    ! TODO
   ! end function

   ! function readFrequencyDependentMaterials() result (res)
   !    type(FreqDepenMaterials) :: res


   !    ! TODO
   ! end function

   ! function readAnisotropicMaterials() result (res)
   !    type(ANISOTROPICelements_t) :: res


   !    ! TODO
   ! end function

   ! function readBoxSources() result (res)
   !    type(Boxes) :: res


   !    ! TODO
   ! end function

   function readPlanewaves(this) result (res)
      class(parser_t) :: this
      type(PlaneWaves) :: res
      type(json_value), pointer :: sources
      type(json_value_ptr), allocatable :: pws(:)
      integer :: i

      call this%core%get(this%root, J_SOURCES, sources)
      pws = jsonValueFilterByKeyValue(this%core, sources, J_SRC_TYPE, J_PW_TYPE)
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

         call this%core%get(pw, J_SRC_MAGNITUDE_FILE, label)
         res%nombre_fichero = trim(adjustl(label))

         call this%core%get(pw, J_PW_ATTRIBUTE, label, found)
         if (found) then
            res%atributo = trim(adjustl(label))
         else
            res%atributo = ""
         endif

         call this%core%get(pw, J_PW_DIRECTION//'.'//J_PW_DIRECTION_THETA, res%theta)
         call this%core%get(pw, J_PW_DIRECTION//'.'//J_PW_DIRECTION_PHI, res%phi)
         call this%core%get(pw, J_PW_POLARIZATION//'.'//J_PW_POLARIZATION_ALPHA, res%alpha)
         call this%core%get(pw, J_PW_POLARIZATION//'.'//J_PW_POLARIZATION_BETA, res%beta)

         region = getCellRegion(this%core, pw)
         res%coor1 = region%coords(1)%v(:)
         res%coor2 = region%coords(2)%v(:)

         res%isRC = .false.
         res%nummodes = 1
         res%incertmax = 0.0
      end function
   end function

   ! function readNodalSources() result (res)
   !    type(NodSource) :: res
   !    ! TODO
   ! end function

   function readProbes(this) result (res)
      class(parser_t) :: this
      type(Sondas) :: res
      type(json_value), pointer :: probes
      type(json_value_ptr), allocatable :: ps(:)
      integer :: i
      character (len=*), dimension(1), parameter :: validTypes = &
         (/J_PR_FARFIELD/)

      call this%core%get(this%root, J_PROBES, probes)
      ps = jsonValueFilterByKeyValues(this%core, probes, J_PR_TYPE, validTypes)
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

         ! call this%core%get(p, J_PR_OUTPUT_NAME, tag)
         ! res%outputrequest = trim(adjustl(tag))

         ! call this%core%get(p, J_PR_TYPE, typeLabel)

         ! call getDomain(p, res)

         ! cells = getCells(p, J_PIXELS)
         ! call this%core%get(p, J_PR_DIRECTIONS, dirLabels)
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

   function readMoreProbes(this) result (res)
      class(parser_t) :: this
      type(MasSondas) :: res
      type(json_value), pointer :: probes
      type(json_value_ptr), allocatable :: ps(:)
      integer :: i
      character (len=*), dimension(4), parameter :: validTypes = &
         [J_PR_ELECTRIC, J_PR_MAGNETIC, J_PR_CURRENT, J_PR_VOLTAGE]

      call this%core%get(this%root, J_PROBES, probes)
      ps = jsonValueFilterByKeyValues(this%core, probes, J_PR_TYPE, validTypes)
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
         integer, dimension(:), allocatable :: coordinateIds

         call this%core%get(p, J_PR_OUTPUT_NAME, outputName)
         res%outputrequest = trim(adjustl(outputName))

         call this%core%get(p, J_PR_TYPE, typeLabel)

         call getDomain(p, res)
         if (typeLabel == J_PR_CURRENT .or. typeLabel == J_PR_VOLTAGE) then
            cells = getCellsFromNodeElementIds(this%core, this%mesh, p, coordinateIds)
            allocate(res%cordinates(size(cells)))
            do i = 1, size(cells)
               res%cordinates(i)%tag = ' '
               res%cordinates(i)%Xi = coordinateIds(i)
               res%cordinates(i)%Yi = 0
               res%cordinates(i)%Zi = 0
               res%cordinates(i)%Or = strToProbeType(typeLabel)
            end do
         else
            cells = [ &
               getCellsFromNodeElementIds(this%core, this%mesh, p), &
               getSimpleCells(this%core, p, J_PIXELS) &
            ]
            call this%core%get(p, J_PR_DIRECTIONS, dirLabels)
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

         call this%core%get(p, J_PR_DOMAIN, domain)

         res%type1 = NP_T1_PLAIN

         call this%core%get(domain, J_PR_TYPE, domainType)
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

   ! function readBlockProbes() result (res)
   !    type(BloqueProbes) :: res


   !    ! TODO
   ! end function

   ! function readVolumicProbes() result (res)
   !    type(VolProbes) :: res


   !    ! TODO
   ! end function

   function readThinWires(this) result (res)
      class(parser_t) :: this
      type(ThinWires) :: res
      type(json_value), pointer :: cables
      type(IdChildTable_t) :: mats
      logical :: cablesFound

      call this%core%get(this%root, J_CABLES, cables, found=cablesFound)
      if (.not. cablesFound) then
         allocate(res%tw(0))
         res%n_tw = 0
         res%n_tw_max = 0
         return
      end if

      ! mats = IdChildTable_t(J_MATERIALS)

      ! Allocates thin wires.
      block
         integer :: nTw = 0
         integer :: i
         type(json_value), pointer :: cable
         do i = 1, this%core%count(cables)
            call this%core%get_child(cables, i, cable)
            if (isThinWire(cable, mats)) nTw = nTw+1
         end do

         allocate(res%tw(nTw))
         res%n_tw = size(res%tw)
         res%n_tw_max = size(res%tw)
      end block

   contains
      logical function isThinWire(cable, mats)
         type(json_value), pointer :: cable, mat
         type(IdChildTable_t), intent(in) :: mats
         integer :: mId
         character (len=:), allocatable :: typeStr

         isThinWire = .false.

         call this%core%get(cable, J_CAB_MAT_ID, mId)
         mat = mats%getId(mId)
         call this%core%get(mat, J_TYPE, typeStr)
         if (typeStr /= J_MAT_TYPE_WIRE) return

         call this%core%get(cable, J_CAB_INI_CONN_ID, mId)
         call this%core%get(mats%getId(mId), J_TYPE, typeStr)
         if (typeStr /= J_MAT_TYPE_CONNECTOR) return

         call this%core%get(cable, J_CAB_END_CONN_ID, mId)
         call this%core%get(mats%getId(mId), J_TYPE, typeStr)
         if (typeStr /= J_MAT_TYPE_CONNECTOR) return

         isThinWire = .true.
      end function
   end function

   ! function readSlantedWires() result (res)
   !    type(SlantedWires) :: res
   !    ! TODO
   ! end function

   ! function readThinSlots() result (res)
   !    type(ThinSlots) :: res

   !    type(json_value), pointer :: root
   !    ! TODO
   ! end function
end module
