module smbjson

   use NFDETypes
   use NFDETypes_extension

   use json_module
   use json_kinds

   implicit none

   ! Typical use.
   public :: readProblemDescription

   ! Set to public for testing.
   public :: readProbes

   private
   ! LABELS
   ! -- shared labels
   character (len=*), parameter :: J_CELL_REGION = "cellRegion"
   character (len=*), parameter :: J_CELLS = "cells"

   character (len=*), parameter :: J_DIR_X = "x"
   character (len=*), parameter :: J_DIR_Y = "y"
   character (len=*), parameter :: J_DIR_Z = "z"

   ! type(NFDEGeneral)
   character (len=*), parameter :: J_GENERAL = "general"
   character (len=*), parameter :: J_TIME_STEP = "timeStep"
   character (len=*), parameter :: J_NUMBER_OF_STEPS = "numberOfSteps"

   ! type(Desplazamiento)
   character (len=*), parameter :: J_GRID = "grid"
   character (len=*), parameter :: J_NUMBER_OF_CELLS = "numberOfCells"
   character (len=*), parameter :: J_STEPS = "steps"

   ! type(Frontera)
   character (len=*), parameter :: J_BOUNDARY = "boundary"
   character (len=*), parameter :: J_ALL = "all"
   character (len=*), parameter :: J_PEC = "pec"
   character (len=*), parameter :: J_PMC = "pmc"
   character (len=*), parameter :: J_PERIODIC = "periodic"
   character (len=*), parameter :: J_MUR = "mur"
   character (len=*), parameter :: J_PML = "pml"

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
  
   ! type(MasSonda)
   character (len=*), parameter :: J_PR_ELECTRIC = "electric"
   character (len=*), parameter :: J_PR_MAGNETIC = "magnetic"
   character (len=*), parameter :: J_PR_CURRENT = "current"
   character (len=*), parameter :: J_PR_VOLTAGE = "voltage"

   type, private :: VoxelRegion
      real, dimension(2,3) :: coords
   end type VoxelRegion

   type, private :: Cell
      real, dimension(3) :: v
   end type Cell

   type, private :: json_value_ptr
      type(json_value), pointer :: p
   end type
contains
   function readProblemDescription(filename) result (res)
      use, intrinsic :: iso_fortran_env , only: error_unit

      character (len=*), intent(in) :: filename
      type(Parseador) :: res !! Problem Description

      type(json_file) :: json
      type(json_core) :: core
      type(json_value), pointer :: root

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

      call json%get_core(core)
      call json%get('.', root)

      call initializeProblemDescription(res)
      res%general = readGeneral(core, root)
      res%despl = readGrid(core, root)
      res%front = readBoundary(core, root)
      res%plnSrc = readPlanewaves(core, root)
      res%sonda = readProbes(core, root)
   end function

   subroutine getRealVec(core, place, path, dest)
      type(json_core) :: core
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

   subroutine setRealIfFound(core, place, path, dest, default)
      type(json_core) :: core
      type(json_value), pointer :: place
      character(kind=CK, len=*) :: path

      real (kind=RK), intent(inout) :: dest
      real (kind=RK), optional :: default
      logical :: found
      real (kind=RK) :: val

      call core%get(place, path, val, found)
      if (found) then
         dest = val
      else
         dest = default
      endif
   end subroutine

   function getVoxelRegion(core, place) result (res)
      type(json_core) :: core
      type(json_value), pointer :: place
      type(VoxelRegion) :: res

      integer :: i, n
      type(json_value), pointer :: coordEntry, voxelRegionEntry
      real (kind=RK), pointer :: vec(:)

      call core%get(place, J_CELL_REGION, voxelRegionEntry)
      do i = 1, core%count(voxelRegionEntry)
         call core%get_child(voxelRegionEntry, i, coordEntry)
         call getRealVec(core, coordEntry, '.', vec)
         if (size(vec) /= 3) then
            stop "Voxel regions are defined by two numerical vectors of size 3."
         end if
         res%coords(i,:) = vec(1:3)
      end do
   end function

   function jsonValueFilterByKeyValue(core, srcs, key, value) result (out)
      type(json_value_ptr), allocatable :: out(:)
      type(json_core) :: core
      character (kind=JSON_CK, len=*) :: key, value
      type(json_value), pointer :: srcs, src
      character (kind=JSON_CK, len=:), allocatable :: type
      integer :: i, j, n
      logical :: found

      n = 0
      do i = 1, core%count(srcs)
         call core%get_child(srcs, i, src)
         call core%get(src, key, type, found)
         if(found .and. type == value) then
            n = n + 1
         end if
      end do

      allocate(out(n))
      j = 1
      do i = 1, core%count(srcs)
         call core%get_child(srcs, i, src)
         call core%get(src, key, type, found)
         if(found .and. type == value) then
            out(j)%p => src
            j = j + 1
         end if
      end do

   end function

   function readGrid(core, root) result (res)
      type(Desplazamiento) :: res
      type(json_core) :: core
      type(json_value), pointer :: root

      call core%get(root, J_GRID//'.'//J_NUMBER_OF_CELLS//'(1)',res%nX)
      call core%get(root, J_GRID//'.'//J_NUMBER_OF_CELLS//'(2)',res%nY)
      call core%get(root, J_GRID//'.'//J_NUMBER_OF_CELLS//'(3)',res%nZ)

      call getRealVec(core, root, J_GRID//'.'//J_STEPS//'.x', res%desX)
      call getRealVec(core, root, J_GRID//'.'//J_STEPS//'.y', res%desY)
      call getRealVec(core, root, J_GRID//'.'//J_STEPS//'.z', res%desZ)
   end function

   function readBoundary(core, root) result (res)
      type(Frontera) :: res
      type(json_core) :: core
      type(json_value), pointer :: root

      character(kind=json_CK,len=:), allocatable :: boundaryTypeLabel
      logical(LK) :: allLabelFound = .false.

      call core%get(root, J_BOUNDARY//'.'//J_ALL,  boundaryTypeLabel, allLabelFound)
      if (allLabelFound) then
         res%tipoFrontera(:) = labelToBoundaryType(boundaryTypeLabel)
         if (all(res%tipoFrontera == F_PML)) then
            !! TODO fill pml properties.
         end if
         return
      end if
   contains
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

   function readPlanewaves(core, root) result (res)
      type(PlaneWaves) :: res
      type(json_core) :: core
      type(json_value), pointer :: root

      type(json_value), pointer :: sources
      type(json_value_ptr), allocatable :: pws(:)
      integer :: i

      call core%get(root, J_SOURCES, sources)
      pws = jsonValueFilterByKeyValue(core, sources, J_SRC_TYPE, J_PW_TYPE)
      allocate(res%collection(size(pws)))
      do i=1, size(pws)
         res%collection(i) = readPlanewave(core, pws(i)%p)
      end do
   contains
      function readPlanewave(core, pw) result (res)
         type(PlaneWave) :: res
         type(json_core) :: core
         type(json_value), pointer :: pw

         character (len=:), allocatable :: label
         type(VoxelRegion) :: region
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

         region = getVoxelRegion(core, pw)
         res%coor1 = region%coords(1,:)
         res%coor2 = region%coords(2,:)

         res%isRC = .false.
         res%nummodes = 1
         res%incertmax = 0.0
      end function
   end function

   function readProbes(core, root) result (res)
      type(MasSondas) :: res
      type(json_core) :: core
      type(json_value), pointer :: root

      type(json_value), pointer :: probes, probe
      integer :: i

      call core%get(root, J_PROBES, probes)
      allocate(res%collection(core%count(probes)))
      do i=1, core%count(probes)
         call core%get_child(probes, i, probe)
         res%collection(i) = readProbe(core, probe)
      end do

   contains
      function readProbe(core, p) result (res)
         type(MasSonda) :: res
         type(json_core) :: core
         type(json_value), pointer :: p, directions

         integer :: i, j
         character (len=:), allocatable :: typeLabel, tag
         character(kind=CK,len=1), dimension(:), allocatable :: dirLabels
         type(Cell), dimension(:), allocatable :: cells
         
         call core%get(p, J_PR_OUTPUT_NAME, tag)
         res%outputrequest = trim(adjustl(tag))
         
         call core%get(p, J_PR_TYPE, typeLabel)
         
         call getDomain(core, p, res)
         
         call core%get(p, J_PR_DIRECTIONS, directions)
         call getCells(core, p, cells) 
         allocate(res%cordinates(size(cells) * core%count(directions)))
         do i = 1, size(cells)
            res%cordinates(i)%tag = tag
            res%cordinates(i)%Xi = int (cells(i)%v(1))
            res%cordinates(i)%Yi = int (cells(i)%v(2))
            res%cordinates(i)%Zi = int (cells(i)%v(3))
            call core%get(p, J_PR_DIRECTIONS, dirLabels)
            do j = 1, core%count(directions)
               res%cordinates(i)%Or = strToProbeType(typeLabel, dirLabels(i))
            end do
         end do


      end function

      subroutine getDomain(core, p, res)
         type(MasSonda), intent(inout) :: res
         type(json_core) :: core
         type(json_value), pointer :: p, domain

         character(kind=CK,len=:),allocatable :: fn
         
         character (len=:), allocatable :: domainType
         logical :: found
         real :: val

         call core%get(p, J_PR_DOMAIN, domain)
         call core%get(domain, J_PR_TYPE, domainType)
         res%type2 = strToDomainType(domainType)

         call setRealIfFound(core, domain, J_PR_DOMAIN_TIME_START, res%tstart, default=0.0)
         call setRealIfFound(core, domain, J_PR_DOMAIN_TIME_STOP,  res%tstop,  default=0.0)
         call setRealIfFound(core, domain, J_PR_DOMAIN_TIME_STEP,  res%tstep,  default=0.0)
         call setRealIfFound(core, domain, J_PR_DOMAIN_FREQ_START, res%fstart, default=0.0)
         call setRealIfFound(core, domain, J_PR_DOMAIN_FREQ_STOP,  res%fstop,  default=0.0)
         call setRealIfFound(core, domain, J_PR_DOMAIN_FREQ_STEP,  res%fstep,  default=0.0)

         call core%get(domain, J_PR_FILENAME, fn, found)
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
         character (len=1) :: dirLabel
         select case (typeLabel)
          case (J_PR_ELECTRIC)
            select case (dirLabel)
             case (J_DIR_X)
               res = NP_COR_EX
             case (J_DIR_Y)
               res = NP_COR_EY
             case (J_DIR_Z)
               res = NP_COR_EZ
            end select
          case (J_PR_MAGNETIC)
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

   function readGeneral(json, root) result (res)
      type(NFDEGeneral) :: res
      type(json_core) :: json
      type(json_value), pointer :: root

      call json%get(root, J_GENERAL//'.'//J_TIME_STEP,       res%dt)
      call json%get(root, J_GENERAL//'.'//J_NUMBER_OF_STEPS, res%nmax)
   end function


end module
