module smbjson

   use NFDETypes
   use NFDETypes_extension

   use json_module
   use json_kinds

   implicit none
   private
   public :: readProblemDescription

   ! LABELS
   ! -- shared labels
   character (len=*), parameter :: J_MAGNITUDE_FILE = "magnitudeFile"
   character (len=*), parameter :: J_VOXEL_REGION = "voxelRegion"
   character (len=*), parameter :: J_TYPE = "type"

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
   ! type(Planewave)
   character (len=*), parameter :: J_TYPE_PLANEWAVE = "planewave"
   character (len=*), parameter :: J_DIRECTION = "direction"
   character (len=*), parameter :: J_DIRECTION_THETA = "theta"
   character (len=*), parameter :: J_DIRECTION_PHI = "phi"
   character (len=*), parameter :: J_POLARIZATION = "polarization"
   character (len=*), parameter :: J_POLARIZATION_ALPHA = "alpha"
   character (len=*), parameter :: J_POLARIZATION_BETA  = "beta"
   
   type, public :: VoxelRegion
      real, dimension(3) :: c1, c2 
   end type VoxelRegion
contains

   subroutine getRealVecAndStore(json, place, dest)
      type(json_core) :: json
      character (len=*), intent(in) :: place
      real (kind=RK), dimension (:), pointer :: dest

      real(RK),dimension(:),allocatable :: vec
      logical :: found = .false.

      call json%get(place, vec, found)
      if (found) then
         allocate(dest(size(vec)))
         dest = vec
      endif
   end subroutine

   subroutine getVoxelRegion(json, place, res)
      type(json_core) :: json
      character (len=*), intent(in) :: place
      type(VoxelRegion), intent(out) :: res

      real (kind=RK), dimension(:), pointer:: c1, c2
      
      call getRealVecAndStore(json, place//'(1)', c1)
      call getRealVecAndStore(json, place//'(2)', c2)

      if (size(c1) /= 3 .or. size(c2) /= 3) then
         stop "Voxel regions are defined by two numerical vectors of size 3."
      end if

      res%c1 = c1
      res%c2 = c2

   end subroutine

   function jsonValueFilterByKeyValue(core, srcs, key, value) result(out)
      type(json_core) :: core
      type(json_core) :: out
      character (kind=JSON_CK, len=:), pointer :: key, value
      type(json_value), pointer :: srcs, src, outSrcs
      character (kind=JSON_CK, len=:), allocatable :: type
      integer :: i
      logical :: found

      call out%create_object(outSrcs, J_SOURCES)

      do i = 1, core%count(srcs)
         call core%get_child(srcs, i, src)
         call core%get(src, key, type, found)
         if(found .and. type == value) then
            call out%add(outSrcs, src)
         end if
      end do

   end function


   function readGrid(json) result (res)
      type(Desplazamiento) :: res
      type(json_file) :: json

      call json%get(J_GRID//'.'//J_NUMBER_OF_CELLS//'(1)',res%nX)
      call json%get(J_GRID//'.'//J_NUMBER_OF_CELLS//'(2)',res%nY)
      call json%get(J_GRID//'.'//J_NUMBER_OF_CELLS//'(3)',res%nZ)

      call getRealVecAndStore(json ,J_GRID//'.'//J_STEPS//'.x', res%desX)
      call getRealVecAndStore(json ,J_GRID//'.'//J_STEPS//'.y', res%desY)
      call getRealVecAndStore(json ,J_GRID//'.'//J_STEPS//'.z', res%desZ)
   end function

   function readBoundary(json) result (res)
      type(Frontera) :: res
      type(json_file) :: json

      character(kind=json_CK,len=:), allocatable :: boundaryTypeLabel
      logical(LK) :: allLabelFound = .false.

      call json%get(J_BOUNDARY//'.'//J_ALL,  boundaryTypeLabel, allLabelFound)
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

   function readPlanewaves(json) result (res)
      type(PlaneWaves) :: res
      type(json_file) :: json

      type(json_core) :: core, pws
      type(json_value), pointer :: sourceEntry, sources, pw
      integer(IK) :: nPlanewaves
      integer :: i

      call json%get_core(core)
      call json%get(J_SOURCES, sources)
      pws = jsonValueFilterByKeyValue(core, sources)
      allocate(res%collection(pws%count(J_SOURCES)))
      do i=1, pws%count(J_SOURCES)
         ! call core%get_child(srcs, i, pw)
         ! res%collection(i) = readPlanewave(core, pw)
      end do
   contains
      function readPlanewave(pws, pw) result (res)
         type(PlaneWave) :: res
         type(json_core) :: pws
         type(json_value), pointer :: pw

         type(VoxelRegion) :: region 
         call pws%get(pw, J_MAGNITUDE_FILE, res%nombre_fichero)
         call pws%get(pw, J_DIRECTION//'.'//J_DIRECTION_THETA, res%theta)
         call pws%get(pw, J_DIRECTION//'.'//J_DIRECTION_PHI, res%phi)
         call pws%get(pw, J_POLARIZATION//'.'//J_POLARIZATION_ALPHA, res%alpha)
         call pws%get(pw, J_POLARIZATION//'.'//J_POLARIZATION_BETA, res%beta)
         call getVoxelRegion(pw, J_VOXEL_REGION, region)
         res%coor1 = int (region%c1)
         res%coor2 = int (region%c2)
      end function

   end function


   function readGeneral(json) result (res)
      type(NFDEGeneral) :: res
      type(json_file) :: json

      call json%get(J_GENERAL//'.'//J_TIME_STEP,       res%dt)
      call json%get(J_GENERAL//'.'//J_NUMBER_OF_STEPS, res%nmax)
   end function

   function readProblemDescription(filename) result (res)
      use, intrinsic :: iso_fortran_env , only: error_unit

      character (len=*), intent(in) :: filename
      type(Parseador) :: res !! Problem Description

      type(json_file) :: json       !! the JSON structure read from the file

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

      call initializeProblemDescription(res)
      res%general = readGeneral(json)
      res%despl   = readGrid(json)
      res%front   = readBoundary(json)
      res%plnSrc  = readPlanewaves(json)
   end function
end module
