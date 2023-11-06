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
   character (len=*), parameter :: J_ATTRIBUTE = "attribute"
   character (len=*), parameter :: J_DIRECTION = "direction"
   character (len=*), parameter :: J_DIRECTION_THETA = "theta"
   character (len=*), parameter :: J_DIRECTION_PHI = "phi"
   character (len=*), parameter :: J_POLARIZATION = "polarization"
   character (len=*), parameter :: J_POLARIZATION_ALPHA = "alpha"
   character (len=*), parameter :: J_POLARIZATION_BETA  = "beta"
   
   type, public :: VoxelRegion_t
      real, dimension(2,3) :: coords
   end type VoxelRegion_t

   type, private :: json_value_ptr
      type(json_value), pointer :: p
   end type
contains
   function readProblemDescription(filename) result (res)
      use, intrinsic :: iso_fortran_env , only: error_unit

      character (len=*), intent(in) :: filename
      type(Parseador) :: res !! Problem Description

      type(json_file) :: json       !! the JSON structure read from the file
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
   end function

   subroutine getRealVecAndStore(core, place, path, dest)
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

   function getVoxelRegion(core, place) result (res)
      type(json_core) :: core
      type(json_value), pointer :: place
      type(VoxelRegion_t) :: res

      integer :: i, n
      type(json_value), pointer :: coordEntry, voxelRegionEntry
      real (kind=RK), pointer :: vec(:)
      
      call core%get(place, J_VOXEL_REGION, voxelRegionEntry)
      do i = 1, core%count(voxelRegionEntry)
         call core%get_child(voxelRegionEntry, i, coordEntry)
         call getRealVecAndStore(core, coordEntry, '.', vec)
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

      call getRealVecAndStore(core, root, J_GRID//'.'//J_STEPS//'.x', res%desX)
      call getRealVecAndStore(core, root, J_GRID//'.'//J_STEPS//'.y', res%desY)
      call getRealVecAndStore(core, root, J_GRID//'.'//J_STEPS//'.z', res%desZ)
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
      integer(IK) :: nPlanewaves
      integer :: i

      call core%get(root, J_SOURCES, sources)
      pws = jsonValueFilterByKeyValue(core, sources, J_TYPE, J_TYPE_PLANEWAVE)      
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
         type(VoxelRegion_t) :: region 
         logical :: found

         call core%get(pw, J_MAGNITUDE_FILE, label)
         res%nombre_fichero = trim(adjustl(label))

         call core%get(pw, J_ATTRIBUTE, label, found)
         if (found) then
            res%atributo = trim(adjustl(label))
         else 
            res%atributo = ""
         endif

         call core%get(pw, J_DIRECTION//'.'//J_DIRECTION_THETA, res%theta)
         call core%get(pw, J_DIRECTION//'.'//J_DIRECTION_PHI, res%phi)
         call core%get(pw, J_POLARIZATION//'.'//J_POLARIZATION_ALPHA, res%alpha)
         call core%get(pw, J_POLARIZATION//'.'//J_POLARIZATION_BETA, res%beta)
         
         region = getVoxelRegion(core, pw)
         res%coor1 = region%coords(1,:)
         res%coor2 = region%coords(2,:)
         
         res%isRC = .false.
         res%nummodes = 1
         res%incertmax = 0.0
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
