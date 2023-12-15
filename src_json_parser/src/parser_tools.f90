module parser_tools_mod
   use labels_mod
   use mesh_mod
   use cells_mod
   use json_module
   use json_kinds

   use, intrinsic :: iso_fortran_env , only: error_unit

   implicit none

   integer, private, parameter :: J_ERROR_NUMBER = 1

   type :: json_value_ptr
      type(json_value), pointer :: p
   end type
contains
   subroutine addCellRegionsAsCoords(res, cellRegions, cellType)
      type(coords), dimension(:), pointer :: res
      type(cell_region_t), dimension(:), allocatable, intent(in) :: cellRegions
      integer, intent(in), optional :: cellType
      type(cell_interval_t), dimension(:), allocatable :: intervals
      type(coords), dimension(:), allocatable :: coords
      integer :: i

      allocate(intervals(0))
      do i = 1, size(cellRegions)
         if (present(cellType)) then
            intervals = [intervals, cellRegions(i)%getIntervalsOfType(cellType)]
         else 
            intervals = [intervals, cellRegions(i)%intervals]
         end if
      end do
      coords = cellIntervalsToCoords(intervals)
      allocate(res(size(coords)))
      res = coords
   end subroutine

   function getPixelsFromElementIds(mesh, ids) result(res)
      type(pixel_t), dimension(:), allocatable :: res
      type(mesh_t), intent(in) :: mesh
      integer, dimension(:), allocatable, intent(in) :: ids

      type(node_t) :: node
      type(cell_region_t) :: cellRegion
      type(pixel_t), dimension(:), allocatable :: pixels
      logical :: nodeFound
      logical :: cellRegionFound
      integer :: i

      allocate(res(size(ids)))
      do i = 1, size(ids)
         node = mesh%getNode(ids(i), nodeFound)
         if (nodeFound) then
            pixels = convertNodeToPixels(mesh, node)
         else
            stop "Error converting pixels."
         end if
         if (size(pixels) /= 1) then
            stop "Each element id must contain a single pixel"
         end if
         res(i) = pixels(1)
      end do

   end function

   function jsonValueFilterByKeyValues(core, srcs, key, values) result (res)
      type(json_core) :: core
      type(json_value_ptr), dimension(:), allocatable :: res
      type(json_value), pointer :: srcs

      character (kind=JSON_CK, len=*) :: key
      character (kind=JSON_CK, len=*), dimension(:) :: values

      type(json_value_ptr), dimension (:), allocatable :: foundEntries
      integer :: i, lastEntry, nEntries

      allocate(res(0))
      do i = 1, size(values)
         foundEntries = jsonValueFilterByKeyValue(core, srcs, key, values(i))
         if (size(foundEntries) /= 0) then
            res = [res, foundEntries]
         end if
      end do
   end function

   function jsonValueFilterByKeyValue(core, place, key, value) result (res)
      type(json_core) :: core
      type(json_value_ptr), allocatable :: res(:)
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

      allocate(res(n))
      j = 1
      do i = 1, core%count(place)
         call core%get_child(place, i, src)
         call core%get(src, key, type, found)
         if(found .and. type == value) then
            res(j)%p => src
            j = j + 1
         end if
      end do
   end function


end module
