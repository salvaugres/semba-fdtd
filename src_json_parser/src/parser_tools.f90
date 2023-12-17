module parser_tools_mod
   use labels_mod
   use mesh_mod
   use cells_mod
   use json_module
   use json_kinds
   use NFDETypes, only: coords, coords_scaled

   use, intrinsic :: iso_fortran_env , only: error_unit

   implicit none

   integer, private, parameter :: J_ERROR_NUMBER = 1

   type :: json_value_ptr
      type(json_value), pointer :: p
   end type
contains

   subroutine addCellRegionsAsCoords(res, cellRegions, cellType)
      type(coords), dimension(:), pointer :: res
      type(cell_region_t), dimension(:), intent(in) :: cellRegions
      integer, intent(in), optional :: cellType
      type(cell_interval_t), dimension(:), allocatable :: intervals
      type(coords), dimension(:), allocatable :: cs
      integer :: i

      allocate(intervals(0))
      do i = 1, size(cellRegions)
         if (present(cellType)) then
            intervals = [intervals, cellRegions(i)%getIntervalsOfType(cellType)]
         else 
            intervals = [intervals, cellRegions(i)%intervals]
         end if
      end do
      cs = cellIntervalsToCoords(intervals)
      allocate(res(size(cs)))
      res = cs
   end subroutine

   subroutine addCellRegionsAsScaledCoords(res, cellRegions, cellType)
      type(coords_scaled), dimension(:), pointer :: res
      type(cell_region_t), dimension(:), intent(in) :: cellRegions
      integer, intent(in), optional :: cellType
      type(cell_interval_t), dimension(:), allocatable :: intervals
      type(coords), dimension(:), pointer :: cs
      integer :: i
      
      if (present(cellType)) then
         call addCellRegionsAsCoords(cs, cellRegions, cellType)
      else
         call addCellRegionsAsCoords(cs, cellRegions)
      end if

      allocate(res(size(cs)))
      res(:)%Xi = cs(:)%Xi
      res(:)%Xe = cs(:)%Xe
      res(:)%Yi = cs(:)%Yi
      res(:)%Ye = cs(:)%Ye
      res(:)%Zi = cs(:)%Zi
      res(:)%Ze = cs(:)%Ze
      res(:)%Or = cs(:)%Or
      res(:)%tag = cs(:)%tag
   end subroutine

   

   function cellIntervalsToCoords(ivls) result(res)
      type(coords), dimension(:), allocatable :: res
      type(cell_interval_t), dimension(:), intent(in) :: ivls
      integer :: i

      allocate(res(size(ivls)))
      do i = 1, size(ivls)
         res(i)%Or = ivls(i)%getOrientation()
         call convertInterval(res(i)%Xi, res(i)%Xe, ivls(i), DIR_X)
         call convertInterval(res(i)%Yi, res(i)%Ye, ivls(i), DIR_Y)
         call convertInterval(res(i)%Zi, res(i)%Ze, ivls(i), DIR_Z)
      end do
   contains
      subroutine convertInterval(xi, xe, interval, dir)
         integer, intent(out) :: xi, xe
         type(cell_interval_t), intent(in) :: interval
         integer, intent(in) :: dir
         integer :: a, b
         a = interval%ini%cell(dir) + FIRST_CELL_START
         b = interval%end%cell(dir) + FIRST_CELL_START
         if (a < b) then
            xi = a 
            xe = b - 1
         else if (a == b) then
            xi = a
            xe = b
         else
            xi = b + 1
            xe = a - 1
         end if

      end subroutine
   end function

   function coordsToScaledCoords(cs) result(res)
      type(coords), dimension(:), intent(in) :: cs
      type(coords_scaled), dimension(:), allocatable :: res
      ! TODO
   end function

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
