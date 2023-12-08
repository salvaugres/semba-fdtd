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
         cellRegion = mesh%getCellRegion(ids(i), cellRegionFound)
         if (nodeFound) then 
            pixels = convertNodeToPixels(mesh, node)
         else if (cellRegionFound) then
            pixels = cellRegion%toPixels()
         else
            stop "Error converting pixels."   
         end if
         if (size(pixels) /= 1) then 
            stop "Each element id must contain a single pixel"
         end if
         res(i) = pixels(1) 
      end do
      
   end function

   function jsonValueFilterByKeyValues(core, srcs, key, values) result (out)
      type(json_core) :: core
      type(json_value_ptr), allocatable :: out(:)
      type(json_value), pointer :: srcs

      character (kind=JSON_CK, len=*) :: key
      character (kind=JSON_CK, len=*), dimension(:) :: values

      type(json_value_ptr), dimension (:), allocatable :: foundEntries, tmp
      integer :: i, lastEntry, nEntries

      do i = 1, size(values)
         if (allocated(foundEntries)) deallocate(foundEntries)
         foundEntries = jsonValueFilterByKeyValue(core, srcs, key, values(i))
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

   function jsonValueFilterByKeyValue(core, place, key, value) result (out)
      type(json_core) :: core
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


end module