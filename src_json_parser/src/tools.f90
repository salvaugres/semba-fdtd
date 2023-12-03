module tools_mod

   use mesh_mod
   use json_module
   
   use, intrinsic :: iso_fortran_env , only: error_unit

   implicit none

contains
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

   function getCellRegion(core, place) result (res)
      type(json_core) :: core
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

   function getSimpleCells(core, place, path) result(res)
      type(json_core) :: core
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
         call getRealVec(core, coordEntry, '.', vec)
         if (size(vec) /= 3) then
            stop "Cells are defined by a vector of size 3."
         end if
         res(i)%v = vec
      end do
   end function

   function getCellsFromNodeElementIds(core, mesh, elementIdsPlace, coordIds) result(res)
      type(json_core) :: core
      type(mesh_t) :: mesh
      type(Cell), dimension(:), allocatable :: res
      type(json_value), pointer :: elementIdsPlace
      integer, dimension(:), allocatable, optional, intent(out) :: coordIds
      integer, dimension(:), allocatable :: ids

      type(node_t) :: node
      type(coordinate_t) :: coordinate
      logical :: idsFound, nodeFound, coordinateFound
      integer :: i

      call core%get(elementIdsPlace, J_ELEMENTIDS, ids, found=idsFound)
      if (.not. idsFound) then
         allocate(res(0))
         return
      end if

      allocate(res(size(ids)))
      if (present(coordIds)) allocate(coordIds(size(ids)))
      do i = 1, size(ids)
         nodeFound = .false.
         coordinateFound = .false.
         node = mesh%getNode(ids(i), nodeFound)
         if (nodeFound) coordinate = mesh%getCoordinate(node%coordIds(1), coordinateFound)
         if (coordinateFound) res(i)%v = coordinate%position
         if (present(coordIds)) coordIds(i) = node%coordIds(1)
      end do
   end function

   function jsonValueFilterByKeyValues(this, srcs, key, values) result (out)
      class(parser_t) :: this
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

   function jsonValueFilterByKeyValue(core, place, key, value) result (out)
      type(json_core) :: core
      type(json_value_ptr), allocatable :: out(:)
      character (kind=JSON_CK, len=*) :: key, value
      type(json_value), pointer :: place, src
      character (kind=JSON_CK, len=:), allocatable :: type
      integer :: i, j, n
      logical :: found

      n = 0
      do i = 1, this%core%count(place)
         call this%core%get_child(place, i, src)
         call this%core%get(src, key, type, found)
         if(found .and. type == value) then
            n = n + 1
         end if
      end do

      allocate(out(n))
      j = 1
      do i = 1, this%core%count(place)
         call this%core%get_child(place, i, src)
         call this%core%get(src, key, type, found)
         if(found .and. type == value) then
            out(j)%p => src
            j = j + 1
         end if
      end do
   end function


end module
