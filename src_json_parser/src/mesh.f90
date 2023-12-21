module mesh_mod
   use fhash, only: fhash_tbl_t, key=>fhash_key
   use cells_mod

   integer, private, parameter  ::  MAX_LINE = 256

   type :: element_t
      integer, dimension(:), allocatable :: coordIds
   end type

   type, public, extends(element_t) :: node_t
      ! coordIds must be size 1.
   end type

   type, public, extends(element_t) :: polyline_t
      ! coordIds must be size >1.
   end type

   type, public :: coordinate_t
      real, dimension(3) :: position
   end type

   type, public :: mesh_t
      private
      type(fhash_tbl_t) :: coordinates ! Map of CoordinateIds to relative coordinates.
      type(fhash_tbl_t) :: elements    ! Map of ElementIds to elements/cellsRegions.
   contains
      procedure :: addCoordinate => mesh_addCoordinate
      procedure :: getCoordinate => mesh_getCoordinate

      procedure :: addElement => mesh_addElement
      procedure :: getNode => mesh_getNode
      procedure :: getPolyline => mesh_getPolyline

      procedure :: addCellRegion  => mesh_addCellRegion
      procedure :: getCellRegion  => mesh_getCellRegion
      procedure :: getCellRegions => mesh_getCellRegions
   end type

   public :: convertPolylineToLinels
   public :: convertNodeToPixels
   
   integer, public, parameter :: FIRST_CELL_START = 1
   private

contains
   ! __________________________________________________________________
   ! Mesh procedures
   subroutine mesh_addCoordinate(this, id, coordinate)
      class(mesh_t) :: this
      integer, intent(in) :: id
      type(coordinate_t), intent(in) :: coordinate
      call this%coordinates%set(key(id), value=coordinate)
   end subroutine

   subroutine mesh_addElement(this, id, e)
      class(mesh_t) :: this
      integer, intent(in) :: id
      class(element_t), intent(in) :: e
      call this%elements%set(key(id), value=e)
   end subroutine

   subroutine mesh_addCellRegion(this, id, e)
      class(mesh_t) :: this
      integer, intent(in) :: id
      class(cell_region_t), intent(in) :: e
      call this%elements%set(key(id), value=e)
   end subroutine

   function mesh_getCoordinate(this, id, found) result(res)
      class(mesh_t) :: this
      type(coordinate_t) :: res
      integer, intent(in) :: id
      integer :: stat
      logical, intent(out), optional :: found
      class(*), allocatable :: d

      if (present(found)) found = .false.

      call this%coordinates%get_raw(key(id), d, stat)
      if (stat /= 0) return

      select type(d)
       type is (coordinate_t)
         res = d
         if (present(found)) found = .true.
      end select

   end function

   function mesh_getNode(this, id, found) result(res)
      class(mesh_t) :: this
      type(node_t) :: res
      integer, intent(in) :: id
      logical, optional, intent(out) :: found
      integer :: status
      class(*), allocatable :: d

      if (present(found)) found = .false.
      call this%elements%get_raw(key(id), d, status)
      if (status /= 0) return

      select type(d)
       type is (node_t)
         res = d
         if (present(found)) found = .true.
      end select

   end function

   function mesh_getPolyline(this, id, found) result(res)
      class(mesh_t) :: this
      type(polyline_t) :: res
      integer, intent(in) :: id
      integer :: stat
      logical, intent(out), optional :: found
      class(*), allocatable :: d

      if (present(found)) found = .false.
      call this%elements%get_raw(key(id), d, stat)
      if (stat /= 0) return

      select type(d)
       type is (polyline_t)
         res = d
         if (present(found)) found = .true.
      end select

   end function

   function mesh_getCellRegion(this, id, found) result (res)
      class(mesh_t) :: this
      type(cell_region_t) :: res
      integer, intent(in) :: id
      integer :: stat
      logical, intent(out), optional :: found
      class(*), allocatable :: d

      if (present(found)) found = .false.
      call this%elements%get_raw(key(id), d, stat)
      if (stat /= 0) return

      select type(d)
       type is (cell_region_t)
         res = d
         if (present(found)) found = .true.
      end select
   
   end function

   function mesh_getCellRegions(this, ids) result (res)
      class(mesh_t) :: this
      type(cell_region_t), dimension(:), allocatable :: res
      integer, dimension(:), intent(in) :: ids
      type(cell_region_t) :: cR
      logical :: found
      integer :: i

      allocate(res(0))
      do i = 1, size(ids)
         cR = this%getCellRegion(ids(i), found)
         if (found) res = [res, cR]
      end do
   
   end function

   ! __________________________________________________________________
   ! Aux functions
   function convertPolylineToLinels(mesh, polyline) result(res)
      type(linel_t), dimension(:), allocatable :: res
      class(mesh_t), intent(in) :: mesh
      type(polyline_t), intent(in) :: polyline
      type(coordinate_t) :: iC, eC, mC
      integer :: i, j, dir, lastSegment, nLinelsInSegment
      integer, dimension(3) :: segment

      if (.not. areAllSegmentsStraight(polyline)) then
         allocate(res(0))
         return
      end if

      allocate(res(countSegments(polyline)))

      lastSegment = 1
      do i = 1, size(polyline%coordIds)-1
         iC = mesh%getCoordinate(polyline%coordIds(i))
         eC = mesh%getCoordinate(polyline%coordIds(i+1))
         dir = varyingDirection(iC, eC)
         segment = int(eC%position - iC%position)
         nLinelsInSegment = abs(segment(dir))
         segment = segment / nLinelsInSegment
         do j = 1, nLinelsInSegment
            mC%position = iC%position + segment * (real(j-1) + 0.5)
            res(lastSegment)%cell = floor(mc%position) + FIRST_CELL_START
            res(lastSegment)%orientation = dir
            if (j == 1) then
               res(lastSegment)%tag = trim(intToString(polyline%coordIds(i)))
            end if
            if (i == size(polyline%coordIds)-1 .and. j == nLinelsInSegment) then
               res(lastSegment)%tag = trim(intToString(polyline%coordIds(i+1)))
            end if
            lastSegment = lastSegment + 1
         end do

      end do

   contains
      integer function countSegments(pl)
         class(polyline_t) :: pl
         type(coordinate_t) :: iC, eC
         integer :: dir
         countSegments = 0
         do i = 1, size(polyline%coordIds)-1
            iC = mesh%getCoordinate(polyline%coordIds(i))
            eC = mesh%getCoordinate(polyline%coordIds(i+1))
            dir = varyingDirection(iC, eC)
            countSegments = countSegments + int(abs(eC%position(dir) - iC%position(dir)))
         end do
      end function

      function areAllSegmentsStraight(pl) result(res)
         logical :: res
         class(polyline_t) :: pl
         type(coordinate_t) :: iC, eC
         integer :: i, d
         integer :: numberOfVaryingDirections

         do i = 1, size(polyline%coordIds)-1
            iC = mesh%getCoordinate(polyline%coordIds(i))
            eC = mesh%getCoordinate(polyline%coordIds(i+1))
            numberOfVaryingDirections = 0
            do d = DIR_X, DIR_Z
               if (iC%position(d) /= eC%position(d)) then
                  numberOfVaryingDirections = numberOfVaryingDirections + 1
               end if
            end do
            if (numberOfVaryingDirections /= 1) then
               res = .false.
               return
            end if
         end do

         res = .true.
      end function
   end function

   function convertNodeToPixels(mesh, node) result(res)
      type(pixel_t), dimension(:), allocatable :: res
      class(mesh_t), intent(in) :: mesh
      type(node_t), intent(in) :: node

      type(coordinate_t) :: c
      logical :: coordFound

      c = mesh%getCoordinate(node%coordIds(1), found=coordFound)
      if (.not. coordFound) then
         allocate(res(0))
         return
      end if
      allocate(res(1))
      res(1)%cell = c%position + FIRST_CELL_START
      res(1)%tag = trim(intToString(node%coordIds(1)))
   end function

   function intToString(i) result(res)
      character (len=MAX_LINE) :: res
      integer, intent(in) :: i
      write(res, '(i10)') i
      res = trim(adjustl(res))
   end function

   integer function varyingDirection(a, b)
      type(coordinate_t), intent(in) :: a, b
      do varyingDirection = DIR_X, DIR_Z
         if (a%position(varyingDirection) /= b%position(varyingDirection)) return
      end do
   end function

end module
