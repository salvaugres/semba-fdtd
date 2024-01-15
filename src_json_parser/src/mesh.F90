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

      procedure :: arePolylineSegmentsStructured => mesh_arePolylineSegmentsStructured
      procedure :: convertPolylineToLinels => mesh_convertPolylineToLinels
      procedure :: convertNodeToPixels => mesh_convertNodeToPixels
   end type

   integer, public, parameter :: FIRST_CELL_START = 1

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

   function mesh_arePolylineSegmentsStructured(this, pl) result(res)
      logical :: res
      class(mesh_t) :: this
      type(polyline_t) :: pl
      type(coordinate_t) :: iC, eC
      integer :: i, d
      integer :: numberOfVaryingDirections

      do i = 1, size(pl%coordIds)-1
         iC = this%getCoordinate(pl%coordIds(i))
         eC = this%getCoordinate(pl%coordIds(i+1))
         if (any(floor(iC%position) /= iC%position) .or. any(floor(eC%position) /= eC%position)) then
            res = .false.
            return
         end if 

         numberOfVaryingDirections = 0
         do d = DIR_X, DIR_Z
            if (iC%position(d) /= eC%position(d)) then
               numberOfVaryingDirections = numberOfVaryingDirections + 1
            end if
         end do
         if (numberOfVaryingDirections > 1) then
            res = .false.
            return
         end if
      end do

      res = .true.
   end function

   function mesh_convertPolylineToLinels(this, pl) result(res)
      type(linel_t), dimension(:), allocatable :: res
      class(mesh_t), intent(in) :: this
      type(polyline_t), intent(in) :: pl
      type(cell_interval_t) :: interval
      type(coordinate_t) :: iC, eC, mC
      integer :: i, j, lastSegment, nLinelsInSegment
      integer, dimension(3) :: segment

      if (.not. this%arePolylineSegmentsStructured(pl)) then
         allocate(res(0))
         return
      end if

      allocate(res(countSegments(pl)))
      if (size(res) == 0) return

      lastSegment = 1
      do i = 1, size(pl%coordIds)-1
         iC = this%getCoordinate(pl%coordIds(i))
         eC = this%getCoordinate(pl%coordIds(i+1))
         interval%ini%cell = int(iC%position)
         interval%end%cell = int(eC%position)
         if (any(iC%position /= eC%position)) then
            segment = (interval%end%cell - interval%ini%cell) / interval%getSize()
            
            res(lastSegment)%tag = pl%coordIds(i)
            do j = 1, interval%getSize()
               mC%position = iC%position + segment * (real(j-1) + 0.5)
               res(lastSegment)%cell = floor(mc%position) + FIRST_CELL_START
               res(lastSegment)%orientation = interval%getOrientation()
               lastSegment = lastSegment + 1
            end do
         end if
      end do

      res(1)%tag             = pl%coordIds( 1 )
      res(lastSegment-1)%tag = pl%coordIds( size(pl%coordIds) )
      
   contains
      integer function countSegments(pl)
         class(polyline_t) :: pl
         type(cell_interval_t) :: interval
         type(coordinate_t) :: iC, eC
         
         ! Assumes that polyline is structured.
         countSegments = 0
         do i = 1, size(pl%coordIds)-1
            iC = this%getCoordinate(pl%coordIds(i))
            eC = this%getCoordinate(pl%coordIds(i+1))
            interval%ini%cell = int(iC%position)
            interval%end%cell = int(eC%position)
            countSegments = countSegments + interval%getSize()
         end do
      end function
   end function

   function mesh_convertNodeToPixels(this, node) result(res)
      type(pixel_t), dimension(:), allocatable :: res
      class(mesh_t), intent(in) :: this
      type(node_t), intent(in) :: node

      type(coordinate_t) :: c
      logical :: coordFound

      c = this%getCoordinate(node%coordIds(1), found=coordFound)
      if (.not. coordFound) then
         allocate(res(0))
         return
      end if
      allocate(res(1))
      res(1)%cell = c%position + FIRST_CELL_START
      res(1)%tag = node%coordIds(1)
   end function
   
end module
