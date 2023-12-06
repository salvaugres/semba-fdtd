module mesh_mod
   use fhash, only: fhash_tbl_t, key=>fhash_key
   use cells_mod
   
   type :: element_t
      integer, dimension(:), allocatable :: coordIds
   end type

   type, extends(element_t) :: node_t
      ! coordIds must be size 1.
   end type

   type, extends(element_t) :: polyline_t
      ! coordIds must be size >1.
   end type

   type, public :: coordinate_t
      real, dimension(3) :: position
   end type

   type :: mesh_t
      private
      type(fhash_tbl_t) :: coordinates ! Map of CoordinateIds to relative coordinates.
      type(fhash_tbl_t) :: elements    ! Map of ElementIds to elements/cells.
   contains
      procedure :: addCoordinate
      procedure :: getCoordinate

      procedure :: addElement
      procedure :: getNode
      procedure :: getPolyline

      procedure :: convertPolylineToLinels
   end type

   integer, private, parameter  ::  MAX_LINE = 256 
contains
   subroutine addCoordinate(this, id, coordinate)
      class(mesh_t) :: this
      integer, intent(in) :: id
      type(coordinate_t), intent(in) :: coordinate
      call this%coordinates%set(key(id), value=coordinate)
   end subroutine

   subroutine addElement(this, id, e)
      class(mesh_t) :: this
      integer, intent(in) :: id
      class(element_t), intent(in) :: e
      call this%elements%set(key(id), value=e)
   end subroutine

   function getCoordinate(this, id, found) result(res)
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

   function getNode(this, id, found) result(res)
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

   function getPolyline(this, id, found) result(res)
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

   function convertPolylineToLinels(this, polyline) result(res)
      type(linel_t), dimension(:), allocatable :: res
      class(mesh_t) :: this
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
         iC = this%getCoordinate(polyline%coordIds(i))
         eC = this%getCoordinate(polyline%coordIds(i+1))
         dir = varyingDirection(iC, eC)
         segment = int(eC%position - iC%position)
         nLinelsInSegment = abs(segment(dir))
         segment = segment / nLinelsInSegment
         do j = 1, nLinelsInSegment
            mC%position = iC%position + segment * (real(j-1) + 0.5)
            res(lastSegment)%v = floor(mc%position) + [1.0, 1.0, 1.0]
            res(lastSegment)%orientation = dir
            if (j == 1) then
               res(lastSegment)%tag = trim(toString(polyline%coordIds(i)))               
            end if 
            if (i == size(polyline%coordIds)-1 .and. j == nLinelsInSegment) then
               res(lastSegment)%tag = trim(toString(polyline%coordIds(i+1)))
            end if 
            lastSegment = lastSegment + 1
         end do

      end do

   contains
      function toString(i) result(res)
         character (len=MAX_LINE) :: res
         integer, intent(in) :: i
         write(res, '(i10)') i
         res = trim(adjustl(res))
      end function

      integer function countSegments(pl)
         class(polyline_t) :: pl
         type(coordinate_t) :: iC, eC
         integer :: dir
         countSegments = 0
         do i = 1, size(polyline%coordIds)-1
            iC = this%getCoordinate(polyline%coordIds(i))
            eC = this%getCoordinate(polyline%coordIds(i+1))
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
            iC = this%getCoordinate(polyline%coordIds(i))
            eC = this%getCoordinate(polyline%coordIds(i+1))
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

      integer function varyingDirection(a, b)
         type(coordinate_t), intent(in) :: a, b
         do varyingDirection = DIR_X, DIR_Z
            if (a%position(varyingDirection) /= b%position(varyingDirection)) return
         end do
      end function
   end function
end module
