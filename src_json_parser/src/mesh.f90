module mesh_mod
   use fhash, only: fhash_tbl_t, key=>fhash_key

   integer, private , parameter :: DIR_X = 1
   integer, private , parameter :: DIR_Y = 2
   integer, private , parameter :: DIR_Z = 3

   type :: cell_t
      real, dimension(3) :: v
      character (len=:), allocatable :: tag
   end type cell_t

   type, extends(cell_t) :: pixel_t
   end type

   type, extends(cell_t) :: linel_t
      integer :: orientation ! DIR_X, DIR_Y, DIR_Z
   end type

   type :: cell_region_t
      type(cell_t), dimension(2) :: coords
   end type cell_region_t

   type :: element_t
      integer, dimension(:), allocatable :: coordIds
   end type

   type, extends(element_t) :: node_t
   end type

   type, extends(element_t) :: polyline_t

   end type

   type, public :: coordinate_t
      real, dimension(3) :: position
   end type

   type :: mesh_t
      private
      type(fhash_tbl_t) :: coordinates
      type(fhash_tbl_t) :: elements
   contains
      procedure :: addCoordinate
      procedure :: getCoordinate

      procedure :: addElement
      procedure :: getNode
      procedure :: getPolyline

      procedure :: convertPolylineToLinels
   end type
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

      ! TODO
   end function
end module
