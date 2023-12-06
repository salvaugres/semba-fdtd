module cells_mod

   integer, parameter :: DIR_X = 1
   integer, parameter :: DIR_Y = 2
   integer, parameter :: DIR_Z = 3

   ! --- Cells
   type :: cell_t
      real, dimension(3) :: v
   end type cell_t

   type, extends(cell_t) :: pixel_t
   end type

   type, extends(cell_t) :: linel_t
      integer :: orientation                ! DIR_X, DIR_Y, DIR_Z
      character (len=:), allocatable :: tag
   end type

   type :: cell_region_t
      type(cell_t), dimension(2) :: coords
   end type cell_region_t

   interface operator(==)
      module procedure linel_eq
   end interface

contains
   logical function linel_eq(a, b)
      type(linel_t), intent(in) :: a, b
      linel_eq = &
         all(a%v == b%v) .and. &
         a%tag == b%tag .and. &
         a%orientation == b%orientation
   end function

end module
