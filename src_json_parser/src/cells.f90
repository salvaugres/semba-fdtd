module cells_mod

   integer, parameter :: DIR_X = 1
   integer, parameter :: DIR_Y = 2
   integer, parameter :: DIR_Z = 3


   integer, private, parameter  ::  MAX_LINE = 256

   real, dimension(3), parameter :: FIRST_CELL_POSITION = [1.0, 1.0, 1.0]

   ! --- Cells
   type :: cell_t
      real, dimension(3) :: cell
      character (len=:), allocatable :: tag
   end type

   type, extends(cell_t) :: pixel_t
   end type

   type, extends(cell_t) :: linel_t
      integer :: orientation                ! DIR_X, DIR_Y, DIR_Z
   end type

   type, extends(cell_t) :: surfel_t
      integer :: orientation                ! DIR_X, DIR_Y, DIR_Z
   end type

   type, extends(cell_t) :: voxel_t
   end type

   type :: cell_region_t
      ! Cell regions are CLOSED intervals of the form [ab(1), ab(2)]
      type(cell_t), dimension(2) :: ab
   end type

   interface operator(==)
      module procedure linel_eq
      module procedure pixel_eq
   end interface

contains
   logical function pixel_eq(a, b)
      type(pixel_t), intent(in) :: a, b
      pixel_eq = &
         all(a%cell == b%cell) .and. &
         a%tag == b%tag
   end function

   logical function linel_eq(a, b)
      type(linel_t), intent(in) :: a, b
      linel_eq = &
         all(a%cell == b%cell) .and. &
         a%tag == b%tag .and. &
         a%orientation == b%orientation
   end function


end module
