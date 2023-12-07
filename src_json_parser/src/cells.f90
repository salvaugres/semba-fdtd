module cells_mod
   use mesh_mod

   integer, parameter :: DIR_X = 1
   integer, parameter :: DIR_Y = 2
   integer, parameter :: DIR_Z = 3

   
   integer, private, parameter  ::  MAX_LINE = 256

   real, dimension(3), parameter :: FIRST_CELL_POSITION = [1.0, 1.0, 1.0]

   ! --- Cells
   type :: cell_t
      real, dimension(3) :: cell
      character (len=:), allocatable :: tag
   end type cell_t

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
      type(cell_t), dimension(2) :: interval
   end type cell_region_t

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
            res(lastSegment)%cell = floor(mc%position) + FIRST_CELL_POSITION
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
   end function

   function convertNodeToPixel(mesh, node) result(res)
      type(pixel_t), dimension(:), allocatable :: res
      class(mesh_t) :: mesh
      type(node_t), intent(in) :: node
      
      type(coordinate_t) :: c
      logical :: coordFound

      c = mesh%getCoordinate(node%coordIds(1), found=coordFound)
      if (.not. coordFound) then
         allocate(res(0))
         return
      end if
      allocate(res(1))
      res(1)%cell = c%position + FIRST_CELL_POSITION
      res(1)%tag = trim(intToString(node%coordIds(1)))
   end function
   ! __________________________________________________________________
   ! Aux functions 
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
