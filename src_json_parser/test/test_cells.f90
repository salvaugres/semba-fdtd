integer function test_cells() result(err)
   use cells_mod
   use testingTools

   implicit none

   type(cell_interval_t) :: interval
   err = 0

   ! Forwared oriented linel interval.
   interval = cell_interval_t(ini=[1, 1, 1], end=[3, 1, 1])
   if (interval%getType()        /= CELL_TYPE_LINEL) err = err + 1
   if (interval%getOrientation() /= + DIR_X) err = err + 1
   if (interval%getSize()        /= 2) err = err + 1

   ! Backwards oriented linel interval.
   interval = cell_interval_t(ini=[2, 1, 1], end=[0, 1, 1])
   if (interval%getType()        /= CELL_TYPE_LINEL) err = err + 1
   if (interval%getOrientation() /= - DIR_X) err = err + 1
   if (interval%getSize()        /= 2) err = err + 1

   ! 2x2 surfel, with +Z normal.
   interval = cell_interval_t(ini=[3, 3, 3], end=[5, 5, 3])
   if (interval%getType()        /= CELL_TYPE_SURFEL) err = err + 1
   if (interval%getOrientation() /= + DIR_Z) err = err + 1
   if (interval%getSize()        /= 4) err = err + 1

   ! 3x1 surfel, with -X normal.
   interval = cell_interval_t(ini=[5, 5, 5], end=[5, 4, 2])
   if (interval%getType()        /= CELL_TYPE_SURFEL) err = err + 1
   if (interval%getOrientation() /= - DIR_X) err = err + 1
   if (interval%getSize()        /= 3) err = err + 1
end function