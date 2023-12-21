integer function test_parser_tools_interval_to_coords() result(err)
   use parser_tools_mod
   use testingTools

   implicit none

   type(cell_interval_t) :: interval
   type(coords), dimension(:), allocatable :: cs
   err = 0

   ! +Y oriented linel interval.
   interval = cell_interval_t( &
      ini=cell_t([10, 0, 10]), end=cell_t([10, 5, 10]))
   cs = cellIntervalsToCoords([interval])

   call expect_eq_int(err,  1, size(cs))
   call expect_eq_int(err, 11, cs(1)%Xi)
   call expect_eq_int(err, 11, cs(1)%Xe)
   call expect_eq_int(err,  1, cs(1)%Yi)
   call expect_eq_int(err,  5, cs(1)%Ye)
   call expect_eq_int(err, 11, cs(1)%Zi)
   call expect_eq_int(err, 11, cs(1)%Ze)
   call expect_eq_int(err, +iEy, cs(1)%Or)

   ! -Z oriented linel interval.
   interval = cell_interval_t( &
      ini=cell_t([10, 10, 5]), end=cell_t([10, 10, -1]))
   cs = cellIntervalsToCoords([interval])

   call expect_eq_int(err,  1, size(cs))
   call expect_eq_int(err, 11, cs(1)%Xi)
   call expect_eq_int(err, 11, cs(1)%Xe)
   call expect_eq_int(err, 11, cs(1)%Yi)
   call expect_eq_int(err, 11, cs(1)%Ye)
   call expect_eq_int(err,  1, cs(1)%Zi)
   call expect_eq_int(err,  5, cs(1)%Ze)
   call expect_eq_int(err, -iEz, cs(1)%Or)

   ! +Y oriented surfel interval.
   interval = cell_interval_t( &
      ini=cell_t([ 9,  2,  9]), end=cell_t([11,  2, 11]) )
   cs = cellIntervalsToCoords([interval])
   call expect_eq_int(err,  1, size(cs))
   call expect_eq_int(err, 10, cs(1)%Xi)
   call expect_eq_int(err, 11, cs(1)%Xe)
   call expect_eq_int(err,  3, cs(1)%Yi)
   call expect_eq_int(err,  3, cs(1)%Ye)
   call expect_eq_int(err, 10, cs(1)%Zi)
   call expect_eq_int(err, 11, cs(1)%Ze)
   call expect_eq_int(err, +iEy, cs(1)%Or)


end function