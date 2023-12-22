integer function test_parser_ctor() result(err)
   use smbjson
   use testingTools

   implicit none

   character(len=*),parameter :: filename = 'cases/planewave.fdtd.json'
   type(parser_t) :: parser
   err = 0

   parser = parser_t(filename)

end function

