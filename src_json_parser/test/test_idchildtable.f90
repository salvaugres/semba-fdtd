integer function test_idchildtable() result(err)
   use idchildtable_mod
   use parser_tools_mod, only: json_value_ptr
   use testingTools
   use json_module

   implicit none

   type(IdChildTable_t) :: tbl

   character(len=*),parameter :: filename = 'cases/holland1981.fdtd.json'
   type(json_file) :: jsonfile
   type(json_core) :: core
   type(json_value), pointer :: root => null()
   type(json_value_ptr) :: mat

   err = 0
   call jsonfile%initialize()
   if (jsonfile%failed()) then
      err = err + 1
      return
   end if

   call jsonfile%load(filename = filename)
   if (jsonfile%failed()) then
      err = err + 1
      return
   end if

   call jsonfile%get_core(core)
   call jsonfile%get('.', root)

   tbl = IdChildTable_t(core, root, J_MATERIALS)

   if (tbl%count() /= 2) err = err + 1
   block
      character (len=:), allocatable :: matType
      logical :: found

      mat = tbl%getId(1)
      call core%get(mat%p, J_TYPE, matType, found)
      if (.not. found) err = err + 1
      if (matType /= J_MAT_TYPE_WIRE) err = err + 1

      mat = tbl%getId(2)
      call core%get(mat%p, J_TYPE, matType, found)
      if (.not. found) err = err + 1
      if (matType /= J_MAT_TYPE_WIRE_TERMINAL) err = err + 1
   end block
end function

