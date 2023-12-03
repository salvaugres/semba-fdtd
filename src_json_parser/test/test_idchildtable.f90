integer function test_idchildtable() result(error_cnt)
   use idchildtable_mod
   use testingTools
   use json_module

   type(IdChildTable_t) :: tbl
   implicit none

   character(len=*),parameter :: filename = 'cases/holland1981.fdtd.json'
   type(json_file) :: jsonfile
   type(json_core) :: core
   type(json_value), pointer :: root => null()
   type(json_value), pointer :: mat

   error_cnt = 0
   call jsonfile%initialize()
   if (jsonfile%failed()) then
      error_cnt = error_cnt + 1
      return
   end if

   call jsonfile%load(filename = filename)
   if (jsonfile%failed()) then
      error_cnt = error_cnt + 1
      return
   end if

   call jsonfile%get_core(core)
   call jsonfile%get('.', root)

   table = IdChildTable_t(core, root, J_MATERIALS)

   block
      character (len=:), allocatable :: name
      mat = table%getId(1)
      
      call core%get(mat, J_NAME, name)
      if (name /= "wireMaterial") error_cnt = error_cnt + 1
   end block
end function

