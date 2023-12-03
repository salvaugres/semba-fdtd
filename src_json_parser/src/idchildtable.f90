module idchildtable_mod
   use json_module
   use fhash, only: fhash_tbl_t, key=>fhash_key
   use labels_mod

   type :: IdChildTable_t
      private
      type(fhash_tbl_t) :: idToChilds
   contains
      procedure :: getId => idChildTable_getId
   end type

   interface IdChildTable_t
      module procedure idChildTable_ctor
   end interface

   type, private :: json_value_ptr
      type(json_value), pointer :: p
   end type
contains

   function idChildTable_ctor(core, root, path) result(res)
      type(json_core) :: core
      type(json_value), pointer :: root
      type(IdChildTable_t) :: res
      character (len=*), intent(in) :: path
      type(json_value), pointer :: jentries, jentry
      integer :: id
      integer :: i
      logical :: found

      call core%get(root, path, jentries, found)
      if (.not. found) return
      do i = 1, core%count(jentries)
         call core%get_child(jentries, i, jentry)
         call core%get(jentry, J_ID, id)
         call res%idToChilds%set(key(id), json_value_ptr(jentry))
      end do
   end function

   function idChildTable_getId(this, id) result(res)
      class(IdChildTable_t) :: this
      type(json_value), pointer :: res
      integer, intent(in) :: id
      integer :: mStat
      class(*), allocatable :: d

      nullify(res)
      call this%idToChilds%check_key(key(id), mStat)
      if (mStat /= 0) then
         return
      end if

      call this%idToChilds%get_raw(key(id), d)
      select type(d)
       type is (json_value_ptr)
         res = d%p
      end select
   end function

end module
