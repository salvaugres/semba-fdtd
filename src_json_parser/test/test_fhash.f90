integer function test_fhash() result(error_cnt)
   use fhash, only: fhash_tbl_t, key=>fhash_key
   implicit none
   type(fhash_tbl_t) :: tbl
   integer :: val
   integer, parameter :: expectedValue=10
   error_cnt = 0
   
   call tbl%set(key('my_key_1'), value=expectedValue)
   call tbl%set(key('my_key_2'), value=1.0)
   call tbl%set(key(123456), value='a string value')
   call tbl%set(key([1,2,3,4,5]), value=.false.)

   call tbl%get(key('my_key_1'),val)

   if (val /= expectedValue) then 
      error_cnt = error_cnt+1
      return
   end if
end function

