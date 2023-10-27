integer function test_fails_1() result(r)
    integer i, j
    
    i = 1
    j = 2
      
    if (i == j) then
        r = 0
    else 
        r = 1
    endif
    
end function