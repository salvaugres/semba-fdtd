integer function test_init() result(error_cnt)    
    use mtlnsolver
    error_cnt = 1
    res = sumone(error_cnt)
    print*, error_cnt
    print*, res
end function