integer function test_coaxial_line_paul_8_6_square() bind(C) result(error_cnt)    
    use mtln_solver_mod

    implicit none

    character(len=*), parameter :: filename = 'testData/parser/Paul/coaxial_line_paul_8_6_square.smb.json'


    ! p = Parser(file)
    ! p.run(finalTime = 18e-6)

    ! start_times = [0.1, 4.1, 6.1, 8.1, 10.1, 12.1, 14.1, 16.1]
    ! end_times = [3.9, 5.9, 7.9, 9.9, 11.9, 13.9, 15.9, 18.9]
    ! check_voltages = [25, -12.5, -37.5, -18.75, 18.75, 9.375, -9.375, -4.6875]
    ! for (t_start, t_end, v) in zip(start_times, end_times, check_voltages):
    !     start = np.argmin(np.abs(p.probes["v_source"].t - t_start*1e-6))
    !     end = np.argmin(np.abs(p.probes["v_source"].t - t_end*1e-6))
    !     assert np.all(np.isclose(p.probes["v_source"].val[start:end], v))

end function test_coaxial_line_paul_8_6_square