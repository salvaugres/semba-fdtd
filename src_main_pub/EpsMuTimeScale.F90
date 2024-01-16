!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! MIT License
! 
! Copyright (c) 2023 University of Granada
! 
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
! 
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
! 
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
!//===========================================================================//
!// HYSTORY & VERSION:                                                        //
!//    DATE beginning: 2018-12-18 17:17:09                                   //
!//===========================================================================//
module EpsMuTimeScale_m

USE FDETYPES
private :: new_input_, checkError_

type EpsMuTimeScale_input_parameters_t
    real (kind=RKind) :: tini, tend, alpha_max
    logical :: electric, magnetic
    logical :: are_there
contains
    procedure, pass, public  :: get_slope  => get_slope_
    procedure, pass, public  :: init0      => new_input_
    procedure, pass, public  :: checkError => checkError_
end type EpsMuTimeScale_input_parameters_t

contains

function get_slope_ (this) result (slope)
    class (EpsMuTimeScale_input_parameters_t) :: this
    real (kind=RKind) :: slope
    slope = (this%alpha_max-1.0_Rkind)/(this%tend-this%tini)
end function get_slope_

subroutine new_input_ (this)
    class (EpsMuTimeScale_input_parameters_t) :: this
    this%alpha_max = 1.0_Rkind
    this%tini        = 1e20_Rkind
    this%tend      = 1e20_Rkind
    this%electric = .false.
    this%magnetic  = .false.
    this%are_there = .false.
end subroutine new_input_

function checkError_ (this) result (res)
    class (EpsMuTimeScale_input_parameters_t) :: this
    integer :: res
    res = 0
    if (this%alpha_max<=0.0 .or. this%tini<0.0) then
        res = -1
    end if

end function checkError_

end module EpsMuTimeScale_m