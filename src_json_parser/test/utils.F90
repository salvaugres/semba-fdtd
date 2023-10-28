module utils
    use json_module, wp => json_RK, IK => json_IK, LK => json_LK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit
    
    implicit none
    private
    public ::  print_json_variable
contains
    subroutine print_json_variable(json,p,finished)

        !! A `traverse` routine for printing out all
        !! the variables in a JSON structure.

        implicit none

        class(json_core),intent(inout)      :: json
        type(json_value),pointer,intent(in) :: p
        logical(LK),intent(out)        :: finished  !! set true to stop traversing
        
        logical :: namelist_style !! for printing JSON variable paths

        character(kind=json_CK,len=:),allocatable :: path !! path to the variable
        logical(LK) :: found !! error flag
        type(json_value),pointer :: child !! variable's first child
        character(kind=json_CK,len=:),allocatable :: value !! variable value as a string
        integer(IK) :: var_type !! JSON variable type

            
        call json%get_child(p,child)
        finished = .false.

        !only print the leafs:
        if (.not. associated(child)) then
            if (namelist_style) then
                call json%get_path(p,path,found,&
                                   use_alt_array_tokens=.true.,&
                                   path_sep=json_CK_'%')  ! fortran-style
            else
                call json%get_path(p,path,found)  ! JSON-style
            end if
            if (found) then

                call json%info(p,var_type=var_type)
                select case (var_type)
                case (json_array)
                    !an empty array
                    value = json_CK_'()'
                case (json_object)
                    !an empty object
                    value = json_CK_'{}'
                case default
                    ! get the value as a string
                    ! [assumes strict_type_checking=false]
                    ! note: strings are returned escaped without quotes
                    call json%get(p,value)
                end select

                !check for errors:
                if (json%failed()) then
                    finished = .true.
                else
                    write(output_unit,'(A)') path//json_CK_' = '//value
                end if

            else
                finished = .true.
            end if
        end if

    end subroutine print_json_variable
end module
    
module NFDETypes_extension
    use NFDETypes
    
    implicit none 
    
    public
    
    interface operator(==)
        module procedure desplazamiento_equal
    end interface
    
contains
    logical function desplazamiento_equal(a, b) result (res)
        type(Desplazamiento), intent(in) :: a, b
        
        res = .false.
        
        return 
    end function
end module