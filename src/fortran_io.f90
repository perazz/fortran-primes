!  ************************************************************************************************************
!
!                                        ____  ____  ______  ______________
!                                       / __ \/ __ \/  _/  |/  / ____/ ___/
!                                      / /_/ / /_/ // // /|_/ / __/  \__ \
!                                     / ____/ _, _// // /  / / /___ ___/ /
!                                    /_/   /_/ |_/___/_/  /_/_____//____/
!
!                                         Constants and parameters
!
!  MIT License
!
!  Copyright (c) 2014-2023 Federico Perini
!  Parts of this code Copyright (c) the Primes.jl contributors and
!  All codes published under https://people.ksp.sk/~misof/primes/
!  are available under the CC BY-NC 4.0 Int'l license.
!
!  ************************************************************************************************************
module fortran_io
    use iso_fortran_env
    implicit none
    private

    integer, parameter :: left_indent = 4
    integer, parameter :: indent      = 4
    integer, parameter :: tot_indent  = left_indent+indent
    integer, parameter :: line_len    = 100


    public :: print_1d_array

    interface print_1d_array
        module procedure print_1d_array_int32
    end interface print_1d_array

    contains

    ! Print a 1D fortran array to unit
    subroutine print_1d_array_int32(iunit,name,array,kind_name)
        integer(int32), intent(in) :: iunit
        character(*), intent(in) :: name
        integer, allocatable, intent(in) :: array(:)
        character(*), intent(in), optional :: kind_name

        integer, parameter :: ndim = 1
        integer :: dim,nelems,dumped
        integer, allocatable :: larray(:)
        character(len=:), allocatable :: line,kindstr
        character(len=24) :: dims(ndim),current

        if (.not.allocated(array)) stop '[fortran_io] cannot print unallocated array'

        if (present(kind_name)) then
            kindstr = trim(kind_name)
        else
            kindstr = 'int32'
        end if

        allocate(character(len=line_len) :: line)
        line = repeat(' ',line_len)

        do dim=1,ndim
            if (lbound(array,dim)==1) then
                write(dims(dim),'(i0)') ubound(array,dim)
            else
                write(dims(dim),"(i0,':',i0)") lbound(array,dim),ubound(array,dim)
            end if
        end do

        write(line,1) repeat(' ',left_indent),kindstr,name,(trim(dims(dim)),dim=1,ndim),kindstr

        ! Add array elements, one by one
        nelems = product(shape(array))
        dumped = 0
        larray = reshape(array,[product(shape(array))])

        do while (dumped<nelems)

            dumped = dumped+1

            ! Add current
            if (dumped<nelems) then
               write(current,"(i0,',')") larray(dumped)
            else
               write(current,"(i0,']')") larray(dumped)
            end if

            call add_or_flush_line(line,current,iunit)

        end do

        ! Flush last line
        write(iunit,'(a)')line

        1 format(a,'integer(',a,'), parameter :: ',a,'(',a,') = [ integer(',a,') :: ')

    end subroutine print_1d_array_int32

    ! Add a string to a Fortran line/ flush the line if it's becoming too long. Do not resize it!
    subroutine add_or_flush_line(line,current,iunit)
        character(*), intent(inout) :: line
        character(*), intent(in) :: current
        integer, intent(in) :: iunit

        integer :: line_len,lt,lcur

        line_len = len(line)
        lcur     = len_trim(current)
        lt       = len_trim(line)

        flush_line: if (lcur+lt > line_len-2) then

            ! Add continuation
            line(lt+1:line_len-1)   = ' '
            line(line_len:line_len) = '&'

            ! Flush line
            write(iunit,'(a)')line

            ! Init new line
            line = ' '
            line(tot_indent+1:tot_indent+lcur) = current(1:lcur)

        else

            ! Do not resize the line
            line(lt+1:lt+lcur) = trim(current)
            line(lt+lcur+1:line_len) = ' '

        endif flush_line

    end subroutine add_or_flush_line

end module fortran_io
