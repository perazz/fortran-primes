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
!
!     automate generation of the first primes module
!     Created: 09/23/2023
!
!  ************************************************************************************************************
module generate_prime_numbers_module
    use fortran_io
    implicit none
    contains

    !> Create list of prime numbers
    subroutine create_primes

        integer :: iunit,last,start,endch,nchunk,loop
        integer, allocatable :: primes(:),pchunk(:)
        integer, parameter :: chunk=32768
        character(64) :: name

        ! Read in the first 1,000,000 primes
        open(newunit=iunit,file='primes1.txt',form='formatted',action='read')
        allocate(primes(1000000))
        read(iunit,*,end=1,err=2) primes
        close(iunit)

        open(newunit=iunit,file='primes_code.txt',form='formatted',action='write')

        ! Write module
        write(iunit,'(a)') ""

        write(iunit,'(a)') "  module prime_numbers"
        write(iunit,'(a)') "    use iso_fortran_env, only: int32"
        write(iunit,'(a)') "    implicit none"
        write(iunit,'(a)') "    private"
        write(iunit,'(a)') ""
        write(iunit,'(a)') "    ! Publicly accessible procedures"
        write(iunit,'(a)') "    public :: prime,is_prime"
        write(iunit,'(a)') "    public :: create_primes"
        write(iunit,'(a)') ""
        write(iunit,'(a)') "    ! Module parameters"
        write(iunit,'(a,i0)') "    integer     , parameter :: chunk       = ",chunk
        write(iunit,'(a,i0)') "    integer     , parameter :: n_primes    = ",size(primes)
        write(iunit,'(a)') "    integer     , parameter :: IP = int32"
        write(iunit,'(a)') "    character(*), parameter :: this_module = '[prime_numbers]'"
        write(iunit,'(a)') "    character(*), parameter :: fmt_er = ""(1x,a,' Overflow detected: n=',I9,' not between 1 and ',I9)"""
        write(iunit,'(a)') "    character(*), parameter :: fmt_er2 = ""(1x,a,' Overflow detected: ',I9,' not in [2,',I9,']')"""

        write(iunit,"(/)")

        ! Create arrays
        last = 0
        do while (last<size(primes))

            ! Create a new chunk
            start  = last+1
            endch  = min(last+chunk,size(primes))
            nchunk = endch-start+1

            ! Create chunk array
            write(name,10) start,endch


            pchunk = primes(start:endch)
            call print_1d_array(iunit,trim(name),pchunk,'IP')

            ! Move to next
            last = last + nchunk


        end do

        write(iunit,"(/)")
        write(iunit,'(a)')"    contains"

        ! Create subroutine to get n-th prime
        write(iunit,"(///)")

        write(iunit,'(a)') ""
        write(iunit,'(a)') "    ! Return the n-th prime number"
        write(iunit,'(a)') "    function prime(n) result(prime_number)"
        write(iunit,'(a)') "        integer, intent(in) :: n"
        write(iunit,'(a)') "        integer             :: prime_number"
        write(iunit,'(a)') ""
        write(iunit,'(a)') "        select case (n)"

        last = 0
        loop = 0
        do while (last<size(primes))

            loop = loop+1

            ! Create a new chunk
            start  = last+1
            endch  = min(last+chunk,size(primes))
            nchunk = endch-start+1

            ! Create chunk array
            write(iunit,20) loop-1,loop,start,endch,loop-1

            ! Move to next
            last = last + nchunk

        end do

        write(iunit,'(a)') "           case default"
        write(iunit,'(a)') "               write(*,fmt_er)this_module,n,n_primes"
        write(iunit,'(a)') "               stop"
        write(iunit,'(a)') "        end select"
        write(iunit,'(a)') " "
        write(iunit,'(a)') "    end function prime"

        ! Create function to return the n-th prime number
        write(iunit,"(///)")
        write(iunit,'(a)') "    ! Detect if an integer number is prime"
        write(iunit,'(a)') "    logical function is_prime(n)"
        write(iunit,'(a)') "       integer, intent(in) :: n"
        write(iunit,'(a)') "       integer :: it"
        write(iunit,'(a)') ""
        write(iunit,'(a)') "       ! Use a divide-and-conquer strategy"
        write(iunit,'(a)') "       select case (n)"

        last = 0
        loop = 0
        do while (last<size(primes))
            loop = loop+1
            ! Create a new chunk
            start  = last+1
            endch  = min(last+chunk,size(primes))
            nchunk = endch-start+1
            ! Create chunk array

            if (nchunk==chunk) then
                name = 'chunk'
            else
                write(name,'(i0)')nchunk
            end if

            write(iunit,30) start,endch,start,endch,trim(adjustl(name))
            write(iunit,31) start,endch,start,endch
            ! Move to next
            last = last + nchunk
        end do
        write(iunit,'(a)') "          case default"
        write(iunit,'(a)') "              write(*,fmt_er2)this_module,n,n_primes"
        write(iunit,'(a)') "              stop"
        write(iunit,'(a)') "       end select"
        write(iunit,'(a)') ""
        write(iunit,'(a)') "       is_prime = it>0.and.it<=chunk"
        write(iunit,'(a)') ""
        write(iunit,'(a)') "    end function is_prime"
        write(iunit,'(//a)') "  end module prime_numbers"
        close(iunit)

        return

        1 stop 'END OF FILE READING PRIMES'
        2 stop 'ERROR READING PRIMES'

        10 format('p',i0,'_to_',i0)
        20 format(11x,'case (',i3.3,'*chunk+1:',i3.3,'*chunk); prime_number = p',i0,'_to_',i0,'(n-',i0,'*chunk)')
        30 format(10x,'case (p',i0,'_to_',i0,'(1):p',i0,'_to_',i0,'(',a,'))')
        31 format(14x,'call quickfind_int(p',i0,'_to_',i0,',n,it,bounds=[',i0,',',i0,'])')

    end subroutine create_primes

end module generate_prime_numbers_module
