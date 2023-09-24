!   ************************************************************************************************
!   **                                                                                            **
!   **                  |\   -  -.   ./                                                           **
!   **                  | \./ \/ | ./ /     __________  ___________ __________                    **
!   **                __|         /  /     / ____/ __ \/ ____/ ___// ____/ __ \                   **
!   **                \    .        /.-/  / /_  / /_/ / __/  \__ \/ /   / / / /                   **
!   **                 \   |\.|\/|    /  / __/ / _, _/ /___ ___/ / /___/ /_/ /                    **
!   **                  \__\     /___/  /_/   /_/ |_/_____//____/\____/\____/                     **
!   **                                                                                            **
!   **                                     FRESCO-SpeedCHEM                                       **
!   **            A code for internal combustion engine flows with chemical kinetics              **
!   **                                                                                            **
!   ************************************************************************************************
!   **                                                                                            **
!   **    prime_number                                                                            **
!   **    A collection of the first 100008 prime numbers                                          **
!   **                                                                                            **
!   ************************************************************************************************
!   **                                                                                            **
!>  **    @author Federico Perini                                                                 **
!   **    Author     : Federico Perini                                                            **
!   **    Created    : monday ,  08/09/2014                                                       **
!   **    Last update: monday ,  08/09/2014                                                       **
!   **                                                                                            **
!   ************************************************************************************************
  module prime_numbers

    use fortran_io
    use iso_fortran_env
    use prime_constants

    implicit none
    private

    ! Publicly accessible procedures
    public :: prime,is_prime
    public :: create_primes
    public :: n_primes
    public :: miller_rabin_test,witnesses
    public :: generate_min_factors

    public :: primes
    public :: primes_mask

    interface primes
        module procedure primes_bounds
        module procedure primes_limit
    end interface primes

    interface primes_mask
        module procedure primes_mask_hi
        module procedure primes_mask_bounds
    end interface primes_mask

    ! Module parameters
    character(*), parameter :: this_module = "[prime_numbers]"
    character(*), parameter :: fmt_er = "(1x,a,' Overflow detected: n=',I9,' not between 1 and ',I9)"
    character(*), parameter :: fmt_er2 = "(1x,a,' Overflow detected: ',I9,' not in [2,',I9,']')"

    contains

    ! Return the n-th prime number
    function prime(n) result(prime_number)
        integer, intent(in) :: n
        integer             :: prime_number

        select case (n)
           case (        1:  chunk); prime_number = p1_to_10000(n)
           case (  chunk+1:2*chunk); prime_number = p10001_to_20000(n-  chunk)
           case (2*chunk+1:3*chunk); prime_number = p20001_to_30000(n-2*chunk)
           case (3*chunk+1:4*chunk); prime_number = p30001_to_40000(n-3*chunk)
           case (4*chunk+1:5*chunk); prime_number = p40001_to_50000(n-4*chunk)
           case default
               write(*,fmt_er)this_module,n,n_primes
               stop
        end select

    end function prime

    ! Return a list of the first i-th prime numbers
    function primes_limit(limit) result(list)
        integer(IP), intent(in) :: limit
        integer(IP), allocatable :: list(:)
        list = primes_bounds(1,limit)
    end function primes_limit

    ! Return a list of the i-th prime numbers (from lo- to hi-)
    function primes_bounds(lo,hi) result(list)
        integer(IP), intent(in) :: lo,hi
        integer(IP), allocatable :: list(:)

        integer(IP), parameter :: i_235(3) = [integer(IP) :: 2,3,5]
        integer(IP) :: i,losafe,lwi,nmask,nshort,last
        logical(LP), allocatable :: mask(:)
        logical(LP) :: has_235(3)

        if (lo>hi) then
            ! Invalid range
            allocate(list(0))
            return
        else

            ! Check small primes
            has_235 = lo<=i_235 .and. hi>=i_235

            if (hi<7) then
                list = pack(i_235,has_235)
            else

                losafe = max(lo,2_IP)

                mask   = primes_mask(max(7,lo), hi)
                nmask  = count(mask)
                nshort = count(has_235)

                allocate(list(nmask+nshort))

                if (nshort>0) list(1:nshort) = pack(i_235,has_235)
                last = nshort
                lwi = wheel_index(losafe-1)

                do i=1,size(mask,kind=IP)
                    if (mask(i)) then
                        list(last+1) = wheel_prime(i+lwi)
                        last = last+1
                    end if
                end do

            end if

        end if

    end function primes_bounds

    ! Detect if an integer number is prime
    logical function is_prime(n)
       integer, intent(in) :: n

       integer(IP), parameter :: SIMPLE_FACTORS(*) = [3,5,7,11,13,17,19,23]

       if (n<2) then
          is_prime = .false.
       elseif (mod(n,2)==0) then
          ! Even
          is_prime = n==2
       elseif (n<N_SMALL) then
          ! Look-up table
          is_prime = min_factor(n)==n
       elseif (any(mod(n,SIMPLE_FACTORS)==0)) then
          ! Very large number. Test simple factors first
          is_prime = .false.
       elseif (n<2_IP**32) then
          ! Perform Miller-Rabin test
          is_prime = miller_rabin_test(witnesses(n),n)
       else
          is_prime = miller_rabin_test(witnesses(n),n)
!    miller_rabbin_test(2, n) || return false
!    return lucas_test(widen(n))
       end if

    end function is_prime

    ! Find position of stored point x in list, list *already* sorted in ascending order
    pure recursive subroutine quickfind_int(list,x,it,bounds)
       integer(IP), intent(in)              :: list(:)
       integer(IP), intent(in)              :: x
       integer(IP), intent(inout)           :: it
       integer(IP), intent(in), optional    :: bounds(2)
       integer(IP), parameter :: max_quickfind_size = 16_IP

       ! Local variables
       integer(IP) :: i,n,l,u

       n = size(list,kind=IP)

       ! Define current global range bounds
       if (present(bounds)) then
         l = bounds(1)
         u = bounds(2)
       else
         l = lbound(list,1,kind=IP)
         u = ubound(list,1,kind=IP)
       end if
       if (n<=max_quickfind_size) then
          ! Point is lower than lowest value
          it = -huge(it)
          if (x<list(1)) return

          do i=1,n
             if (x==list(i)) then
                it = l-1+i
                return
             end if
          end do

          ! Point is bigger than largest value
          it = huge(it)
       else
          ! Set is big, use quicksort
          i = int(n/2,kind=IP)
          split_list: if (x>=list(i)) then
             call quickfind_int(list(i:),x,it,[l-1+i,u])
          else split_list
             call quickfind_int(list(:i),x,it,[l,l-1+i])
          end if split_list
       end if
    end subroutine quickfind_int

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
        write(iunit,'(//)')
        write(iunit,'(a)')"    ! Find position of stored point x in list, list *already* sorted in ascending order"
        write(iunit,'(a)')"    pure recursive subroutine quickfind_int(list,x,it,bounds)"
        write(iunit,'(a)')"       integer(IP), intent(in)              :: list(:)"
        write(iunit,'(a)')"       integer(IP), intent(in)              :: x"
        write(iunit,'(a)')"       integer(IP), intent(inout)           :: it"
        write(iunit,'(a)')"       integer(IP), intent(in), optional    :: bounds(2)"
        write(iunit,'(a)')"       integer(IP), parameter :: max_quickfind_size = 16_IP"
        write(iunit,'(a)')""
        write(iunit,'(a)')"       ! Local variables"
        write(iunit,'(a)')"       integer(IP) :: i,n,l,u"
        write(iunit,'(a)')""
        write(iunit,'(a)')"       n = size(list,kind=IP)"
        write(iunit,'(a)')""
        write(iunit,'(a)')"       ! Define current global range bounds"
        write(iunit,'(a)')"       if (present(bounds)) then"
        write(iunit,'(a)')"         l = bounds(1)"
        write(iunit,'(a)')"         u = bounds(2)"
        write(iunit,'(a)')"       else"
        write(iunit,'(a)')"         l = lbound(list,1,kind=IP)"
        write(iunit,'(a)')"         u = ubound(list,1,kind=IP)"
        write(iunit,'(a)')"       end if"
        write(iunit,'(a)')"       if (n<=max_quickfind_size) then"
        write(iunit,'(a)')"          ! Point is lower than lowest value"
        write(iunit,'(a)')"          it = -huge(it)"
        write(iunit,'(a)')"          if (x<list(1)) return"
        write(iunit,'(a)')""
        write(iunit,'(a)')"          do i=1,n"
        write(iunit,'(a)')"             if (x==list(i)) then"
        write(iunit,'(a)')"                it = l-1+i"
        write(iunit,'(a)')"                return"
        write(iunit,'(a)')"             end if"
        write(iunit,'(a)')"          end do"
        write(iunit,'(a)')""
        write(iunit,'(a)')"          ! Point is bigger than largest value"
        write(iunit,'(a)')"          it = huge(it)"
        write(iunit,'(a)')"       else"
        write(iunit,'(a)')"          ! Set is big, use quicksort"
        write(iunit,'(a)')"          i = int(n/2,kind=IP)"
        write(iunit,'(a)')"          split_list: if (x>=list(i)) then"
        write(iunit,'(a)')"             call quickfind_int(list(i:),x,it,[l-1+i,u])"
        write(iunit,'(a)')"          else split_list"
        write(iunit,'(a)')"             call quickfind_int(list(:i),x,it,[l,l-1+i])"
        write(iunit,'(a)')"          end if split_list"
        write(iunit,'(a)')"       end if"
        write(iunit,'(a)')"    end subroutine quickfind_int"
        write(iunit,'(a)')""
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

    ! Return number of trailing zeros in the bitwise representation
    elemental integer(IP) function trailing_zeros(n) result(nzero)
       integer(IP), intent(in) :: n

       integer(IP) :: pos
       nzero = 0
       do pos=0,bit_size(n)-1
           if (btest(n,pos)) return
           nzero = nzero+1
       end do
    end function trailing_zeros

    ! n > 2, an odd integer to be tested for primality
    elemental logical function miller_rabin_test(a, n) result(is_prime)

       ! a parameter that determines the accuracy of the test, i.e. a random in [2, n−1]
       integer(IP), intent(in) :: a

       ! n > 2, an odd integer to be tested for primality
       integer(IP), intent(in) :: n

       !> Local variables
       integer(IP) :: s,d,x,t

       ! Probably not prime
       is_prime = .false.

       if (n<=2) return

       ! Write (n-1) as 2s*d (d odd) by factoring powers of 2 from n − 1
       s = trailing_zeros(n-1)
       d = shiftr(n-1,s)

       x = mod(a**d,n) ! can grow quickly

       if (x/=1) then
          t = s
          do while (x/=n-1)
             t = t-1
             if (t<=0) return

             x = int(mod(int(x,WP)**2,n),IP)
             if (x==1) return

          end do
       end if
       is_prime = .true.

    end function miller_rabin_test

    elemental integer(IP) function witnesses(n)
       integer(IP), intent(in) :: n
       integer(WP) :: i,nw

       integer(IP), parameter :: bases(*) = [ &
            15591,  2018,   166,  7429,  8064, 16045, 10503,  4399,  1949,  1295,  2776,  3620,&
              560,  3128,  5212,  2657,  2300,  2021,  4652,  1471,  9336,  4018,  2398, 20462,&
            10277,  8028,  2213,  6219,   620,  3763,  4852,  5012,  3185,  1333,  6227,  5298,&
             1074,  2391,  5113,  7061,   803,  1269,  3875,   422,   751,   580,  4729, 10239,&
              746,  2951,   556,  2206,  3778,   481,  1522,  3476,   481,  2487,  3266,  5633,&
              488,  3373,  6441,  3344,    17, 15105,  1490,  4154,  2036,  1882,  1813,   467,&
             3307, 14042,  6371,   658,  1005,   903,   737,  1887,  7447,  1888,  2848,  1784,&
             7559,  3400,   951, 13969,  4304,   177,    41, 19875,  3110, 13221,  8726,   571,&
             7043,  6943,  1199,   352,  6435,   165,  1169,  3315,   978,   233,  3003,  2562,&
             2994, 10587, 10030,  2377,  1902,  5354,  4447,  1555,   263, 27027,  2283,   305,&
              669,  1912,   601,  6186,   429,  1930, 14873,  1784,  1661,   524,  3577,   236,&
             2360,  6146,  2850, 55637,  1753,  4178,  8466,   222,  2579,  2743,  2031,  2226,&
             2276,   374,  2132,   813, 23788,  1610,  4422,  5159,  1725,  3597,  3366, 14336,&
              579,   165,  1375, 10018, 12616,  9816,  1371,   536,  1867, 10864,   857,  2206,&
             5788,   434,  8085, 17618,   727,  3639,  1595,  4944,  2129,  2029,  8195,  8344,&
             6232,  9183,  8126,  1870,  3296,  7455,  8947, 25017,   541, 19115,   368,   566,&
             5674,   411,   522,  1027,  8215,  2050,  6544, 10049,   614,   774,  2333,  3007,&
            35201,  4706,  1152,  1785,  1028,  1540,  3743,   493,  4474,  2521, 26845,  8354,&
              864, 18915,  5465,  2447,    42,  4511,  1660,   166,  1249,  6259,  2553,   304,&
              272,  7286,    73,  6554,   899,  2816,  5197, 13330,  7054,  2818,  3199,   811,&
              922,   350,  7514,  4452,  3449,  2663,  4708,   418,  1621,  1171,  3471,    88,&
            11345,   412,  1559,   194]

       nw = int(n,WP)

       i = ieor(shifta(nw,16),nw) * int(z'45d9f3b',WP)
       i = ieor(shifta(i,16), i ) * int(z'45d9f3b',WP)
       i = iand(ieor(shifta(i,16), i), 255_WP) + 1_WP
       witnesses = int(bases(i),IP)
    end function witnesses

    elemental integer(IP) function wheel_index(n)
       integer(IP), intent(in) :: n
       integer(IP) :: d,r
       d = int((n-1)/30,IP)
       r = mod(n-1,30)
       wheel_index = 8*d+wheel_indices(r+2)
    end function wheel_index

    elemental integer(IP) function wheel_prime(n)
       integer(IP), intent(in) :: n
       integer(IP) :: d,r
       d = shifta(n-1,3_IP)
       r = iand(n-1,7_IP)
       wheel_prime = 30*d+wheel_primes(r+1)
    end function wheel_prime

    function generate_min_factors(limit) result(res)
       integer(IP), intent(in) :: limit
       integer(IP), allocatable :: res(:)

       integer(IP) :: i,m,n

       ! Only include odd numbers
       allocate(res(max(0,(limit-3)/2+1)))

       n = 0
       do i=3,limit,2
          n = n+1
          m = min_factor(i)
          res(n) = merge(1_IP,m,m==i)
       end do
       if (n/=size(res)) then
          print *, n, size(res)
          stop 'catastrophic! n/=size(Res)'
       endif

       contains

          integer(IP) function min_factor(n)
             integer(IP), intent(in) :: n
             if (n<4) then
                min_factor = n
                return
             end if
             do min_factor=3,isqrt(n),2
                if (mod(n,min_factor)==0) return
             end do
             min_factor = n
          end function min_factor

    end function generate_min_factors

    ! Largest integer for which isqrt**2<=n
    elemental integer(IP) function isqrt(n)
       integer(IP), intent(in) :: n
       intrinsic :: sqrt
       isqrt = floor(sqrt(real(n,WP)),IP)
    end function isqrt

    function primes_mask_hi(limit) result(mask)
       integer(IP), intent(in) :: limit
       logical(LP), allocatable :: mask(:)

       integer(IP) :: m,n,p,q,i,j

       if (.not.limit>=7) stop '[primes] mask range limit must be >=7.'

       n = wheel_index(limit)
       m = wheel_prime(m)
       allocate(mask(n),source=.true._LP)

       do i=1,wheel_index(isqrt(limit))
          if (mask(i)) then
             p = wheel_prime(i)
             q = p**2
             j = iand(i-1,7_IP)+1_IP
             do while (q<=m)
                mask(wheel_index(q)) = .false._LP
                q = q+wheel(j)*p
                j = iand(j,7_IP)+1_IP
             end do
          endif
       end do

    end function primes_mask_hi

    function primes_mask_bounds(lo,hi) result(mask)
       integer(IP), intent(in) :: lo,hi
       logical(LP), allocatable :: mask(:)

       integer(IP) :: wlo,whi,m,i,j,p,q
       integer(WP) :: r
       logical(LP), allocatable :: small_mask(:)

       if (.not.(lo>=7 .and. hi>=7)) stop '[primes] inputs to mask range must be >=7. '

       wlo = wheel_index(lo-1)
       whi = wheel_index(hi)
       m   = wheel_prime(whi)

       allocate(mask(whi-wlo), source=.true._LP)
       if (hi<49) return

       small_mask = primes_mask_hi(isqrt(hi))

       do i=1,size(small_mask)
          if (small_mask(i)) then
            p = wheel_prime(i)
            j = wheel_index(2*int((lo-p-1)/(2*p),IP)+1)

            ! Use wide integer to agoid r<=m due to overflow
            r = int(p,WP)*int(wheel_prime(j+1),WP)
            if (r>m) cycle



            j = iand(j,7_IP) + 1_IP
            q = int(r,IP)

            ! q < 0 indicates that overflow occurred incrementing q
            do while (q>=0_IP .and. q<=m)
                mask(wheel_index(q) - wlo) = .false._LP
                q = q + wheel(j) * p
                j = iand(j,7_IP) + 1_IP
            end do

          end if
       end do

    end function primes_mask_bounds

  end module prime_numbers

