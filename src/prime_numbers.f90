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
module prime_numbers

    use iso_fortran_env
    use prime_constants

    implicit none
    private

    ! prime(n) returns the n-th prime number
    public :: prime

    ! next_prime(n, i) returns the i-th prime number next to n (n not included)
    public :: next_prime

    ! call prime_factors(n, factors): return all prime factors, and their multiplicities, of an integer.
    ! factors(FACTORS_PRIME,:) -> all prime factors
    ! factors(FACTORS_POWER,:) -> their powers
    public :: prime_factors

    ! Check if an integer is prime
    public :: is_prime
    interface is_prime
        module procedure is_prime32
        module procedure is_prime64
    end interface is_prime

    ! Return a list of primes within a given integer range (or upper bound)
    public :: primes
    interface primes
        module procedure primes_bounds
        module procedure primes_limit
    end interface primes

    ! Return a logical mask of primes within a given integer range (or upper bound)
    public :: primes_mask
    interface primes_mask
        module procedure primes_mask_hi
        module procedure primes_mask_bounds
    end interface primes_mask

    ! Module parameters
    character(*), parameter :: this_module = "[prime_numbers]"
    character(*), parameter :: fmt_er = "(1x,a,' Overflow detected: n=',I9,' not between 1 and ',I9)"
    character(*), parameter :: fmt_er2 = "(1x,a,' Overflow detected: ',I9,' not in [2,',I9,']')"

    ! Internal
    public :: generate_min_factors

    contains

    ! Return the n-th prime number
    function prime(n) result(prime_number)
        integer(IP), intent(in) :: n
        integer(IP)             :: prime_number,more

        select case (n)
           case (        1:  chunk); prime_number = p1_to_10000(n)
           case (  chunk+1:2*chunk); prime_number = p10001_to_20000(n-  chunk)
           case (2*chunk+1:3*chunk); prime_number = p20001_to_30000(n-2*chunk)
           case (3*chunk+1:4*chunk); prime_number = p30001_to_40000(n-3*chunk)
           case (4*chunk+1:5*chunk); prime_number = p40001_to_50000(n-4*chunk)
           case (5*chunk+1:)
               ! Use next-prime outside of the table
               more = n-5*chunk
               prime_number = next_prime(p40001_to_50000(chunk),more)
           case default
               ! Negative/invalid
               prime_number = -huge(prime_number)
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

                mask   = primes_mask_ge7_bounds(max(7,lo), hi)
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
    elemental logical function is_prime32(n) result(is_prime)
       integer(IP), intent(in) :: n
       is_prime = is_prime64(int(n,WP))
    end function is_prime32

    elemental logical function is_prime64(n) result(is_prime)
       integer(WP), intent(in) :: n

       integer(WP), parameter :: SIMPLE_FACTORS(*) = [3,5,7,11,13,17,19,23]
       integer(IP) :: w

       if (n<2) then
          is_prime = .false.
       elseif (mod(n,2)==0) then
          ! Even
          is_prime = n==2
       elseif (n<N_SMALL) then
          ! Look-up table
          is_prime = odd_min_factor(n)==n
       elseif (any(mod(n,SIMPLE_FACTORS)==0)) then
          ! Very large number. Test simple factors first
          is_prime = .false.
       elseif (n<2_WP**32) then
          ! Perform Miller-Rabin test
          is_prime = is_prime_32(int(n,IP), int(witnesses(n),IP))
       else

          if (.not.is_SPRP64(n,2_WP)) then
              is_prime = .false.
              return
          else
              w = witnesses_for_64(n)
              is_prime =       is_SPRP64(n,int(iand(w,4095_IP),WP)) &
                         .and. is_SPRP64(n,int(shifta(w,12_IP),WP))
          end if

       end if

    end function is_prime64

    ! Return the minimum factor of n for 1) n odd; 2) 1<n<N_SMALL
    elemental integer(WP) function odd_min_factor(n)
       integer(WP), intent(in) :: n
       integer(WP) :: m
       m = int(min_factor(shifta(n,1)),WP)
       odd_min_factor = merge(n,m,m==1_IP)
    end function odd_min_factor

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

    ! http://people.ksp.sk/~misof/primes
    elemental logical(LP) function is_prime_32(n, a)
        integer(IP), intent(in) :: n,a

        integer(IP) :: d,s,r,aa
        integer(WP) :: cur,pw

        aa = a
        d  = n-1_IP

        ! Find trailing zeros and shift by them
        s  = trailz(d)
        d  = shifta(d,s)

        cur = 1_WP
        pw  = d

        do while (pw/=0)
            if (iand(pw,1_WP)/=0) cur = mod(cur*aa,n)
            aa = int(mod(int(aa,WP)**2,n),IP)
            pw = shifta(pw,1)
        end do

        if (cur==1_WP) then
            is_prime_32 = .true._LP
            return
        end if

        do r=0,s-1
            if (cur==n-1) then
                is_prime_32 = .true._LP
                return
            end if
            cur = mod(cur**2,n)
        end do
        is_prime_32 = .false._LP
        return
    end function is_prime_32

    elemental integer(WP) function witnesses(n)
       integer(WP), intent(in) :: n
       integer(WP) :: i
       i =      ieor(shifta(n,16), n) * int(z'45d9f3b',WP)
       i =      ieor(shifta(i,16), i) * int(z'45d9f3b',WP)
       i = iand(ieor(shifta(i,16), i), 255_WP)
       witnesses = int(witnesses32(i+1_WP),WP)
    end function witnesses

    elemental integer(IP) function witnesses_for_64(n)
       integer(WP), intent(in) :: n
       integer(WP) :: i
       i =      ieor(shifta(n,32), n) * int(z'45d9f3b3335b369',WP)
       i =      ieor(shifta(i,32), i) * int(z'3335b36945d9f3b',WP)
       i = iand(ieor(shifta(i,32), i), 16383_WP)
       witnesses_for_64 = witnesses64(i+1_WP)
    end function witnesses_for_64

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


    ! Return a list of prime numbers between low and hi.
    pure function primes_mask_bounds(lo,hi) result(mask)
       integer(IP), intent(in) :: lo,hi
       logical(LP), allocatable :: mask(:)

       integer(IP), parameter :: i_235(3) = [integer(IP) :: 2,3,5]
       integer(IP) :: lsi,lwi,i
       logical(LP) :: has_235(3)
       logical(LP), allocatable :: wheel_sieve(:)

       ! Invalid inputs
       if (.not.(hi>=lo .and. lo>0 .and. hi<huge(hi))) then
           allocate(mask(0))
           return
       end if

       ! Internally use flexible bounds, will return a [1:hi-lo+1] array
       allocate(mask(lo:hi),source=.false._LP)

       ! Check small primes
       has_235 = lo<=i_235 .and. hi>=i_235

       if (has_235(1)) mask(2) = .true._LP
       if (has_235(2)) mask(3) = .true._LP
       if (has_235(3)) mask(5) = .true._LP

       if (hi<7) return

       wheel_sieve = primes_mask_ge7_bounds(max(7, lo), hi)
       lsi = lo - 1
       lwi = wheel_index(lsi)
       do i = 1,size(wheel_sieve,kind=IP)
           mask(wheel_prime(i+lwi)) = wheel_sieve(i)
       end do
       return
    end function primes_mask_bounds

    pure function primes_mask_hi(limit) result(mask)
       integer(IP), intent(in) :: limit
       logical(LP), allocatable :: mask(:)
       mask = primes_mask_bounds(1,limit)
    end function primes_mask_hi

    pure function primes_mask_ge7_hi(limit) result(mask)
       integer(IP), intent(in) :: limit
       logical(LP), allocatable :: mask(:)

       integer(IP) :: m,n,p,q,i,j

       ! This is an internal function that NEEDS to operate with limit>=7
       if (.not.limit>=7) then
          allocate(mask(0))
          return
       end if

       n = wheel_index(limit)
       m = wheel_prime(m)
       allocate(mask(n),source=.true._LP)

       do i=1,wheel_index(isqrt(limit))
          if (mask(i)) then
             p = wheel_prime(i)
             q = p**2
             j = iand(i-1,7_IP)+1_IP
             do while (q<=m)
                if (wheel_index(q)<=n) mask(wheel_index(q)) = .false._LP
                q = q+wheel(j)*p
                j = iand(j,7_IP)+1_IP
             end do
          endif
       end do

    end function primes_mask_ge7_hi

    pure function primes_mask_ge7_bounds(lo,hi) result(mask)
       integer(IP), intent(in) :: lo,hi
       logical(LP), allocatable :: mask(:)

       integer(IP) :: wlo,whi,m,i,j,p,q
       integer(WP) :: r
       logical(LP), allocatable :: small_mask(:)

       ! This is an internal function that needs to be called with min(lo,hi)>=7
       if (.not.min(lo,hi)>=7) then
           allocate(mask(0))
           return
       endif

       wlo = wheel_index(lo-1)
       whi = wheel_index(hi)
       m   = wheel_prime(whi)

       allocate(mask(whi-wlo), source=.true._LP)
       if (hi<49) return

       small_mask = primes_mask_ge7_hi(isqrt(hi))

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

    end function primes_mask_ge7_bounds

    ! given 0 <= a,b,n < 2^64, computes (a*b)%n without overflow
    elemental integer(WP) function safe_mul(a,b,n)
       integer(WP), intent(in) :: a,b,n

       integer(QP) :: qa,qn

       ! 128bit
       qa = int(a,QP)
       qn = int(n,QP)

       safe_mul = int(mod(qa*b,qn),WP)
    end function safe_mul

    ! given 0 <= a,b,n < 2^64, computes (a^b)%n without overflow
    elemental integer(WP) function safe_exp(a,b,n) result(res)
       integer(WP), intent(in) :: a,b,n

       integer(WP) :: pw,qa
       integer(IP) :: i

       res = 1
       pw  = 1
       qa  = a

       i   = 0
       do while (i<64 .and. pw<=b)
          if (iand(b,pw)/=0) res = safe_mul(res,qa,n)
          qa = safe_mul(qa,qa,n)
          pw = shiftl(pw,1)
          i = i+1
       end do

    end function safe_exp

    elemental logical(LP) function is_SPRP64(n,a) result(is_prime)
       integer(WP), intent(in) :: n,a

       integer(WP) :: d,cur
       integer(IP) :: s,r

       if (n==a) then
          is_prime = .true._LP
          return
       elseif (mod(n,a)==0) then
          is_prime = .false.
          return
       end if

       ! Find trailing zero bits and shift by them
       d = n-1_WP
       s = trailz(d)
       d = shifta(d,s)

       cur = safe_exp(a,d,n)
       if (cur==1_WP) then
          is_prime = .true._LP
          return
       end if

       r = 0;
       do while (r<s)
          if (cur==n-1_WP) then
             is_prime = .true._LP
             return
          end if
          cur = safe_mul(cur,cur,n)
          r = r+1
       end do
       is_prime = .false._LP
    end function is_SPRP64


    ! Find the next prime number
    elemental integer(IP) function next_prime(n, i)
       integer(IP), intent(in) :: n
       integer(IP), optional, intent(in) :: i

       integer :: usen,usei

       usei = 1_IP; if (present(i)) usei = i
       usen = max(n,1_IP)

       ! If possible, locate the next prime using the look-up table
       if (usei<=0) then !stop 'i-th next prime to be found must be >=1'
           next_prime = -huge(next_prime)
           return
       end if

       ! Handle 2
       if (usen<2_IP) then
          ! Start from 2
          usen = 2_IP
          usei = usei-1
       endif

       if (usei<=0) then
          next_prime = usen
          return
       end if

       ! Ensure that the iteration starts from a non-prime number
       if (mod(usen,2_IP)/=0) usen = usen + 1_IP

       ! While I still have to find more
       more_needed: do while (usei>0)

           do while (usen==2_IP .or. .not.is_prime(usen))
              usen = usen+1_IP  ! Skip all even numbers
           end do

           ! Prime found
           usei = usei-1_IP
           if (usei <= 0) exit

           ! Position to the next candidate
           usen = usen+1_IP
       end do more_needed

       next_prime = usen
    end function next_prime



    ! Return all prime factors, and their multiplicities, of an integer
    subroutine prime_factors(n,factors)
        integer(IP), intent(in) :: n
        integer(IP), allocatable, intent(out) :: factors(:,:)

        ! This buffer should be large enough that the product of the 1st 4196 primes
        ! is surely out of the current precision
        integer(IP) :: buffer(2,FACTORS_CHUNKSIZE),nfactors,remainder,prime

        nfactors  = 1
        buffer(FACTORS_POWER,1) = 0
        remainder = abs(n)
        prime     = 2_IP

        ! The easy test
        if (remainder<2_IP) then
           allocate(factors(2,0))
           return
        elseif (is_prime(remainder)) then
           allocate(factors(2,1))
           factors(FACTORS_PRIME,1) = remainder
           factors(FACTORS_POWER,1) = 1_IP
           return
        end if

        do while (remainder>0_IP)

            ! Prime is a factor
            if (mod(remainder,prime)==0) then
                remainder = remainder/prime

                ! Add to multiplicity
                buffer(FACTORS_POWER,nfactors) = buffer(FACTORS_POWER,nfactors)+1_IP

                if (remainder==1_IP) then
                    buffer(FACTORS_PRIME,nfactors) = prime
                    exit
                endif
            else

                ! Should we close a previous factor?
                if (buffer(FACTORS_POWER,nfactors)>0) then
                    buffer(FACTORS_PRIME,nfactors)   = prime
                    buffer(FACTORS_POWER,nfactors+1) = 0
                    nfactors = nfactors+1
                end if

                ! Try another prime
                prime = next_prime(prime)
            end if

        end do

        allocate(factors(2,nfactors),source=buffer(:,1:nfactors))

    end subroutine prime_factors

end module prime_numbers

