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
!     test_primes
!     Perform prime-related tests
!     Created: 09/23/2023
!
!  ************************************************************************************************************
program test_primes

    use prime_numbers
    use prime_constants
    use iso_fortran_env, only: output_unit

    implicit none

    character(*), parameter :: this_test = '[test_primes]'
    integer :: nfailed,npassed

    ! Local variables
    character(*), parameter :: fmt_failed = "(1x,a,' has ',i0,' test passed, ',i0,' not passed.')"
    nfailed = 0
    npassed = 0

    ! Perform tests
    call add_test(test_first_10000())
    call add_test(test_is_prime())
    call add_test(test_vs_c())
    call add_test(test_next_prime())
    call add_test(test_factors())

    write(*,fmt_failed)this_test,npassed,nfailed
    if (nfailed>0) then
        stop -1
    else
        stop 0
    endif

    return

    contains

    subroutine add_test(success_flag)
        logical, intent(in) :: success_flag
        if (success_flag) then
           npassed = npassed+1
        else
           nfailed = nfailed+1
        end if
    end subroutine add_test

    ! Test first 1000 primes
    logical function test_first_10000() result(success)

        integer(IP) :: tenthousandth

        tenthousandth = p1_to_10000(chunk)

        success = all(primes(tenthousandth) == p1_to_10000) .and. &
                  all(primes(2,tenthousandth) == pack(p1_to_10000,p1_to_10000>=2))

    end function test_first_10000

    logical function test_is_prime() result(success)

       success = .not.is_prime( 1000000003); if (.not.success) return
       success = .not.is_prime( 1000000005); if (.not.success) return
       success =      is_prime( 1000000007); if (.not.success) return
       success =      is_prime( 1000000009); if (.not.success) return
       success = .not.is_prime( 1000000011); if (.not.success) return
       success = .not.is_prime( 1000000013); if (.not.success) return
       success = .not.is_prime( 1000000015); if (.not.success) return
       success = .not.is_prime( 1000000017); if (.not.success) return
       success = .not.is_prime( 1000000019); if (.not.success) return
       success =      is_prime( 1000000021); if (.not.success) return
       success = .not.is_prime( 1000000023); if (.not.success) return
       success =      is_prime(10000000019_WP); if (.not.success) return
       success = .not.is_prime(10000000020_WP); if (.not.success) return
       success = .not.is_prime(10000000021_WP); if (.not.success) return

       ! Clausen prime
       success =      is_prime(67280421310721_WP); if (.not.success) return

    end function test_is_prime

    logical function test_vs_c() result(success)
       use iso_c_binding

       interface
           logical(c_bool) function is_SPRP_c_32(n,a) bind(C,name="is_SPRP_c_32")
              use iso_c_binding, only: c_bool,c_int32_t
              integer(c_int32_t), intent(in), value :: n,a
           end function is_SPRP_c_32
           logical(c_bool) function is_prime_c_32(x) bind(C,name="is_prime_c_32")
              use iso_c_binding, only: c_bool,c_int32_t
              integer(c_int32_t), intent(in), value :: x
           end function is_prime_c_32
       end interface

       integer(c_int32_t) :: x

       do x=huge(x)-10000,huge(x)-1
          success = is_prime(x) .eqv. is_prime_c_32(x)
          if (.not.success) then
             print *, 'discrepancy at x=',x
          end if
       end do

    end function test_vs_c

    logical function test_next_prime() result(success)

       integer(IP), parameter :: N1(*) = [-1000, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,  2**20,2**30]
       integer(IP), parameter :: N2(*) = [    2, 3, 5, 5, 7, 7,11,11,11,11,13,13,1048583,1073741827]
       integer(IP), parameter :: N3(*) = [    3, 5, 7, 7,11,11,13,13,13,13,17,17,1048589,1073741831]

       integer :: i

       do i=1,size(N1)
          success = next_prime(N1(i))==N2(i)
          if (.not.success) then
              print *, 'next_prime(',N1(i),') = ',N2(i),', but returned ',next_prime(N1(i))
              return
          end if
          success = next_prime(N1(i),1)==N2(i)
          if (.not.success) then
              print *, 'next_prime(',N1(i),',1) = ',N2(i),', but returned ',next_prime(N1(i),1)
              return
          end if
          success = next_prime(N1(i),2)==N3(i)
          if (.not.success) then
              print *, 'next_prime(',N1(i),',2) = ',N3(i),', but returned ',next_prime(N1(i),2)
              return
          end if
       end do

       ! The second prime number after -20 is 3 ([2, 3, 5, ...])
       success = next_prime(-20,2) == 3; if (.not.success) return

       ! The fifth prime number after 4 is 17 ([5, 7, 11, 13, 17]), etc.
       success = next_prime(  2)   == 3;  if (.not.success) return
       success = next_prime(  4)   == 5;  if (.not.success) return
       success = next_prime(  4,1) == 5;  if (.not.success) return
       success = next_prime(  4,2) == 7;  if (.not.success) return
       success = next_prime(  4,3) == 11; if (.not.success) return
       success = next_prime(  4,4) == 13; if (.not.success) return
       success = next_prime(  4,5) == 17; if (.not.success) return
       success = next_prime(  4,6) == 19; if (.not.success) return

       success = next_prime(  0,2) == 3;  if (.not.success) return
       success = next_prime(  0,3) == 5;  if (.not.success) return

    end function test_next_prime

    ! Test factors
    logical function test_factors() result(success)

        integer(IP) :: i
        integer(IP), allocatable :: factors(:,:)

        do i=1,99999
            call prime_factors(i,factors)
            success = sign(product(factors(FACTORS_PRIME,:)**factors(FACTORS_POWER,:)),i)==i &
                      .and. all(is_prime(factors(FACTORS_PRIME,:)))
            if (.not.success) then
                print 1, i,factors
                return
            end if
        end do


        1 format('prime_factors(',i0,') error: returned ',*(i0,'^',i0,:,' * '))


    end function test_factors

end program test_primes

