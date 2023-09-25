!   ***********************************************************************************************
!   **                                                                                           **
!   **                  |\   -  -.   ./                                                          **
!   **                  | \./ \/ | ./ /     __________  ___________ __________                   **
!   **                __|         /  /     / ____/ __ \/ ____/ ___// ____/ __ \                  **
!   **                \    .        /.-/  / /_  / /_/ / __/  \__ \/ /   / / / /                  **
!   **                 \   |\.|\/|    /  / __/ / _, _/ /___ ___/ / /___/ /_/ /                   **
!   **                  \__\     /___/  /_/   /_/ |_/_____//____/\____/\____/                    **
!   **                                                                                           **
!   **                                     FRESCO-SpeedCHEM                                      **
!   **            A code for internal combustion engine flows with chemical kinetics             **
!   **                                                                                           **
!   ***********************************************************************************************
!   **                                                                                           **
!   **    test_primes                                                                            **
!   **    Perform prime-related tests                                                            **
!   **                                                                                           **
!   ***********************************************************************************************
!   **                                                                                           **
!   **    Author     : (C) Federico Perini                                                       **
!   **    Created    : 09/23/2023                                                                **
!   **                                                                                           **
!   ***********************************************************************************************
program test_primes

    use prime_numbers
    use prime_constants
    use fortran_io
    use iso_fortran_env, only: output_unit

    implicit none

    character(*), parameter :: this_test = '[test_primes]'
    integer :: nfailed,npassed

    ! Local variables
    integer, allocatable :: w64(:)
    integer      :: i,j,ierr
    character(*), parameter :: fmt_failed = "(1x,a,' has ',i0,' test passed, ',i0,' not passed.')"
    nfailed = 0
    npassed = 0

    ! Perform tests
    call add_test(test_first_10000())
    call add_test(test_is_prime())
    call add_test(test_vs_c())

    if (nfailed>0) then
        write(*,fmt_failed)this_test,npassed,nfailed
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
!
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

    end function test_vs_c

    logical function make_min_factors() result(success)

       use fortran_io

       integer, allocatable :: min_factors_table(:)

       ! Generate list of min factors
       integer :: N_SMALL = 2**18

       ! min_factors = the minimum factor of n for odd n, if 3<n<N_SMALL_FACTORS. Return 1 if prime
       min_factors_table = generate_min_factors(N_SMALL)

       call print_1d_array(output_unit,'min_factor',min_factors_table,'IP')
       success = .true.

    end function make_min_factors


end program test_primes

