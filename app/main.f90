program generate_module
    use generate_prime_numbers_module
    use http, only: response_type, request
    implicit none
    type(response_type) :: response
    integer :: file

    !> Download list of first 1,000,000 prime numbers
    response = request('https://t5k.org/lists/small/millions/primes1.zip')

    if (response%ok) then

        stop 'unzip file'

    else
        error stop response%err_msg
    end if

    call create_primes 
end program
