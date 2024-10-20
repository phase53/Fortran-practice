program Hadamard
    implicit none
    integer,parameter :: n = 3
    real(8) :: a(1:n,1:n), b(1:n,1:n), c(1:n,1:n), d(1:n,1:n)

    call random_seed
    call random_number(a(:,:))
    call random_number(b(:,:))

    c(1:n,1:n) = a(1:n,1:n) * b(1:n,1:n)
    d(1:n,1:n) = matmul(a(1:n,1:n),b(1:n,1:n))

    write(*,*) 'A =', a(:,:)
    write(*,*) 'B =', b(:,:)
    write(*,*) 'Hadamard product = =', c(:,:)
    write(*,*) 'Matrix product =', d(:,:)

end program Hadamard
