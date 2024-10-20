program tenchi2
    implicit none
    integer,parameter :: n = 3
    real(8) :: a(1:n,1:n), b(1:n,1:n), c(1:n,1:n), d(1:n,1:n), e(1:n,1:n), f(1:n,1:n), g(1:n,1:n)
    integer :: i, j, k

    call random_seed
    call random_number(a(:,:))
    call random_number(b(:,:))

    !(AB)^Tの計算

    c(1:n,1:n) = matmul(a(1:n,1:n),b(1:n,1:n))

    d(1:n,1:n) = transpose(c(1:n,1:n))

    !(B^T)(A^T)の計算

    e(1:n,1:n) = transpose(a(1:n,1:n))
    
    f(1:n,1:n) = transpose(b(1:n,1:n))

    g(1:n,1:n) = matmul(f(1:n,1:n),e(1:n,1:n))

    write(*,*) 'A =', a
    write(*,*) 'B =', b
    write(*,*) '(AB)^T =', d
    write(*,*) '(B^T)(A^T) =', g

end program tenchi2
