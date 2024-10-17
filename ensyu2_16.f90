program vert2
    implicit none
    integer :: n, i, j, fo = 11
    real(8),allocatable :: x(:, :, :), phi(:,:), pi, plot(:,:,:)

    open(fo,file='mat3d.d')
    write(*,*) 'input n (>= 2)'
    read(*,*) n
    if(n < 2) then
        write(*,*) 'stop n < 2'
    endif
    allocate(x(2,n,n))
    allocate(phi(n,n))
    allocate(plot(3,n,n))

    pi = 2._8 * acos(0._8)

    do i = 1, n
        x(1,i,1:n) = dble(i-1)/dble(n-1)
    enddo

    do i = 1, n
        x(2,1:n,i) = dble(i-1)/dble(n-1)
    enddo

    do j = 1, n
        do i = 1, n
            phi(i, j) = (sin(pi * x(1,i,j)) * sinh(pi * (1 - x(2,i,j))))/sinh(pi)
        enddo
    enddo

    plot(1,1:n,1:n) = x(1,1:n,1:n)
    plot(2,1:n,1:n) = x(2,1:n,1:n)
    plot(3,1:n,1:n) = phi(1:n,1:n)

    do j = 1, n
        do i = 1, n - 1
            write(fo,*) x(1:3,i,j)
            write(fo,*) ''
        enddo
    enddo

    deallocate(x)
    close(fo)

end program vert2
