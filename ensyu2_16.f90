program vert2
    implicit none
    integer :: n1, n2, i, j, fo = 11
    real(8),allocatable :: x(:, :, :), phi(:,:), pi, plot(:,:,:)

    open(fo,file='mat3d.d')
    write(*,*) 'input n1 (>= 2)'
    read(*,*) n1
    if(n1 < 2) then
        write(*,*) 'stop n1 < 2'
    endif

    write(*,*) 'input n2 (>= 2)'
    read(*,*) n2
    if(n2 < 2) then
        write(*,*) 'stop n2 < 2'
    endif

    allocate(x(2,n1,n2))
    allocate(phi(n1,n2))
    allocate(plot(3,n1,n2))

    pi = 2._8 * acos(0._8)

    do i = 1, n1
        x(1,i,1:n2) = dble(i-1)/dble(n1-1)
    enddo

    do i = 1, n2
        x(2,1:n1,i) = dble(i-1)/dble(n2-1)
    enddo

    do j = 1, n2
        do i = 1, n1
            phi(i, j) = (sin(pi * x(1,i,j)) * sinh(pi * (1 - x(2,i,j))))/sinh(pi)
        enddo
    enddo

    plot(1,1:n1,1:n2) = x(1,1:n1,1:n2)
    plot(2,1:n1,1:n2) = x(2,1:n1,1:n2)
    plot(3,1:n1,1:n2) = phi(1:n1,1:n2)

    do j = 1, n2
        do i = 1, n1
            write(fo,*) plot(1:3,i,j)
            write(fo,*) ''
        enddo
    enddo

    deallocate(x)
    close(fo)

end program vert2
