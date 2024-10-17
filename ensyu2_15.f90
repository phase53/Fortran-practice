program vert
    implicit none
    integer :: n, i, j, fo = 11
    real(8),allocatable :: x(:, :, :)

    open(fo,file='mat2d.d')
    write(*,*) 'input n (>= 2)'
    read(*,*) n
    if(n < 2) then
        write(*,*) 'stop n < 2'
    endif
    allocate(x(2,n,n))

    do i = 1, n
        x(1,i,1:n) = dble(i-1)/dble(n-1)
    enddo

    do i = 1, n
        x(2,1:n,i) = dble(i-1)/dble(n-1)
    enddo

    do j = 1, n
        do i = 1, n - 1
            write(fo,*) x(1:2,i,j)
            write(fo,*) x(1:2,i+1,j)
            write(fo,*) ''
        enddo
    enddo

    do j = 1, n - 1
        do i = 1, n
            write(fo,*) x(1:2,i,j)
            write(fo,*) x(1:2,i,j+1)
            write(fo,*) ''
        enddo
    enddo
    
    deallocate(x)
    close(fo)
end program vert
    

    
