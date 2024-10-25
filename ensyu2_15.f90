program vert
    implicit none
    integer :: n1, n2, i, j, fo = 11
    real(8),allocatable :: x(:, :, :)

    open(fo,file='mat2d.d')
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

    do i = 1, n1
        x(1,i,1:n2) = dble(i-1)/dble(n1-1)
    enddo

    do i = 1, n2
        x(2,1:n1,i) = dble(i-1)/dble(n2-1)
    enddo

    do j = 1, n2
        do i = 1, n1 - 1
            write(fo,*) x(1:2,i,j)
            write(fo,*) x(1:2,i+1,j)
            write(fo,*) ''
        enddo
    enddo

    do j = 1, n2 - 1
        do i = 1, n1
            write(fo,*) x(1:2,i,j)
            write(fo,*) x(1:2,i,j+1)
            write(fo,*) ''
        enddo
    enddo
    
    deallocate(x)
    close(fo)
end program vert
    

    
