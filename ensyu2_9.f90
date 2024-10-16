program dotp4
    implicit none
    real(8), allocatable :: u(:), v(:)
    integer :: n, i, j, fi1 = 10, fi2 = 11, fi3 = 12, fo = 13

    open(fi1, file = 'input1.d')
    open(fi2, file = 'input2.d')
    open(fi3, file = 'input3.d')
    open(fo, file = 'output.d')

    read(fi1,*) n
    allocate(u(n), v(n))

    do i = 1, n
        read(fi2,*) u(i)
    enddo

    do j = 1, n
        read(fi3,*) v(j)
    enddo

    write(fo,*) n, u, v, dot_product(u,v)
    deallocate(u,v)

    close(fi1)
    close(fi2)
    close(fi3)
    close(fo)

end program dotp4
