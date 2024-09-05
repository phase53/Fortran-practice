program daensekibun
    implicit none
    real(8) :: a, b, c1, c2, R1, R2, k, pi
    integer :: i, j, n, fo = 11
    
    open(fo, file = 'output.d')
    do n = 1, 100
        pi = 2.0_8 * acos(0.0_8)
        c1 = 1.0_8
        c2 = 1.0_8
        k = 0.5_8

        do i = 2, n
            a = 1.0_8
            b = 1.0_8
            do j = 2, i
                a = a * (((2.0_8 * real(j - 1, kind = 8)) - 1.0_8) ** 2)
                b = b * ((2.0_8 * real(j - 1, kind = 8)) ** 2)
            enddo
            c1 = c1 + (a/b) * (k ** (2.0_8 * real(i - 1, kind = 8)))
        enddo

        if(n >= 3) then
            do i = 2, n - 1
                a = 1.0_8
                b = 1.0_8
                do j = 2, i
                    a = a * (((2.0_8 * real(j - 1, kind = 8)) - 1.0_8) ** 2)
                    b = b * ((2.0_8 * real(j - 1, kind = 8)) ** 2)
                enddo
                c2 = c2 + (a/b) * (k ** (2.0_8 * real(i - 1, kind = 8)))
            enddo
        endif

        R1 = (pi/2.0_8) * c1
        R2 = (pi/2.0_8) * c2
        write(fo,*) n, abs(R1 - R2)
    enddo
    close(fo)

end program daensekibun
