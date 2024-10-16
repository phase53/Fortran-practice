program menseki
    implicit none
    real(8) :: a(3), b(3), c(3), d(3), A1, A2, s, al, bl, cl

    call random_number(a)
    call random_number(b)

    a = -5 + 10 * a
    b = -5 + 10 * b

    d(1) = a(2) * b(3) - a(3) * b(2)
    d(2) = a(3) * b(1) - a(1) * b(3)
    d(3) = a(1) * b(2) - a(2) * b(1)

    A1 = sqrt(dot_product(d,d))/2._8

    c = b - a


    al = sqrt(dot_product(a,a))
    bl = sqrt(dot_product(b,b))
    cl = sqrt(dot_product(c,c))

    s = (al + bl + cl)/2._8

    A2 = sqrt(s * (s - al) * (s - bl) * (s - cl))

    write(*,*) 'gaiseki:', A1
    write(*,*) 'Heron:', A2

end program menseki 
