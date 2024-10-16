program houkou
    implicit none
    real(8) :: ca, cb, cc, u(3), l

    call random_number(u)

    u = -1._8 + 2._8 * u
    l = dot_product(u,u)
    l = sqrt(l)

    ca = u(1)/l
    cb = u(2)/l
    cc = u(3)/l

    write(*,*) 'cos alpha =', ca
    write(*,*) 'cos beta =', cb
    write(*,*) 'cos gamma =', cc
    write(*,*) 'size =', ca ** 2 + cb ** 2 + cc ** 2
end program houkou
