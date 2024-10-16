program gaiseki
    implicit none
    real(8) :: u(3), v(3), w(3)

    call random_number(u)
    call random_number(v)

    u = -1 + 2 * u
    v = -1 + 2 * v

    w(1) = u(2) * v(3) - u(3) * v(2)
    w(2) = u(3) * v(1) - u(1) * v(3)
    w(3) = u(1) * v(2) - u(2) * v(1)

    write(*,*) 'u = (', u, ')'
    write(*,*) 'v = (', v, ')'
    write(*,*) 'u × v = (', w, ')'
end program gaiseki

