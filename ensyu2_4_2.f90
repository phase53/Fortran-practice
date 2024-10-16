program norm241
    implicit none
    real(8) :: u(3), l

    call random_number(u)

    l = dot_product(u,u)
    l = sqrt(l)

    write(*,*) u
    write(*,*) l

end program norm241
