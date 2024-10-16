program seikika
    implicit none
    real(8) :: u(3), l, zero(3)

    call random_number(u(:))
    zero(:) = 0._8

    if(all(u == zero)) then
        write(*,*) 'u is zero vector'
        stop
    endif

    l = dot_product(u(:),u(:))
    l = sqrt(l)
    
    u(:) = u(:)/l
    l = dot_product(u(:),u(:))
    l = sqrt(l)

    
    write(*,*) u(:)
    write(*,*) 'norm of u :', l 
end program seikika
