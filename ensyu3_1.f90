module subprog
    implicit none
contains 
    subroutine swap(a,b)
        integer :: a, b
        integer :: tmp
        tmp = a
        a = b
        b = tmp
    end subroutine swap
end module subprog

program exchange
    use subprog
    implicit none
    real(8) :: x = 77, y = 9095, tmp = 0 !module内は整数型，program内は実数型
    write(*,*) 'x, y, tmp =', x, y, tmp
    call swap(x,y)
    write(*,*) 'x, y, tmp =', x, y, tmp
end program exchange
