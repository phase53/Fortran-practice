program Fibonacci
    implicit none
    integer i, a, b, temp
    a = 1
    b = 2
    do i = 1, 10
        if(i == 1) then
            write(*,*) 'a', i, '= ',a
        else if(i == 2) then
            write(*,*) 'a', i, '= ',b
        else if(i >= 3) then
            write(*,*) 'a', i, '=', a+b
            temp = b
            b = a + temp
            a = temp
        endif
    enddo
end program Fibonacci
        
