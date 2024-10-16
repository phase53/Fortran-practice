program copy1
    implicit none
    real(8) :: ia(1:4)
    integer :: i

    ia(1:4) = (/1, 2, 3, 4/)
    
    do i = 2, 4
        ia(i) = ia(i-1)
    enddo

    write(*,*) 'ia =', ia(1:4) 

end program copy1
