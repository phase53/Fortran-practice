program copy2
    implicit none
    real(8) :: ia(1:4)

    ia(1:4) = (/1, 2, 3, 4/)
    
    ia(2:4) = ia(1:3)

    write(*,*) 'ia =', ia(1:4) 

end program copy2
