module subprog
    implicit none
contains
    subroutine plus(i,j,k)
        integer :: i, j, k
        
	k = i + j
    end subroutine plus
end module subprog

program tashizan
    use subprog
    implicit none
    integer :: i = 12, j = 13, k

    call plus(i,j,k)
    write(*,*) 'i, j, k =', i, j, k
end program tashizan

