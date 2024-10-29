subroutine allocate_rmat(a,n)
    implicit none
    real(8),allocatable,intent(out) :: a(:,:)
    integer,intent(out) :: n
    
    write(*,'(a)',advance ='no') 'input n :'
    read(*,*) n
    if(n < 1.or.n > 100) then
        write(*,*) 'stopnn must be 0 < n < 100'
	stop
    endif
    allocate(a(n,n))

    
    call random_number(a)
end subroutine allocate_rmat

subroutine print_mat(a, n, m)
    implicit none
    integer,intent(in) :: n, m
    real(8),intent(in) :: a(n,m)
    integer :: i

    do i = 1, n
        write(*,'(100e12.4)') a(i,1:m)
    enddo
end subroutine print_mat
