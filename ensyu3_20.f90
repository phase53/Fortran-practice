module subprogs
    implicit none
contains
    function siki(a) result(r)
        real(8),intent(in) :: a(2,2)
	real(8) :: r
	
	r = a(1,1) * a(2,2) - a(1,2) * a(2,1)
    end function siki
end module subprogs

program main
    use subprogs
    implicit none
    real(8) :: a(2,2), b(2,2), ab(2,2)
    integer :: i

    call random_seed
    call random_number(a)
    call random_number(b)

    write(*,*) 'A ='
    do i = 1, 2
        write(*,*) a(i,1:2)
    enddo

    write(*,*) 'B ='
    do i = 1, 2
        write(*,*) b(i,1:2)
    enddo
    
    ab = matmul(a,b)
    write(*,*) 'AB ='
    do i = 1, 2
        write(*,*) ab(i,1:2)
    enddo

    write(*,'(a)',advance = 'no') '|A|||B| ='
    write(*,*) siki(a) * siki(b)

    write(*,'(a)',advance = 'no') '|AB| ='
    write(*,*) siki(ab)
end program main
