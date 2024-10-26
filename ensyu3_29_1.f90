module subprogs
    implicit none
contains
    function henkei1(c,i,n) result(e1)
        real(8),intent(in) :: c
        integer,intent(in) :: i, n
        real(8) :: e1(1:n,1:n)
        integer :: j

        if(i > n.or.1 > i) then
	    write(*,*) 'stop i > n or 1 > i'
	    stop
	endif
	if(c == 0._8) then
	    write(*,*) 'stop c = 0'
	endif

	e1(1:n,1:n) = 0._8
        do j = 1, n
	    if(j == i) then
                e1(j,j) = c
	    else
	        e1(j,j) = 1._8
	    endif
	enddo

    end function henkei1

    function henkei2(i,j,c,n) result(e2)
        real(8),intent(in) :: c
	integer,intent(in) :: i, j, n
	real(8) :: e2(1:n,1:n)
	integer :: k

        if(i > n.or.1 > i) then
	    write(*,*) 'stop i > n or 1 > i'
	    stop
	endif

        if(j > n.or.1 > j) then
	    write(*,*) 'stop j > n or 1 > j'
	    stop
	endif

        if(i == j) then
	    write(*,*) 'stop i = j'
	    stop
	endif

	e2(1:n,1:n) = 0._8
	e2(j,i) = c
	do k = 1, n
	    e2(k,k) = 1._8
	enddo

    end function henkei2

    function henkei3(i,j,c,n) result(e3)
        real(8),intent(in) :: c
	integer,intent(in) :: i, j, n
	real(8) :: e3(1:n,1:n), tmp(1:n)
	integer :: k

        if(i > n.or.1 > i) then
	    write(*,*) 'stop i > n or 1 > i'
	    stop
	endif

        if(j > n.or.1 > j) then
	    write(*,*) 'stop j > n or 1 > j'
	    stop
	endif

        if(i == j) then
	    write(*,*) 'stop i = j'
	    stop
	endif

	e3(1:n,1:n) = 0._8
	do k = 1, n
	    e3(k,k) = 1._8
	enddo

        tmp(1:n) = e3(i,1:n)
        e3(i,1:n) = e3(j,1:n)
	e3(j,1:n) = tmp(1:n)

    end function henkei3
end module subprogs

program main
    use subprogs
    implicit none
    integer,parameter :: n = 4, i = 2, j = 3
    real(8),parameter :: c = 2
    real(8) :: a(1:n,1:n), e(1:n,1:n), p(1:n,1:n)
    integer :: k

    call random_seed
    call random_number(a)

    write(*,*) 'original A ='
    do k = 1, n
        write(*,'(5e12.4)') a(k,1:n)
    enddo
    
    write(*,*) 'A1 ='
    e(1:n,1:n) = henkei1(c,i,n)
    p(1:n,1:n) = matmul(e,a)
    do k = 1, n
        write(*,'(5e12.4)') p(k,1:n)
    enddo

    write(*,*) 'A2 ='
    e(1:n,1:n) = henkei2(i,j,c,n)
    p(1:n,1:n) = matmul(e,a)
    do k = 1, n
        write(*,'(5e12.4)') p(k,1:n)
    enddo
    
    write(*,*) 'A3 ='
    e(1:n,1:n) = henkei3(i,j,c,n)
    p(1:n,1:n) = matmul(e,a)
    do k = 1, n
        write(*,'(5e12.4)') p(k,1:n)
    enddo

end program main
