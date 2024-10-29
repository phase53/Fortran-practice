module interface_mod
    interface
        subroutine set_dd_mat(a,n)
            integer,intent(in) :: n
            real(8),intent(out) :: a(n,n)
        end subroutine set_dd_mat 
    end interface
end module interface_mod

subroutine set_dd_mat(a,n)
    implicit none
    integer,intent(in) :: n
    real(8),intent(out) :: a(n,n)
    integer :: i, j

    a(:,:) = 1._8
    do i = 1, n
        do while(a(i,i) < sum(a(i,1:n)) - a(i,i))
            do j = 1, n
	        if(i /= j) then
                    call random_number(a(i,j))
		else
		    a(i,j) = 1._8
	        endif
	    enddo
	enddo
    enddo

end subroutine set_dd_mat
