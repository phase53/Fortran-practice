module interface_mod2
    interface
        function chk_dd_mat(a,n) result(c)
            real(8),intent(in) :: a(n,n)
            integer,intent(in) :: n
            integer :: c
        end function chk_dd_mat
    end interface
end module interface_mod2



function chk_dd_mat(a,n) result(c)
    real(8),intent(in) :: a(n,n)
    integer,intent(in) :: n
    integer :: c
    integer :: i

    do i = 1, n + 1
        if(1 <= i.and.i <= n) then
            if(a(i,i) < sum(a(i,1:n)) - a(i,i)) then
	        c = 0
		exit
            endif
	else
            c = 1
	endif
    enddo

end function chk_dd_mat

