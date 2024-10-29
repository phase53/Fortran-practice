module interface_mod
    interface
        subroutine allocate_rmat(a,n)
            real(8),allocatable,intent(out) :: a(:,:)
            integer,intent(out) :: n
	end subroutine allocate_rmat

        subroutine print_mat(a, n, m)
            integer,intent(in) :: n, m
            real(8),intent(in) :: a(n,m)
	end subroutine print_mat
    end interface
end module interface_mod

