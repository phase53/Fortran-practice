program random_mat
    use interface_mod
    implicit none
    real(8),allocatable :: a(:,:)
    integer :: n

    call allocate_rmat(a,n)
    call print_mat(a,n,n)
end program random_mat
