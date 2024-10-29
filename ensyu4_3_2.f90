function revchar(c) result(rc)
    implicit none
    character(*),intent(in) :: c
    character(len(c)) :: rc
    integer :: i
    do i = 1, len(c)
        rc(i:i) = c(len(c) - i + 1:len(c) - i + 1)
    enddo
end function revchar
