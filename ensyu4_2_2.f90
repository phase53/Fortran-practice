subroutine print_title(title)
    character(*),intent(in) :: title
    
    write(*,*) title, len(title)
end subroutine print_title
