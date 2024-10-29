module interface_mod
    interface
        function revchar(c) result(rc)
            character(*),intent(in) :: c
            character(len(c)) :: rc
	end function revchar
    end interface
end module interface_mod
