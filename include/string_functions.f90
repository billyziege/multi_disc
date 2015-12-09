module stringFunctions
  implicit none
  
  contains
  
  function to_upper(string_in) result (string_out)
    !Adapted from http://www.star.le.ac.uk/~cgp/fortran.html
    !Original author: Clive page
    !Only works for strings of characters.  No numbers!
    implicit none
    character(len=*), intent(in) :: string_in
    character(len=len(string_in)) :: string_out
    integer :: character_index, character_code
  
    do character_index = 1, len(string_in)
      character_code = iachar(string_in(character_index:character_index))
      if ( character_code >= iachar("a") .and. character_code <= iachar("z") ) then
        string_out(character_index:character_index) = achar(character_code - 32 )
      else
        string_out(character_index:character_index) = achar(character_code)
      end if
    end do
  end function
  
  function to_lower(string_in) result (string_out)
    !Adapted from http://www.star.le.ac.uk/~cgp/fortran.html
    !Original author: Clive page
    !Only works for strings of characters.  No numbers!
    implicit none
    character(len=*), intent(in) :: string_in
    character(len=len(string_in)) :: string_out
    integer :: character_index, character_code
  
    do character_index = 1, len(string_in)
      character_code = iachar(string_in(character_index:character_index))
      if ( character_code >= iachar("a") .and. character_code <= iachar("z") ) then
        string_out(character_index:character_index) = achar(character_code)
      else
        string_out(character_index:character_index) = achar(character_code + 32 )
      end if
    end do
  end function
end module stringFunctions
