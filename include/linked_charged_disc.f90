module class_LinkedChargedDisc
  !Provides access to an ordered list of charged discs.
  !I have intentionally chosen to make this_link circular
  !so that the first and last elements are easy to obtain.
  use class_ChargedDisc
  implicit none
  private :: adjustDiscIndices
  private :: insertChargedDiscBefore
  private :: insertChargedDiscAfter

  public :: LinkedChargedDisc
  public :: initLinkedChargedDisc
  public :: nextLinkedChargedDisc
  public :: previousLinkedChargedDisc
  public :: countDiscs
  public :: getLinkedChargedDisc
  public :: getChargedDisc
  public :: freeAllLinkedDiscs
  public :: removeLinkedDisc
  public :: insertChargedDisc
  public :: reportLinkedChargedDisc


  type LinkedChargedDisc
    !Defines an ordered  list of charged discs.
    type(linkedChargedDisc), pointer :: prev_link !The first link points to the last link.
    type(ChargedDisc) :: disc
    integer :: disc_index
    logical :: disc_initialized
    type(linkedChargedDisc), pointer :: next_link !The last link points to the first link.
  end type LinkedChargedDisc

contains

  subroutine initLinkedChargedDisc(this_link, charged_disc)
    !Initializes the linked list of charged discs
    !putting the optional charged disc in the first position,
    !next_link pointo to the current and first only link, 
    !and having prev_link point to the current
    !and last link. 
    type(LinkedChargedDisc), pointer :: this_link
    type(ChargedDisc), intent(in), optional :: charged_disc
    
    allocate(this_link)
    this_link%prev_link => this_link
    this_link%disc_initialized = .TRUE.
    if( .not. present(charged_disc) ) then
      call initChargedDisc(this_link%disc,0.0)
      this_link%disc_initialized = .FALSE.
    else
      this_link%disc = copyChargedDisc(charged_disc)
    end if
    this_link%disc_index = 1
    this_link%next_link => this_link
  end subroutine initLinkedChargedDisc

  function nextLinkedChargedDisc(this_link) result(next_link)
    type(LinkedChargedDisc), pointer :: this_link
    type(LinkedChargedDisc), pointer :: next_link
  
    next_link=>this_link%next_link
  end function nextLinkedChargedDisc
   
  function previousLinkedChargedDisc(this_link) result(prev_link)
    type(LinkedChargedDisc), pointer :: this_link
    type(LinkedChargedDisc), pointer :: prev_link
  
    prev_link=>this_link%prev_link
  end function previousLinkedChargedDisc
   
  function countDiscs(this_link) result(disc_count)
    type(LinkedChargedDisc), pointer :: this_link
    type(LinkedChargedDisc), pointer :: next_link
    integer :: current_disc_index
    integer :: disc_count
 
    current_disc_index = this_link%disc_index
    next_link => nextLinkedChargedDisc(this_link)
    do while(next_link%disc_index > current_disc_index)
      current_disc_index = next_link%disc_index
      next_link => nextLinkedChargedDisc(next_link)
    end do
    disc_count = current_disc_index
  end function countDiscs
  
  function getLinkedChargedDisc(this_link, disc_index) result(linked_charged_disc)
    !Returns the linked charged disc with the disc_index.
    !If disc_index is not provided, this_link is assumed.
    type(LinkedChargedDisc), pointer :: this_link
    type(LinkedChargedDisc), pointer :: next_link
    integer, intent(in), optional :: disc_index
    type(LinkedChargedDisc), pointer :: linked_charged_disc
    integer :: first_disc_index

    if( .not. present(disc_index) .or. this_link%disc_index == disc_index ) then
      linked_charged_disc => this_link
    else  
      first_disc_index = this_link%disc_index
      next_link => nextLinkedChargedDisc(this_link)
      do while( next_link%disc_index /= disc_index .and. next_link%disc_index /= first_disc_index )
        next_link => nextLinkedChargedDisc(next_link)
      end do
      if( next_link%disc_index == first_disc_index ) then
        print *, 'No disc found with index ', disc_index
      else
        linked_charged_disc => next_link
      end if
    end if
  end function getLinkedChargedDisc
    
  function getChargedDisc(this_link, disc_index) result(charged_disc)
    !Returns the charged disc from link with the disc_index.
    !If disc_index is not provided, the present this_link is assumed.
    type(LinkedChargedDisc), pointer :: this_link
    integer, intent(in) :: disc_index
    type(LinkedChargedDisc), pointer :: linked_charged_disc
    type(ChargedDisc), pointer :: charged_disc

    linked_charged_disc => getLinkedChargedDisc(this_link,disc_index)
    charged_disc => linked_charged_disc%disc
  end function getChargedDisc
    
  subroutine freeAllLinkedDiscs(this_link)
    !Nullifies and deallocates all pointers for all links to
    !free up their memory.
    type(LinkedChargedDisc), pointer :: this_link
    type(LinkedChargedDisc), pointer :: next_link
    type(LinkedChargedDisc), pointer :: prev_link
    type(LinkedChargedDisc), pointer :: current_link

    
    !No stitching necessary, so simply nullify and deallocate
    next_link => nextLinkedChargedDisc(this_link)
    prev_link => previousLinkedChargedDisc(this_link)
    
    do while ( .not. associated(this_link,next_link) )
      current_link => next_link
      next_link => nextLinkedChargedDisc(current_link)

      !Take care of pointers within this_link and deallocate
      nullify(current_link%prev_link)
      nullify(current_link%next_link)
      deallocate(current_link)
    end do
    !Take care of pointers within this_link and deallocate
    nullify(this_link%prev_link)
    nullify(this_link%next_link)
    deallocate(this_link)
  end subroutine freeAllLinkedDiscs

  subroutine adjustDiscIndices(this_link, adjustment)
    !Adds the adjustment to the disc index of 
    !all links from this_link onward.
    type(LinkedChargedDisc), pointer :: this_link
    type(LinkedChargedDisc), pointer :: next_link
    integer, intent(in) :: adjustment
    integer :: first_disc_index
    
    first_disc_index = this_link%disc_index
    this_link%disc_index = this_link%disc_index + adjustment

    next_link => nextLinkedChargedDisc(this_link)
    do while (next_link%disc_index > first_disc_index)
      next_link%disc_index = next_link%disc_index + adjustment
      next_link => nextLinkedChargedDisc(next_link)
    end do
  end subroutine adjustDiscIndices

  subroutine removeLinkedDisc(this_link)
    !Nullifies and deallocates all pointers for one link to
    !free up its memory.  Adjusted the order index accordingly.
    type(LinkedChargedDisc), pointer :: this_link
    type(LinkedChargedDisc), pointer :: prev_link
    type(LinkedChargedDisc), pointer :: next_link

    !Stitch the left and right links
    next_link => nextLinkedChargedDisc(this_link)
    prev_link => previousLinkedChargedDisc(this_link)
    next_link%prev_link => prev_link
    prev_link%next_link => next_link
    if( next_link%disc_index /= 1 ) then
      call adjustDiscIndices(next_link,-1)
    end if
    
    !Take care of pointers within this_link and deallocate
    nullify(this_link%prev_link)
    nullify(this_link%next_link)
    deallocate(this_link)

  end subroutine removeLinkedDisc

  subroutine insertChargedDiscBefore(this_link, charged_disc)
    !Creates a new link before this_link and inserts the
    !charged disc there.  If it is before the "1" link, this
    !is actually appending on the end of the chain.
    use class_ChargedDisc
    type(LinkedChargedDisc), pointer :: this_link
    type(LinkedChargedDisc), pointer :: prev_link
    type(LinkedChargedDisc), pointer :: new_link
    type(ChargedDisc), intent(in) :: charged_disc

    prev_link => previousLinkedChargedDisc(this_link)

    !Create new_link and link it.
    allocate(new_link)
    new_link%prev_link => prev_link
    new_link%disc = copyChargedDisc(charged_disc)
    if( prev_link%disc_index < this_link%disc_index) then
      new_link%disc_index = this_link%disc_index
      call adjustDiscIndices(this_link,1)
    else !Remember circular, so before the first is on the end.
      new_link%disc_index = prev_link%disc_index + 1
    end if
    new_link%next_link => this_link
    new_link%disc_initialized = .TRUE.

    !Stitch
    prev_link%next_link => new_link
    this_link%prev_link => new_link

  end subroutine insertChargedDiscBefore

  subroutine insertChargedDiscAfter(this_link, charged_disc)
    !Creates a new link before this_link and inserts the
    !charged disc there. 
    type(LinkedChargedDisc), pointer :: this_link
    type(LinkedChargedDisc), pointer :: next_link
    type(LinkedChargedDisc), pointer :: new_link
    type(ChargedDisc), intent(in) :: charged_disc

    next_link => nextLinkedChargedDisc(this_link)

    !Create new_link and link it.
    allocate(new_link)
    new_link%prev_link => this_link
    new_link%disc = copyChargedDisc(charged_disc)
    new_link%disc_index = this_link%disc_index
    new_link%next_link => next_link
    new_link%disc_initialized = .TRUE.

    !Stitch
    this_link%next_link => new_link
    next_link%prev_link => new_link

    call adjustDiscIndices(new_link,1)
  end subroutine insertChargedDiscAfter

  subroutine insertChargedDisc(this_link, charged_disc, position)
    !Determines if the link has a charged disc, and if not, assigns
    !the disc there.  Otherwise, inserts the disc according to the
    !position direction ("after", "before", or "append").  By default, position
    !is after.
    use stringFunctions
    type(LinkedChargedDisc), pointer :: this_link
    type(LinkedChargedDisc), pointer :: first_link
    type(ChargedDisc), intent(in) :: charged_disc
    character(len=*), intent(in), optional :: position
    character(len=6) :: lower_position

    if( .not. present(position) ) then
      lower_position = "append"
    else
      lower_position = to_lower(position)
    end if
    if ( .not. this_link%disc_initialized ) then
      !Only necessary for linked lists initiated without a disc.
      this_link%disc = copyChargedDisc(charged_disc)
      this_link%disc_initialized = .TRUE.
    else
      lower_position = to_lower(position)
      if( lower_position == "before" ) then
        call insertChargedDiscBefore(this_link,charged_disc)
      elseif( lower_position == "append" ) then
        first_link => getLinkedChargedDisc(this_link, 1)
        call insertChargedDiscBefore(first_link,charged_disc)
      else
        call insertChargedDiscAfter(this_link,charged_disc)
      end if
    end if
  end subroutine insertChargedDisc

 subroutine reportLinkedChargedDisc(this_link,only_disc_i)
    type(LinkedChargedDisc), pointer :: this_link
    type(LinkedChargedDisc), pointer:: current_link
    integer, optional, intent(in) :: only_disc_i
    if( .not. present(only_disc_i) ) then
      current_link => getLinkedChargedDisc(this_link,1)      
      print *, "Disc index = ", current_link%disc_index
      call reportChargedDisc(current_link%disc)
      current_link => nextLinkedChargedDisc(current_link)      
      do while(current_link%disc_index > 1)
        print *, "Disc index = ", current_link%disc_index
        call reportChargedDisc(current_link%disc)
        current_link => nextLinkedChargedDisc(current_link)      
      end do
    else
      current_link => getLinkedChargedDisc(this_link,only_disc_i)      
      print *, "Disc index = ", current_link%disc_index
      if( .not. this_link%disc_initialized ) then
        print *, 'Not initialized'
      end if
      call reportChargedDisc(current_link%disc)
    end if
  end subroutine reportLinkedChargedDisc

end module class_LinkedChargedDisc
