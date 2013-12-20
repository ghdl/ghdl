
-- Copyright (C) 1996 Morgan Kaufmann Publishers, Inc

-- This file is part of VESTs (Vhdl tESTs).

-- VESTs is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation; either version 2 of the License, or (at
-- your option) any later version. 

-- VESTs is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
-- for more details. 

-- You should have received a copy of the GNU General Public License
-- along with VESTs; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA 

-- ---------------------------------------------------------------------
--
-- $Id: ch_17_fg_17_16.vhd,v 1.2 2001-10-26 16:29:37 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package body «element_type_simple_name»_ordered_collection_adt is

  function new_ordered_collection return ordered_collection is
    variable result : ordered_collection := new ordered_collection_object;
  begin
    result.next_element := result;
    result.prev_element := result;
    return result;
  end function new_ordered_collection;

  procedure insert ( c : inout ordered_collection;  e : in element_type ) is
    variable current_element : ordered_collection := c.next_element;
    variable new_element : ordered_collection;
  begin
    while current_element /= c
      and key_of(current_element.element) < key_of(e) loop
      current_element := current_element.next_element;
    end loop;
    -- insert new element before current_element
    new_element := new ordered_collection_object'(
      element => e,
      next_element => current_element,
      prev_element => current_element.prev_element );
    new_element.next_element.prev_element := new_element;
    new_element.prev_element.next_element := new_element;
  end procedure insert;

  procedure get_element ( variable p : in position;  e : out element_type ) is
  begin
    e := p.current_element.element;
  end procedure get_element;

  procedure test_null_position ( variable p : in position;  is_null : out boolean ) is
  begin
    is_null := p.current_element = p.the_collection;
  end procedure test_null_position;

  procedure search ( variable c : in ordered_collection;  k : in key_type;
  p : out position ) is
    variable current_element : ordered_collection := c.next_element;
  begin
    while current_element /= c
      and key_of(current_element.element) < k loop
      current_element := current_element.next_element;
    end loop;
    if current_element = c or k < key_of(current_element.element) then
      p := new position_object'(c, c);  -- null position
    else
      p := new position_object'(c, current_element);
    end if;
  end procedure search;

  procedure find_first ( variable c : in ordered_collection;  p : out position ) is
  begin
    p := new position_object'(c, c.next_element);
  end procedure find_first;

  procedure advance ( p : inout position ) is
    variable is_null : boolean;
  begin
    test_null_position(p, is_null);
    if not is_null then
      p.current_element := p.current_element.next_element;
    end if;
  end procedure advance;

  procedure delete ( p : inout position ) is
    variable is_null : boolean;
  begin
    test_null_position(p, is_null);
    if not is_null then
      p.current_element.next_element.prev_element
        := p.current_element.prev_element;
      p.current_element.prev_element.next_element
        := p.current_element.next_element;
      p.current_element := p.current_element.next_element;
    end if;
  end procedure delete;

end package body «element_type_simple_name»_ordered_collection_adt;
