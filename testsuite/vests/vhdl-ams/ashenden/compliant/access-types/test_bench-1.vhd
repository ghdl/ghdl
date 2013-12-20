
-- Copyright (C) 2002 Morgan Kaufmann Publishers, Inc

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

-- not in book

package stimulus_element_ordered_collection_adt is

  -- template: fill in the placeholders to specialize for a particular type

  alias element_type is work.stimulus_types.stimulus_element;
  alias key_type is delay_length;
  alias key_of is work.stimulus_types.stimulus_key [ element_type return key_type ];
  alias "<" is std.standard."<" [ key_type, key_type return boolean ];

  -- types provided by the package

  type ordered_collection_object;    -- private
  type position_object;              -- private

  type ordered_collection is access ordered_collection_object;
  type position is access position_object;

  -- operations on ordered collections

  function new_ordered_collection return ordered_collection;
  -- returns an empty ordered collection of element_type values

  procedure insert ( c : inout ordered_collection;  e : in element_type );
  -- inserts e into c in position determined by key_of(e)

  procedure get_element ( variable p : in position;  e : out element_type );
  -- returns the element value at position p in its collection

  procedure test_null_position ( variable p : in position;  is_null : out boolean );
  -- test whether p refers to no position in its collection

  procedure search ( variable c : in ordered_collection;  k : in key_type;
                     p : out position );
  -- searches for an element with key k in c, and returns the position of
  -- that element, or, if not found, a position for which test_null_position
  -- returns true

  procedure find_first ( variable c : in ordered_collection;  p : out position );
  -- returns the position of the first element of c

  procedure advance ( p : inout position );
  -- advances p to the next element in its collection,
  -- or if there are no more, sets p so that test_null_position returns true

  procedure delete ( p : inout position );
  -- deletes the element at position p from its collection, and advances p

  -- private types: pretend these are not visible

  type ordered_collection_object is
    record
      element : element_type;
      next_element, prev_element : ordered_collection;
    end record ordered_collection_object;

  type position_object is
    record
      the_collection : ordered_collection;
      current_element : ordered_collection;
    end record position_object;

end package stimulus_element_ordered_collection_adt;



package body stimulus_element_ordered_collection_adt is

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

end package body stimulus_element_ordered_collection_adt;



entity test_bench is
end entity test_bench;

-- end not in book


architecture initial_test of test_bench is

  use work.stimulus_types.all;

  -- . . .    -- component and signal declarations

  -- not in book
  signal dut_signals : real_vector(0 to stimulus_vector_length - 1);
  -- end not in book

begin

  -- . . .    -- instantiate design under test

  stimulus_generation : process is

    use work.stimulus_element_ordered_collection_adt.all;

    variable stimulus_list : ordered_collection := new_ordered_collection;
    variable next_stimulus_position : position;
    variable next_stimulus : stimulus_element;
    variable position_is_null : boolean;

  begin
    insert(stimulus_list, stimulus_element'(0 ns, real_vector'(0.0, 5.0, 0.0, 2.0)));
    insert(stimulus_list, stimulus_element'(200 ns, real_vector'(3.3, 2.1, 0.0, 2.0)));
    insert(stimulus_list, stimulus_element'(300 ns, real_vector'(3.3, 2.1, 1.1, 3.3)));
    insert(stimulus_list, stimulus_element'(50 ns, real_vector'(3.3, 3.3, 2.2, 4.0)));
    insert(stimulus_list, stimulus_element'(60 ns, real_vector'(5.0, 3.3, 4.0, 2.2)));
    -- . . .
    -- not in book
    insert(stimulus_list, stimulus_element'(100 ns, real_vector'(0.0, 0.0, 0.0, 0.0)));
    search(stimulus_list, 100 ns, next_stimulus_position);
    delete(next_stimulus_position);
    get_element(next_stimulus_position, next_stimulus);
    -- end not in book
    find_first(stimulus_list, next_stimulus_position);
    loop
      test_null_position(next_stimulus_position, position_is_null);
      exit when position_is_null;
      get_element(next_stimulus_position, next_stimulus);
      wait for next_stimulus.application_time - now;
      dut_signals <= next_stimulus.pattern;
      advance(next_stimulus_position);
    end loop;
    wait;
  end process stimulus_generation;

end architecture initial_test;
