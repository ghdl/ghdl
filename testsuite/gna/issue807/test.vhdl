library ieee;
use ieee.std_logic_1164.all;

package test_pkg is

  type record_t is record
    data : std_ulogic_vector;
  end record;

  procedure test_procedure(
    variable rec : out record_t);

  function test_function
    return record_t;

  function get_data
    return std_ulogic_vector;

end package;

package body test_pkg is

  procedure test_procedure(
    variable rec : out record_t)
  is
  begin
    rec := test_function;
  end procedure;

  function test_function
    return record_t
  is
  begin
    return (data => get_data);
  end function;

  function get_data
    return std_ulogic_vector
  is
  begin
    return x"0F";
  end function;

end package body;
