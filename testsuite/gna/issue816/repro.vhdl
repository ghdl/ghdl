library ieee;
context ieee.ieee_std_context;

package integer_vector_ptr_pkg is
  subtype index_t is integer range -1 to integer'high;
  type integer_vector_ptr_t is record
    index : index_t;
  end record;

  impure function get(ptr : integer_vector_ptr_t; index : integer)
    return integer;
  
end package;

entity tb_test is
  generic ( runner_cfg : string );
end entity;

use work.integer_vector_ptr_pkg.all;

architecture tb of tb_test is

  constant params: integer_vector_ptr_t := (index => 0);

  type time_t is array (natural range 0 to 1) of natural;

  procedure get_time(variable t: inout time_t) is begin
    t(0) := get(params, 0);
    t(1) := get(params, 1);
  end;

begin

  run: process(all)
    variable r: time_t;
  begin
      get_time(r);
  end process;

end architecture;
