package gen is
  generic (max : natural);

  subtype my_int is integer range 0 to max;

  function get_val (x : my_int) return my_int;
end;

package body gen is
  function get_val (x : my_int) return my_int is
  begin
    return x + 1;
  end get_val;
end;

package inst1 is new work.gen generic map (max => 3);

entity pkg03 is
  port (i : in integer;
        o : out integer);
end pkg03;

architecture behav of pkg03 is
  use work.inst1.all;
begin
  o <= get_val (i);
end behav;
