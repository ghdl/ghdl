package gen is
  generic (val : integer);

  function get_val (x : integer) return integer;
end;

package body gen is
  function get_val (x : integer) return integer is
  begin
    return val + x;
  end get_val;
end;

package inst1 is new work.gen generic map (val => 3);

entity pkg02 is
  port (i : in integer;
        o : out integer);
end pkg02;

architecture behav of pkg02 is
  use work.inst1.all;
begin
  o <= get_val (i);
end behav;
