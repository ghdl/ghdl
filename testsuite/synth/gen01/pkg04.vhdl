package gen is
  generic (val : integer);

  constant c : integer := val;

  function get_val (x : integer := c) return integer;
end;

package body gen is
  function get_val (x : integer := c) return integer is
  begin
    return x + val;
  end get_val;
end;

package inst1 is new work.gen generic map (val => 5);

entity pkg04 is
  port (i : in integer;
        o : out integer);
end pkg04;

architecture behav of pkg04 is
  use work.inst1.all;
begin
  o <= i + get_val;
end behav;
