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

entity pkg01 is
  port (i : in integer;
        o : out integer);
end pkg01;

architecture behav of pkg01 is
  package inst1 is new work.gen generic map (val => 2);
  use inst1.all;
begin
  o <= get_val (i);
end behav;
