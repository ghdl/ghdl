library ieee;
use ieee.std_logic_1164.all;

entity ent1 is
end;

architecture arch of ent1 is

    procedure f(a : std_logic_vector(open)) is
    begin
      report "a(a'high)=" & std_logic'image(a(a'high));
    end procedure;

begin
  f ("0110");
end;
