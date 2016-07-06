library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ent is
end entity ent;

architecture arch of ent is
  constant test: natural := 3;
begin
  LL: if test=10 generate
   begin
   end;
  elsif test=5 generate
   begin
   end;
  end generate;
end architecture arch;


