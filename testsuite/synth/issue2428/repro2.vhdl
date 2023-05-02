library IEEE;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity repro2 is
  port (a : unsigned (0 to 7);
        o : out unsigned (7 downto 0));
end;

architecture behav of repro2 is
  function pkg_unsigned (lit : unsigned) return unsigned is
    alias ret : unsigned(lit'length-1 downto 0) is lit;
  begin
    return ret;
  end pkg_unsigned;
begin
  o <= pkg_unsigned (a);
end;
