library IEEE;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity repro1 is
end;

architecture behav of repro1 is
  function pkg_unsigned (lit : unsigned) return unsigned is
    alias ret : unsigned(lit'length-1 downto 0) is lit;
  begin
    return ret;
  end pkg_unsigned;

  constant c : unsigned := pkg_unsigned("0011");
begin
  assert c = x"3" severity failure;
end;
