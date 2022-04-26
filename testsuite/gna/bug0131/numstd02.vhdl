library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity numstd02 is
end;

architecture behav of numstd02 is
  constant va : unsigned (15 downto 0) := x"ab02";
  constant vb : unsigned (15 downto 0) := x"31_de";
  constant res : unsigned (15 downto 0) := minimum(va, vb);
  constant t : boolean := res = vb;
begin
  process
  begin
    case true is
      when t =>
        report "Test OK";
      when false =>
        assert false severity failure;
    end case;
    wait;
  end process;
end behav;
