library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity numstd01 is
end;

architecture behav of numstd01 is
  constant v : unsigned (15 downto 0) := x"ab02";
  constant v_1 : unsigned (15 downto 0) := v and x"cfff";
  constant t : boolean := v_1 = x"8b02";
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
