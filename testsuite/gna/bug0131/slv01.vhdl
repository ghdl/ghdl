library ieee;
use ieee.std_logic_1164.all;

entity slv01 is
end;

architecture behav of slv01 is
  constant v : std_logic_vector (15 downto 0) := x"ab02";
  constant v_1 : std_logic_vector (15 downto 0) := v and x"cfff";
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
