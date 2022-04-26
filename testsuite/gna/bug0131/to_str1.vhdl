library ieee;
use ieee.std_logic_1164.all;

entity to_str1 is
end;

architecture behav of to_str1 is
  constant v : std_logic_vector (15 downto 0) := x"ab02";
  constant v_1 : std_logic_vector (15 downto 0) := v and x"cfff";
  constant v1_str : string (1 to 4) := to_hstring (v_1);
  constant t : boolean := v1_str = "8b02";
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
