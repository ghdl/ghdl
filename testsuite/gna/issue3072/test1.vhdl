library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--hdlregression:tb
entity test is
end test;

architecture beh of test is
  signal sig : std_logic_vector(11 downto 0) := (others => '0');
begin
  process
  begin
    case to_integer(unsigned(sig)) is
      when to_integer(unsigned(std_logic_vector'("000000000000"))) =>
        report "end-test";
      when others =>
        report "Incorrect choice" severity failure;
    end case;
    wait;
  end process;
end architecture;
