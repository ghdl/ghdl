library ieee;
use ieee.std_logic_1164.all;

entity test3 is
  port (led: out std_logic_vector (7 downto 0);
        rst : std_logic;
        clk : std_logic);
end test3;

architecture synth of test3 is
  signal int : std_logic_vector(1 downto 0);
begin
--  led(7) <= '0';
--  led(6) <= '1';
--  led(5) <= '0';
--  led(3 downto 0) <= x"9";
  process (clk) is
  begin
    if rising_edge (clk) then
      if rst = '1' then
        int(1) <= '0';
      else
        int(1) <= not int(1);
      end if;
    end if;
  end process;
  led(5) <= int (1);
end synth;
