library ieee;
use ieee.std_logic_1164.all;

entity inner is
  port (
  clk : in std_logic;
 inner_counter : in std_logic_vector(6 downto 0));
end inner;

architecture default of inner is
begin
  assert now < 1 ns or inner_counter (6) /= 'U' severity error;
  assert inner_counter (6) /= 'U' severity error;
  --do something
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test_bench is
end test_bench;

architecture default of test_bench is
  signal clk : std_logic := '0';
  signal counter : unsigned(7 downto 0) := (others => '0');

begin

  i0: entity work.inner port map (
  clk => clk,
  inner_counter => std_logic_vector(counter(6 downto 0)));

  process
  begin
    clk <= '1';
    wait for 1 ns;
    clk <= '0';
    wait for 1 ns;
  end process;

  process(clk)
  begin
    if rising_edge(clk) then
      counter <= counter + 1;
    end if;
  end process;

end default;
