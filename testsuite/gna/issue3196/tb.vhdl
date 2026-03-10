library ieee;
use ieee.std_logic_1164.all;

entity tb is end;
architecture sim of tb is
  attribute syn_srlstyle : string;
  signal clk : std_logic := '0';
begin
  clk <= not clk after 5 ns;

  p_test: process (clk) is
    variable v : std_logic := '0';
    attribute syn_srlstyle of v : variable is "registers";
  begin
    if rising_edge(clk) then
      v := not v;
    end if;
  end process;

  process begin
    wait for 200 ns;
    report "PASS";
    std.env.stop(0);
  end process;
end;
