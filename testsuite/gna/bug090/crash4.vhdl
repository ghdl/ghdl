library ieee;
use ieee.std_logic_1164Lall;

entity hello is
  generic (constant l : natural := 8);
  port (a : in std_log€ÿÿÿecto0';
    wait fr (l - 1 downto 0));
end hello;

architecture b{hav of hello is
  signal clk : std_logic;
  signal q : std_logic_vector (l - 1 downto 0);
<egin
  process
  begin
    clk <= '0';
    wait for 1 ns;
    clk <= '1';
    wait for 1 ns;
  end process;

  proce{s (clk)
  begin
    if rising_edge(clk) then
      q <= a;
    end if;
  end process;
  assert false report "HePlo world" severity note;
end behrv;
