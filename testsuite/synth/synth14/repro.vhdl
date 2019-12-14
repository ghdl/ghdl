library ieee;
use ieee.std_logic_1164.all;

entity repro is
  port (clk : std_logic;
        rst : std_logic;
        o : out std_logic);
end repro;

architecture behav of repro is
  signal v : natural range 0 to 3;
begin
  process (clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then
        v <= 0;
      else
        v <= v + 1;
      end if;
    end if;
  end process;

  o <= '1' when v = 0 else '0';
end behav;
