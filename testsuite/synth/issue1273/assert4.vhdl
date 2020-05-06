library ieee;
use ieee.std_logic_1164.all;

entity assert4 is
  port (v : std_logic_Vector (7 downto 0);
        en : std_logic;
        clk : std_logic;
        res : out std_logic);
end;

architecture behav of assert4 is
begin
  process (clk)
  begin
    if rising_edge(clk) and en = '1' then
      assert v /= x"00";
      res <= v(0) xor v(1);
    end if;
  end process;
end behav;

