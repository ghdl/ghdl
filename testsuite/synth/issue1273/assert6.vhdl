library ieee;
use ieee.std_logic_1164.all;

entity assert6 is
  port (v : std_logic_Vector (7 downto 0);
        en : std_logic;
        clk : std_logic;
        rst : std_logic;
        res : out std_logic);
end;

architecture behav of assert6 is
begin
  process (clk, rst)
  begin
    if rst = '1' then
      res <= '0';
    elsif rising_edge(clk) and en = '1' then
      assert v /= x"05";
      res <= v(0) xor v(1);
    else
      assert v = x"00";
    end if;
  end process;
end behav;

