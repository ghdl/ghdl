library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity repro is
  port (clk : in std_logic; cnt : in unsigned(3 downto 0); q : out std_logic);
end entity;

architecture rtl of repro is
  signal m : natural range 0 to 0;
begin
  m <= to_integer(cnt) mod 1;
  process (clk)
  begin
    if rising_edge(clk) then
      if m = 0 then
        q <= '1';
      else
        q <= '0';
      end if;
    end if;
  end process;
end architecture;


