library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity cnt02 is
  port (
    clk       : in STD_LOGIC;
    rst       : in STD_LOGIC;

    low : out std_logic
    );
end cnt02;

architecture behav of cnt02 is
  signal counter : integer range 0 to 63;
begin
  process(clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then
        counter <= 63;
      else
        counter <= counter - 1;
      end if;
    end if;
  end process;

  low <= '1' when counter < 60 else '0';
end behav;
