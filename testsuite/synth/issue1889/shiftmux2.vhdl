library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity shiftMux2 is
  Port (
    clk : in  std_logic;
    --inputA
    inputA : in std_logic;
    outputB : out std_logic
  );
end shiftMux2;

architecture Rtl of shiftMux2 is
begin
  process (clk) is
  begin
    if rising_edge(clk) then
      outputB <= inputA;
    end if;
  end process;
end Rtl;
