library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity DelayLine is
  generic (
    Depth : natural := 2;
    Width : natural := 16
    );
  
  port(
    din  : in  signed(Width-1 downto 0);
    clk  : in  std_logic;
    ce   : in  std_logic;
    dout : out signed(Width-1 downto 0)
    );
end DelayLine;

architecture behavioral of DelayLine is
  type DelayType is array (0 to Depth-1) of signed(Width-1 downto 0);
  signal DataBuffer : DelayType;
begin
  dout <= DataBuffer(Depth-1);
  
  process (clk)
  begin
    if rising_edge (clk) then
      if (ce = '1') then
        DataBuffer(0) <= din;
        DataBuffer(1 to Depth-1) <= DataBuffer(0 to Depth-2); --XX
      end if;
    end if;
  end process;
end behavioral;
