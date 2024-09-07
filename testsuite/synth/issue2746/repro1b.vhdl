library ieee;
use ieee.std_logic_1164.all;

entity repro1 is
  port (
    clk : std_logic;
    din : std_logic_vector(7 downto 0);
    dout : out std_logic_vector(7 downto 0);
    d2 : std_logic);
end;

architecture behav of repro1 is
begin
  process (clk, d2)
  begin
    if rising_edge(clk) then
      dout <= din;
    end if;

    dout(0) <= d2;
    dout(6) <= '0';
  end process;
end;

      
    
