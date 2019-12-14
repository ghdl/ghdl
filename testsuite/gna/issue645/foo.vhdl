library ieee;
use ieee.std_logic_1164.all;

package types is
  type t is array (natural range <>) of std_logic_vector; 
  -- Warning : only compiles with --std=08
end package;

library ieee;
use ieee.std_logic_1164.all;
use work.types.all;

entity foo is
  generic (n: natural; p: natural);
  port (clk: in std_logic;
        din: in std_logic_vector(p-1 downto 0);
        dout: out t);
end entity;

architecture rtl of foo is
  signal REG: t(0 to n-1)(p-1 downto 0); -- !!! CRASH HERE 
begin
  process (clk)
  begin
    if clk'event and clk='1' then
      for i in 1 to n-1 loop
        reg(i) <= reg(i-1);
      end loop;			
      reg(0) <= din;	    
    end if;
  end process;
  dout <= reg;
end architecture;
