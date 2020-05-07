-- 3-D Ram Inference Example (Single port) 
-- Compile this file in VHDL2008 mode
-- File:rams_sp_3d.vhd

library ieee;
use ieee.std_logic_1164.all;
package mypack is
  type myarray_t is array(integer range<>) of std_logic_vector;
  type mem_t is array(integer range<>) of myarray_t;
end package;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.mypack.all;
entity rams_sp_3d is generic (
                       NUM_RAMS : integer := 2;
                       A_WID    : integer := 10;
                       D_WID    : integer := 32
                      );
                port  (
                       clk    : in std_logic;
                       we     : in std_logic_vector(NUM_RAMS-1 downto 0);
                       ena    : in std_logic_vector(NUM_RAMS-1 downto 0);
                       addr   : in myarray_t(NUM_RAMS-1 downto 0)(A_WID-1 downto 0);
                       din    : in myarray_t(NUM_RAMS-1 downto 0)(D_WID-1 downto 0);
                       dout   : out myarray_t(NUM_RAMS-1 downto 0)(D_WID-1 downto 0)
                      );
end rams_sp_3d;

architecture arch of rams_sp_3d is
signal mem : mem_t(NUM_RAMS-1 downto 0)(2**A_WID-1 downto 0)(D_WID-1 downto 0);
begin
process(clk)
begin
  if(clk'event and clk='1') then
    for i in 0 to NUM_RAMS-1 loop
      if(ena(i) = '1') then
        if(we(i) = '1') then
          mem(i)(to_integer(unsigned(addr(i)))) <= din(i);
        end if;
        dout(i) <= mem(i)(to_integer(unsigned(addr(i))));
      end if;
    end loop;
  end if;
end process;

end arch;
