-- 3-D Ram Inference Example ( Simple Dual port) 
-- Compile this file in VHDL2008 mode
-- File:rams_sdp_3d.vhd

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
entity rams_sdp_3d is generic (
                       NUM_RAMS : integer := 2;
                       A_WID    : integer := 10;
                       D_WID    : integer := 32
                      );
                port  (
                       clka    : in std_logic;
                       clkb    : in std_logic;
                       wea     : in std_logic_vector(NUM_RAMS-1 downto 0);
                       ena     : in std_logic_vector(NUM_RAMS-1 downto 0);
                       enb     : in std_logic_vector(NUM_RAMS-1 downto 0);
                       addra   : in myarray_t(NUM_RAMS-1 downto 0)(A_WID-1 downto 0);
                       addrb   : in myarray_t(NUM_RAMS-1 downto 0)(A_WID-1 downto 0);
                       dina    : in myarray_t(NUM_RAMS-1 downto 0)(D_WID-1 downto 0);
                       doutb   : out myarray_t(NUM_RAMS-1 downto 0)(D_WID-1 downto 0)
                      );
end rams_sdp_3d;

architecture arch of rams_sdp_3d is
signal mem : mem_t(NUM_RAMS-1 downto 0)(2**A_WID-1 downto 0)(D_WID-1 downto 0);
begin
process(clka)
begin
  if(clka'event and clka='1') then
    for i in 0 to NUM_RAMS-1 loop
      if(ena(i) = '1') then
        if(wea(i) = '1') then
          mem(i)(to_integer(unsigned(addra(i)))) <= dina(i);
        end if;
      end if;
    end loop;
  end if;
end process;

process(clkb)
begin
  if(clkb'event and clkb='1') then
    for i in 0 to NUM_RAMS-1 loop
      if(enb(i) = '1') then
        doutb(i) <= mem(i)(to_integer(unsigned(addrb(i))));
      end if;
    end loop;
  end if;
end process;

end arch;
