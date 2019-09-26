library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package top_pack is

type top_reg_t is record
  prescale : integer range 0 to (2**24)-1;
  count    : integer range 0 to 3;
  blip     : std_logic;
  y        : std_logic_vector(1 to 5);
end record;

constant TOP_REG_RESET : top_reg_t := ( 0, 0, '0', (others => '0') );

function to_slv(C:integer; B:std_logic; E:std_logic) return std_logic_vector;

component top port (
   clk :  in std_logic;
   D   : out std_logic_vector(1 to 5));
end component;

end package;

package body top_pack is

function to_slv(C:integer; B:std_logic; E:std_logic) return std_logic_vector is
variable ret : std_logic_vector(1 to 5) := (others => '0');
begin
   ret(C+1) := E;
   ret(5)   := B;

   return ret;
end to_slv;

end top_pack;
