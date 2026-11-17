library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity dynslice02 is
  port (s, w : in std_logic;
        q : out std_logic_vector(7 downto 0));
end entity;

architecture rtl of dynslice02 is
  type mem_t is array (0 to 1) of std_logic_vector(63 downto 0);
  signal mem : mem_t := (x"0123456789abcdef", x"fedcba9876543210");
  signal si, wi : integer range 0 to 1;
begin
  si <= 1 when s = '1' else 0;
  wi <= 1 when w = '1' else 0;
  --  Same as dynslice01, on the read side.
  q <= mem(si)(32*wi+15 downto 32*wi+8);
end architecture;
