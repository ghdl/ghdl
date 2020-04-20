library ieee;
use ieee.std_logic_1164.all;
entity theunit is
  -- NOTE: w := 2 prevents bug
  generic (w    : natural := 1);
  port    (dout : out std_ulogic);
end;

architecture rtl of theunit is
  type selsel_t is array (0 to 1) of natural range 0 to w-1;
  signal selsel : selsel_t := (others => 0);
begin
  -- NOTE: selsel(0) prevents bug
  selsel(1) <= 0;
  dout <= '0';
end;
