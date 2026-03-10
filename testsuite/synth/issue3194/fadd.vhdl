library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity fadd is
  port (
    a, b : in  std_logic_vector(15 downto 0); -- float 16-bit (1.5.10)
    y    : out std_logic_vector(15 downto 0)  -- float 16-bit (1.5.10)
  );
end entity;

architecture rtl of fadd is
  use work.float16_pkg.all;
begin
  y <= to_slv(to_float(a) + to_float(b));
end architecture;
