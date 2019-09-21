library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity display is
  port (
    clk : in std_logic;
    rst_n : in std_logic;
    column : integer;
    row : integer;
    col2x : out unsigned(9 downto 0);
    row2x : out unsigned(9 downto 0);
    twochars : in std_logic_vector(15 downto 0)
  );
end display;

architecture behavior of display is
  signal disp_ena : std_logic;
  signal ucol : unsigned(9 downto 0);
  signal urow : unsigned(9 downto 0);
  signal tileaddress : std_logic_vector(10 downto 0);
  signal char : std_logic_vector(7 downto 0);
  signal char_row : std_logic_vector(7 downto 0);

begin

  -- 8x8 bitmap is upscaled 2x
  ucol <= to_unsigned(column, 10);
  urow <= to_unsigned(row, 10);
  col2x <= shift_right(urow, 1);
  row2x <= shift_right(ucol, 1);


end;
 
