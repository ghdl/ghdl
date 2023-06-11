library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity slice_bug is
  generic(
    INPUT_WIDTH : natural := 64;
    OUTPUT_WIDTH: natural := 32;
    MAX_DIV : natural := 16
  );
  port (
    clock_i : in std_logic;
    divindex  : in std_logic_vector(31 downto 0);
    indata : in std_logic_vector(INPUT_WIDTH-1 downto 0);
    outdata : out std_logic_vector(OUTPUT_WIDTH-1 downto 0)
  );
end entity slice_bug;

architecture rtl of slice_bug is

  signal div_index : natural range 0 to MAX_DIV; 

  signal indata_tready_s : std_logic;

  signal output_re_s : std_logic_vector((OUTPUT_WIDTH/2)-1 downto 0);
  signal output_im_s : std_logic_vector((OUTPUT_WIDTH/2)-1 downto 0);

  signal output_tmp : std_logic_vector((INPUT_WIDTH/2)-1 downto 0);

begin

  div_index <= to_integer(unsigned(divindex));

  output_re_s <= indata((OUTPUT_WIDTH/2)+div_index-1 downto div_index);

--  output_tmp <= indata(INPUT_WIDTH-1 downto OUTPUT_WIDTH);
--  output_im_s <= output_tmp((OUTPUT_WIDTH/2)+div_index-1 downto div_index);
  output_im_s <= indata((OUTPUT_WIDTH/2)+(INPUT_WIDTH/2) + div_index-1 downto
                 div_index + (INPUT_WIDTH/2));

  outdata <= output_im_s & output_re_s;

end architecture rtl;
