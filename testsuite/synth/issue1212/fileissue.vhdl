library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use STD.TEXTIO.all;

entity fileissue is
 generic(
  data_width : integer := 4
 );
 port(
  clk : in  std_logic;
  di  : in  std_logic_vector(data_width - 1 downto 0);
  do  : out std_logic_vector(data_width - 1 downto 0)
 );
end fileissue;

architecture behavioral of fileissue is
 file results : text;
begin
 process(clk)
  variable txtline     : line;
  variable file_status : file_open_status;
 begin
  file_open(file_status, results, "explicit.dat", write_mode);
  write(txtline, string'("--------------------"));
  writeline(results, txtline);
 end process;

end behavioral;

