library ieee;
use ieee.std_logic_1164.all;

entity generic_array_range is
  generic (
    SIZE : integer := 1
  );
  port (
    data_in  : in std_logic_vector(7 downto 0);
    data_out : out std_logic_vector(7 downto 0)
  );
end;

architecture behavioral of generic_array_range is
  --constant SIZE : integer := 1;
  type t_slv_array_1d is array (natural range <>) of std_logic_vector; 
  signal a_slv_tmp : t_slv_array_1d(0 to SIZE-1)(7 downto 0);
begin
  a_slv_tmp(0) <= data_in;
  data_out <= a_slv_tmp(0);
end behavioral;
