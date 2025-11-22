library ieee;
use ieee.std_logic_1164.all;

entity file2 is 
  end entity;

architecture behavioral of file2 is 
  signal w_test_signal : std_logic_vector(14 downto 0);
begin 

  U0 : entity work.file1 
  port map(
    test_out(15 ) => open,
    test_out(14 downto 0) => w_test_signal
  );

end architecture;
