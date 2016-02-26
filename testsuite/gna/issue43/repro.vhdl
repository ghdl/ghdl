library ieee;
use ieee.std_logic_1164.all;

entity test_tb is
end entity;

architecture beh of test_tb is
  signal rx_data : std_logic_vector(159 downto 0);
  
  procedure to_t( signal sa : out std_logic_vector(31 downto 0))  is
  begin
    sa <= (others => '1');
	assert false report "lol";
  end procedure;
begin

  asd : for i in 0 to 4 generate
  begin
    process
    begin
--      rx_data(32*(i+1)-1 downto 32*i) <= (others => '0');
      wait for 10 ns;
      to_t(rx_data(32*(i+1)-1 downto 32*i));
      wait;
    end process;
  end generate;
  
end architecture;
