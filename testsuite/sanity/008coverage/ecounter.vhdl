library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ecounter is
  port (clk : std_logic;
        rst : std_logic;
        lim : std_logic_vector (7 downto 0);
        zero : out std_logic;
        tick : out std_logic);
end ecounter;

architecture arch of ecounter is
  signal cnt : std_logic_vector(7 downto 0);
begin

  process (clk)
  begin
    if rising_edge(clk) then
      tick <= '0';
      
      if rst = '1' then
        cnt <= (others => '0');
      elsif cnt = lim then
        tick <= '1';
        cnt <= (others => '0');
      else
        cnt <= std_logic_vector(unsigned(cnt) + 1);
      end if;
    end if;
  end process;
  
end arch;
