library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.ALL;

entity cnt04 is
  port (
    clk       : in std_logic;
    rst       : in std_logic;

    counter : out std_logic_vector (7 downto 0)
    );
end cnt04;

architecture behav of cnt04 is
  signal s_count : unsigned(7 downto 0); -- := (others => '0');
begin
  process(clk, rst)
  begin
    if rst = '1' then
      s_count <= (others => '0');
    elsif rising_edge(clk) then
      s_count <= s_count + 1;
    end if;
  end process;

  -- connect internal signal to output
  counter <= std_logic_vector(s_count + 1);
end behav;
