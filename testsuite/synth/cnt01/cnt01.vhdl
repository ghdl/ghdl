library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity cnt01 is
  port (
    clock       : in STD_LOGIC;
    reset       : in STD_LOGIC;

    clear_count : in STD_LOGIC;
    enable      : in STD_LOGIC;

    counter_out : out STD_LOGIC_VECTOR (9 downto 0)
    );
end cnt01;

architecture behav of cnt01 is
  signal s_count : unsigned(9 downto 0); -- := (others => '0');
begin
  process(clock, reset)
  begin
    if reset = '1' then
      s_count <= (others => '0');
    elsif rising_edge(clock) then
      if clear_count = '1' then
        s_count <= (others => '0');
      elsif enable = '1' then
        s_count <= s_count + 1;
      end if;
    end if;
  end process;

  -- connect internal signal to output
  counter_out <= std_logic_vector(s_count);
end behav;
