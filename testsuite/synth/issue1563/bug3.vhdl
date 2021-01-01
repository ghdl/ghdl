library ieee;
use ieee.std_logic_1164.all;

entity bug3 is
port (
  clock : in std_logic;
  reset : std_logic;
  input : std_logic_vector(3 downto 0);
  output : out std_logic_vector(3 downto 0)
);
end;

architecture bug_arch OF bug3 is
begin
    process (clock)
    begin
      if reset = '1' then
        output(0) <= '0';
      elsif rising_edge(clock) then
        output <= input;
      end if;
    end process;
end bug_arch;
