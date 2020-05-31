library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity repro is
end repro;

architecture behav of repro is
  signal s : unsigned (3 downto 0);
  signal n : integer;
begin
  process
  begin
    for i in 1 to 5 loop
      n <= to_integer(s);
      s <= s + 1;
      wait for 1 ns;
    end loop;
    wait;
  end process;
end behav;
