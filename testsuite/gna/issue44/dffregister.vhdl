-- dffregister.vhd
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity dffregister is
  generic (word_size: integer);
	port(clk, reset: in std_logic;
  d:in signed(word_size-1 downto 0);
  reset_word: in signed(word_size-1 downto 0);
  q:out signed(word_size-1 downto 0));
end dffregister;

architecture dffregister_arch of dffregister is
  signal arr:signed(word_size -1 downto 0);

begin
  q <= arr;
  process(reset, clk)
  begin
    if reset = '1' then
      arr <= reset_word;
  elsif rising_edge(clk) then
      arr <= d;
    end if;

  end process;

end dffregister_arch;
