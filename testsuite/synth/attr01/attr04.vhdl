library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity attr04 is
  generic (init : natural := 5);
  port (
    rst : std_logic;
    clk : std_logic;
    cnt : out std_logic_vector (7 downto 0)
    );
end;

architecture behav of attr04 is
  signal counter : natural;
  attribute my_attr : std_logic_vector;
  attribute my_attr of counter : signal is "01X";

begin
  process (clk)
  begin
    if rising_edge (clk) then
      if rst = '1' then
        counter <= 0;
      else
        counter <= counter + 1;
      end if;
    end if;
  end process;

  cnt <= std_logic_vector (to_unsigned (counter, 8));
end behav;
