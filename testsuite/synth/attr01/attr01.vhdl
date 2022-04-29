library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity attr01 is
  port (
    rst : std_logic;
    clk : std_logic;
    cnt : out std_logic_vector (7 downto 0)
    );
end attr01;

architecture behav of attr01 is
  signal counter : std_logic_vector (7 downto 0);

  attribute keep : boolean;
  attribute keep of counter : signal is True;
begin
  process (clk)
  begin
    if rising_edge (clk) then
      if rst = '1' then
        counter <= (others => '0');
      else
        counter <= std_logic_vector (unsigned (counter) + 1);
      end if;
    end if;
  end process;

  cnt <= counter;
end behav;
