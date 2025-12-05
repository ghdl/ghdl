library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity attr03 is
  generic (init : natural := 5);
  port (
    rst : std_logic;
    clk : std_logic;
    cnt : out std_logic_vector (7 downto 0)
    );
  attribute my_attr : Natural;
  attribute my_attr of attr03 : entity is init - 1;
end;

architecture behav of attr03 is
  signal counter : natural;

begin
  process (clk)
  begin
    if rising_edge (clk) then
      if rst = '1' then
        counter <= attr03'my_attr;
      else
        counter <= counter + 1;
      end if;
    end if;
  end process;

  cnt <= std_logic_vector (to_unsigned (counter, 8));
end behav;
