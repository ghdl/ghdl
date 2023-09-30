library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity repro2 is
  port (n : natural;
        en : std_logic;
        d1 : std_logic_vector(31 downto 0);
        q : out std_logic_vector(31 downto 0));
end;

architecture behav of repro2 is
begin
  process (n, en)
  begin
    if en = '1' then
      q <= d1 or std_logic_vector(to_unsigned(2 ** n, 32));
    end if;

    if en = '0' then
      q <= (others => '0');
    end if;
  end process;
end;
