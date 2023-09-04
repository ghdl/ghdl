library ieee;
use ieee.std_logic_1164.all;

entity tri is
  port (a : std_logic_vector(7 downto 0);
        en : std_logic;
        o : out std_logic_vector(7 downto 0));
end;

architecture behav of tri is
begin
  o <= a when en = '1' else (others => 'Z');
end;

library ieee;
use ieee.std_logic_1164.all;

entity repro3 is
  port (a, b : std_logic_vector(7 downto 0);
        c : std_logic_vector(3 downto 0);
        ena, enb, enc : std_logic;
        o : out std_logic_vector(7 downto 0));
end;

architecture behav of repro3 is
begin
  inst1: entity work.tri
    port map (a, ena, o);
  inst2: entity work.tri
    port map (b, enb, o);

  process (c, enc)
  begin
    o <= (others => 'Z');
    if enc = '1' then
      o (3 downto 0) <= c;
    end if;
  end process;
end behav;
