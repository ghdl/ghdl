library ieee;
use ieee.std_logic_1164.all;

entity SubEnt is
  port (a, b: in std_logic;
        O: out std_logic);
end;

architecture behav of SubEnt is
begin
  O <= a or b;
end;

library ieee;
use ieee.std_logic_1164.all;

entity SubAl is
  port (a, b: in std_logic_vector(3 downto 0));
end;

architecture behav of SubAl is
begin
end;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Tb is
end;

architecture ar of Tb is
  signal SigA, SigB, SigO : std_logic;
  signal word : std_logic_vector(7 downto 0);
  signal val : natural;
begin
  DUT: entity work.SubEnt
    port map (SigA, SigB, SigO);

  SubAl: entity work.SubAl
    port map (word(7 downto 4), word(3 downto 0));

  process
  begin
    word <= b"0000_0000";

    for va in std_logic loop
      SigA <= va;
      for vb in std_logic loop
        SigB <= vb;

        word <= std_logic_vector(to_unsigned(val, 8));
        val <= val + 3;

        wait for 1 ns;
      end loop;
    end loop;
    wait;
  end process;
end ar;
