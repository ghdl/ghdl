library ieee;
use ieee.std_logic_1164.all;

entity sub_cons is
  generic (L : natural);
  port (i : std_logic_vector(L-1 downto 0);
        o : out std_logic);
end sub_cons;

architecture behav of sub_cons is
begin
  o <= '1' when i = (L - 1 downto 0 => '1') else '0';
end behav;

library ieee;
use ieee.std_logic_1164.all;

entity sub_unsc is
  generic (L : natural);
  port (i : std_logic_vector;
        o : out std_logic);
end sub_unsc;

architecture behav of sub_unsc is
begin
  o <= '1' when i = (i'range => '1') else '0';
end behav;

library ieee;
use ieee.std_logic_1164.all;

entity repro is
  port (a, b, c: in std_logic;
        oc : out std_logic;
        ou : out std_logic);
end repro;

architecture behav of repro is
begin
  instc: entity work.sub_cons
    generic map (L => 3)
    port map (i(0) => a,
              i(1) => b,
              i(2) => c,
              o => oc);

  instu: entity work.sub_unsc
    generic map (L => 3)
    port map (i(0) => a,
              i(1) => b,
              i(2) => c,
              o => ou);
end behav;
