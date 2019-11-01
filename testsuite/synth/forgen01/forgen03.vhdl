library ieee;
use ieee.std_logic_1164.all;

entity fulladder is
  port (a, b, ci : std_logic;
        o, co : out std_logic);
end fulladder;

architecture behav of fulladder is
begin
  o <= a xor b xor ci;
  co <= (a and b) or (a and ci) or (b and ci);
end behav;

library ieee;
use ieee.std_logic_1164.all;

entity forgen03 is
  generic (l : natural := 8;
           structural : boolean := true);
  port (a, b : std_logic_vector (l - 1 downto 0);
        o : out std_logic_vector (l - 1 downto 0));
end forgen03;

architecture behav of forgen03
is
begin
  gstr: if structural generate
    signal carry : std_logic_vector (l downto 0);
  begin
    carry (0) <= '0';
    gadd: for i in 0 to l - 1 generate
      iadd: entity work.fulladder
        port map (a => a (i), b => b (i), ci => carry (i),
                  o => o (i), co => carry (i + 1));
    end generate;
  end generate;
end behav;
