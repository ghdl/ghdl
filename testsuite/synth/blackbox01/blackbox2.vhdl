library ieee;
use ieee.std_logic_1164.all;

entity blackbox2 is
  port (a, b : in std_logic_vector(7 downto 0);
        r : out std_logic_vector(7 downto 0));
end blackbox2;

architecture behav of blackbox2 is
  component blackbox2_adder is
    generic (iwidth : natural := 8;
             owidth : natural := 8);
    port (a, b : in std_logic_vector(iwidth - 1 downto 0);
          r : out std_logic_vector(owidth - 1 downto 0));
  end component;
  signal t : std_logic_vector(7 downto 0);
begin
  adder: blackbox2_adder
    generic map (iwidth => 8)
    port map (a, b, t);

  r <= t and x"ee";
end behav;
