library ieee;
use ieee.std_logic_1164.all;

entity blackbox1 is
  port (a, b : in std_logic_vector(7 downto 0);
        r : out std_logic_vector(7 downto 0));
end blackbox1;

architecture behav of blackbox1 is
  component blackbox1_adder is
    port (a, b : in std_logic_vector(7 downto 0);
          r : out std_logic_vector(7 downto 0));
  end component;
  signal t : std_logic_vector(7 downto 0);
begin
  adder: blackbox1_adder
    port map (a, b, t);

  r <= t and x"ee";
end behav;
