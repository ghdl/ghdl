library ieee;
use ieee.std_logic_1164.all;

entity delay_vector is

  generic (
    DELAY : integer := 32
  );
  port (
    clk : in std_logic;
    clken : in std_logic := '1';
    d : in std_logic_vector;
    q : out std_logic_vector);

end delay_vector;

architecture archi of delay_vector is

begin

  gen_bits : for i in d'low to d'high generate
    db : entity work.delay_bit
      generic map
      (
        DELAY => DELAY
      )
      port map
      (
        clk => clk,
        clken => clken,
        d => d(i),
        q => q(i)
      );
  end generate;

  end;