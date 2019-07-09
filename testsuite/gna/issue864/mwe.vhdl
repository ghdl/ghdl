library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (
    a_in : IN std_logic;
    a_out : OUT std_logic
  );
end entity a;

library ieee;
use ieee.std_logic_1164.all;

entity b is
  port (
    b_in : IN std_logic;
    b_out : OUT std_logic
  );
end entity b;

architecture rtl of a is
begin
  process (a_in)
  begin
    a_out <= a_in;
  end process;
end architecture rtl;

architecture rtl of b is 
  component a
    port (
      a_in : IN std_logic;
      a_out : OUT std_logic
    );
  end component;

  for a0 : a;
begin
end architecture rtl;
