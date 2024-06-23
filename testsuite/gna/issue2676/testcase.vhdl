package A_pkg is
  type f is (ONE, TWO);
end package;

library ieee;
use ieee.std_logic_1164.all;
use work.A_pkg.all;

entity A is
  port (
    clk : in std_logic
  );
end A;

architecture rtl of A is
  signal e : f;
begin
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity B is
end B;

architecture sim of B is
  signal clk : std_logic := '1';
begin
  DUT : entity work.A(rtl)
  port map (
    clk => clk
  );
end architecture;

use work.A_pkg.all;

entity C is
end C;

architecture sim of C is
begin
  E_PROC : process
    alias e is <<signal .D.B.DUT.e : f>>;
  begin
      wait on e;
  end process;
end architecture;

entity D is
end D;

architecture sim of D is
begin
  B : entity work.B(sim);

  process
  begin
    wait;
  end process;
end architecture;