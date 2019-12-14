library ieee;
use ieee.std_logic_1164.all;

entity repro is
end entity repro;

architecture a of repro is
  signal clk : std_logic := '0';

  signal check_stable_in_1 : std_logic_vector(1 to 5) := "00000";
  alias check_stable_expr_1         : std_logic_vector(2 downto 0) is check_stable_in_1(3 to 5);

  procedure check_stable(
    signal clock               : in std_logic;
    signal expr                : in std_logic_vector) is
  begin
    wait until rising_edge(clock);
  end;

begin
  clock : process is
  begin
    clk <= '1', '0' after 5 ns;
    wait;
  end process clock;

--  process
--  begin
--    check_stable(clk, check_stable_expr_1);
--    wait on clk, check_stable_expr_1;
--    assert check_stable_expr_1 = "000";
--  end process;
  check_stable_1 : check_stable(clk, check_stable_expr_1);

end architecture;
