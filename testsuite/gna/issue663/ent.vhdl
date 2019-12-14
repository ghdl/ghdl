library ieee;
use ieee.std_logic_1164.all;

entity ent is
end entity ent;

architecture a of ent is
  signal clk : std_logic := '0';

  signal check_stable_in_1 : std_logic_vector(1 to 5) := "00000";
  alias check_stable_start_event_1  : std_logic is check_stable_in_1(1);
  alias check_stable_end_event_1    : std_logic is check_stable_in_1(2);
  alias check_stable_expr_1         : std_logic_vector(2 downto 0) is check_stable_in_1(3 to 5);

  signal check_stable_en_1 : std_logic;

  signal en, start_event, end_event, expr : std_logic := '1';

  procedure check_stable(
    signal clock               : in std_logic;
    signal en                  : in std_logic;
    signal start_event         : in std_logic;
    signal end_event           : in std_logic;
    signal expr                : in std_logic_vector) is
  begin
    wait until (falling_edge(clock) or rising_edge(clock)) and (to_x01(en) = '1');
  end;

begin
  clock : process is
  begin
    clk <= '1', '0' after 5 ns;
    wait;
  end process clock;

  check_stable_1 : check_stable(clk,
                                check_stable_en_1,
                                check_stable_start_event_1,
                                check_stable_end_event_1,
                                check_stable_expr_1);

end architecture;
