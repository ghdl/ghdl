library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
end entity test;
architecture beh of test is
  type t_msg_id is (ONE, TWO);
  type t_msg_id_panel is array (t_msg_id'left to t_msg_id'right) of boolean;
  type t_quietness is (NON_QUIET, QUIET);
  procedure enable_log_msg(
    constant msg_id       : t_msg_id;
    variable msg_id_panel : inout t_msg_id_panel;
    constant msg          : string      := "";
    constant scope        : string      := "";
    quietness : t_quietness := NON_QUIET
  )is begin
  end procedure;

  procedure enable_log_msg(
    msg_id    : t_msg_id;
    msg       : string;
    quietness : t_quietness := NON_QUIET;
    scope     : string      := ""
  )is begin
  end procedure;
begin
process(all)
begin
  enable_log_msg(ONE, "Test test");
end process;
end architecture beh;
