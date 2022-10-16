library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
end entity test;
architecture beh of test is
  type t_msg_id_panel is array (natural range <>) of boolean;
  procedure enable_log_msg(
    variable msg_id_panel : t_msg_id_panel
  )is begin
  end procedure;

  procedure enable_log_msg(
    msg       : string
  )is begin
  end procedure;
begin
process(all)
begin
  enable_log_msg("Test test");
end process;
end architecture beh;
