library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

package ax_wb_pli_pkg is

  type bus_t is access integer;

  function init_queue(queue_id : integer) return integer;
    attribute foreign of init_queue : function is "VHPIDIRECT sim_init_queue";

  procedure delete_queue(queue_id : integer);
    attribute foreign of delete_queue : procedure is "VHPIDIRECT sim_delete_queue";

end package;

package body ax_wb_pli_pkg is

  function init_queue(queue_id : integer) return integer is
  begin
    assert false report "VHPI" severity failure;
  end function;

  procedure delete_queue(queue_id : integer) is
  begin
    assert false report "VHPI" severity failure;
  end procedure;

end package body;
