library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library work;
use     work.ax_wb_pli_pkg.all;

entity ax_wb_pli is
end entity ax_wb_pli;

architecture behav of ax_wb_pli is

  constant cQUEUE_ID : integer := 0;
  shared variable queue_handle : integer;

begin


  init_bus_handle : process 
  begin
    queue_handle := init_queue(cQUEUE_ID);
    if (queue_handle = 0) then
      assert false report "Failed to register Message Queue";
    end if;  
    wait for 1 us;
    delete_queue(cQUEUE_ID);
  end process;


end behav;
