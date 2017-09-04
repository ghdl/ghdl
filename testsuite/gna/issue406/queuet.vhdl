library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

library libvhdl;


entity QueueT is
end entity QueueT;


architecture sim of QueueT is

  package SlvQueue is new libvhdl.QueueP
    generic map (QUEUE_TYPE => std_logic_vector(63 downto 0),
                 to_string  => to_string);

--  shared variable sv_simple_queue : SlvQueue.t_simple_queue;

begin

end architecture sim;
