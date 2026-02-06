library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

package axis_pkg is

type axis_rec_type is record
  ready : std_logic;
  valid : std_logic;
  data  : std_logic_vector;
end record axis_rec_type;

view axis_master_view of axis_rec_type is
  ready : in;
  valid : out;
  data  : out;
end view axis_master_view;

alias axis_slave_view is axis_master_view'converse;

end package axis_pkg;

package body axis_pkg is

end package body;

