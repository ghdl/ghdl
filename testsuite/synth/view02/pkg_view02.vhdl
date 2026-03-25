library ieee;
use ieee.std_logic_1164.all;

package view02_pkg is
  type inner_t is record
    a : std_logic;
    b : std_logic;
  end record;

  type outer_t is record
    ctrl : std_logic;
    data : inner_t;
  end record;

  view outer_master_view of outer_t is
    ctrl : out;
    data : out;
  end view outer_master_view;

  alias outer_slave_view is outer_master_view'converse;
end package;
