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

type axis_rec_array is array (natural range <>) of axis_rec_type;


alias axis_slave_view is axis_master_view'converse;

end package axis_pkg;

package body axis_pkg is

end package body;


-----------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.axis_pkg.all;

entity axis_pipe_lite is
  port (
    clk_i   : in std_logic;
    rst_i   : in std_logic;
    s_axis  :    view axis_slave_view;
    m_axis  :    view axis_master_view of axis_rec_type;  -- this produces the error
    mm_axis :    view (axis_master_view) of axis_rec_array -- this is the reason why I found this bug
    );
end entity axis_pipe_lite;

architecture synthesis of axis_pipe_lite is
begin
end architecture synthesis;
