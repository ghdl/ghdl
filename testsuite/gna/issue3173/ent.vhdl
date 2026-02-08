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

-- ---------------------------------------------------------------------------------------

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std_unsigned.all;

library work;
  use work.axis_pkg.all;

entity axis_master_sim is
  port (
    clk_i  : in    std_logic;
    rst_i  : in    std_logic;
    m_axis : view  axis_master_view
  );
end entity axis_master_sim;

architecture simulation of axis_master_sim is

begin

  m_axis.data  <= (m_axis.data'range => '0');
  m_axis.valid <= m_axis.ready;

end architecture simulation;

-- ---------------------------------------------------------------------------------------

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

library work;
  use work.axis_pkg.all;

entity axis_sim is
  port (
    clk_i  : in    std_logic;
    rst_i  : in    std_logic;
    m_axis : view  axis_master_view
  );
end entity axis_sim;

architecture simulation of axis_sim is

begin

  axis_master_sim_inst : entity work.axis_master_sim
    port map (
      clk_i  => clk_i,
      rst_i  => rst_i,
      m_axis => m_axis    -- this is the offending line
    ); -- axis_master_sim_inst : entity work.axis_master_sim

end architecture simulation;
