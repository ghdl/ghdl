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

end architecture simulation;

-- ---------------------------------------------------------------------------------------

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std_unsigned.all;

library work;
  use work.axis_pkg.all;

entity axis_slave_sim is
  port (
    clk_i  : in    std_logic;
    rst_i  : in    std_logic;
    s_axis : view  axis_slave_view
  );
end entity axis_slave_sim;

architecture simulation of axis_slave_sim is

begin

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
    m_axis : view  axis_master_view;
    s_axis : view  axis_slave_view
  );
end entity axis_sim;

architecture simulation of axis_sim is

begin

  axis_master_sim_inst : entity work.axis_master_sim
    port map (
      clk_i  => clk_i,
      rst_i  => rst_i,
      m_axis => m_axis
    );

  axis_slave_sim_inst : entity work.axis_slave_sim
    port map (
      clk_i  => clk_i,
      rst_i  => rst_i,
      s_axis => s_axis
    );

end architecture simulation;

-- ---------------------------------------------------------------------------------------

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

library work;
  use work.axis_pkg.all;

entity tb_axis_sim is
end entity tb_axis_sim;

architecture simulation of tb_axis_sim is

  signal clk : std_logic := '1';
  signal rst : std_logic := '1';

  signal tx_axis : axis_rec_type (
    data(23 downto 0)
  );

begin

  clk <= not clk after 5 ns;
  rst <= '1', '0' after 100 ns;

  axis_sim_inst : entity work.axis_sim
    port map (
      clk_i  => clk,
      rst_i  => rst,
      m_axis => tx_axis,
      s_axis => tx_axis
    );

end architecture simulation;
