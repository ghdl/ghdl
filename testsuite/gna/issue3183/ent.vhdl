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

  m_axis.valid <= '0';
  m_axis.data  <= (m_axis.data'range => '0');

  m_assert_proc : process
  begin
    wait until rst_i = '0';
    assert m_axis.ready = '1' report "axis_master_sim : Bad m_axis.ready";
  end process m_assert_proc;

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

  s_axis.ready <= '1';

  s_assert_proc : process
  begin
    wait until rst_i = '0';
    assert s_axis.valid = '0' report "axis_slave_sim : Bad s_axis.valid";
    assert or(s_axis.data) = '0' report "axis_slave_sim : Bad s_axis.data";
  end process s_assert_proc;

end architecture simulation;

-- ---------------------------------------------------------------------------------------

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std_unsigned.all;

library work;
  use work.axis_pkg.all;

entity axis_pause is
  port (
    clk_i  : in    std_logic;
    rst_i  : in    std_logic;
    s_axis : view  axis_slave_view;
    m_axis : view  axis_master_view
  );
end entity axis_pause;

architecture simulation of axis_pause is

begin

  s_axis.ready <= m_axis.ready;
  m_axis.valid <= s_axis.valid;
  m_axis.data  <= s_axis.data;

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
    data(7 downto 0)
  );

  signal rx_axis : axis_rec_type (
    data(7 downto 0)
  );

begin

  clk <= not clk after 5 ns;
  rst <= '1', '0' after 100 ns;

  axis_master_sim_inst : entity work.axis_master_sim
    port map (
      clk_i  => clk,
      rst_i  => rst,
      m_axis => tx_axis
    );

  axis_slave_sim_inst : entity work.axis_slave_sim
    port map (
      clk_i  => clk,
      rst_i  => rst,
      s_axis => rx_axis
    );

  axis_pause_inst : entity work.axis_pause
    port map (
      clk_i  => clk,
      rst_i  => rst,
      s_axis => tx_axis,
      m_axis => rx_axis
    );

end architecture simulation;
