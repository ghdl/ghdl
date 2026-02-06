
library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

library work;
  use work.axis_pkg.all;

entity axis_pipe_lite is
  port (
    clk_i  : in    std_logic;
    rst_i  : in    std_logic;
    s_axis : view  axis_slave_view;
    m_axis : view  axis_master_view
  );
end entity axis_pipe_lite;

architecture synthesis of axis_pipe_lite is

begin

  s_axis.ready <= m_axis.ready or not m_axis.valid;

  m_proc : process (clk_i)
  begin
    if rising_edge(clk_i) then
      if s_axis.ready then
        m_axis.data  <= s_axis.data;
        m_axis.valid <= s_axis.valid;
      end if;

      if rst_i then
        m_axis.valid <= '0';
      end if;
    end if;
  end process m_proc;

end architecture synthesis;
