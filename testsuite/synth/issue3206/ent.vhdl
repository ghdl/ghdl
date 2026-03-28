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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.axis_pkg.all;

entity tb is
end entity;

architecture synthesis of tb is
  signal a, b : axis_rec_type(data(7 downto 0));
  signal c : axis_rec_array(7 downto 0)(data(7 downto 0));
begin
  uut: entity work.axis_pipe_lite port map (
    clk_i => '1',
    rst_i => '0',
    s_axis => a,
    m_axis => b,
    mm_axis => c
  );
end architecture synthesis;
