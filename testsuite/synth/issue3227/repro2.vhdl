library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.axis_pkg.all;

entity repro2_rec2array is
  port (
    m  :  view axis_master_view;
    s : view (axis_slave_view) of axis_rec_array(1 downto 0)
    );
end entity;

architecture synthesis of repro2_rec2array is
begin
  m.ready <= s(0).ready;
  s(0).valid <= m.valid;
  s(0).data <= m.data;
end architecture synthesis;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.axis_pkg.all;

entity repro2 is
  port (res : out std_logic);
end entity;

architecture synthesis of repro2 is
  signal a : axis_rec_type(data(7 downto 0));
  signal c : axis_rec_array(1 downto 0)(data(7 downto 0));
begin
  inst : entity work.repro2_rec2array port map (
    m => a,
    s => c
  );
  c(0).ready <= '0';
  c(1).ready <= '1';
  res <= a.ready;
end architecture synthesis;
