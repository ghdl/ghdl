library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.axis_pkg.all;

entity repro1_rec2array is
  port (
    m  :  view axis_master_view;
    s : view (axis_slave_view) of axis_rec_array(0 to 1)
    );
end entity;

architecture synthesis of repro1_rec2array is
begin
  m.ready <= s(0).ready;
  s(0).valid <= m.valid;
  s(0).data <= m.data;
  s(1).valid <= '1';
end architecture synthesis;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.axis_pkg.all;

entity repro1 is
  port (res : out std_logic);
end entity;

architecture synthesis of repro1 is
  signal a : axis_rec_type(data(7 downto 0));
  signal c : axis_rec_array(0 to 1)(data(7 downto 0));
begin
  inst : entity work.repro1_rec2array port map (
    m => a,
    s => c
  );

  c(0).ready <= '0';
  c(1).ready <= '1';
  a.valid <= '0';
  res <= a.ready or c(0).valid;
end architecture synthesis;
