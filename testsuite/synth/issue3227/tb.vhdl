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
    ready : out;
  valid : in;
  data  : in;
end view axis_master_view;

type axis_rec_array is array (natural range <>) of axis_rec_type;


alias axis_slave_view is axis_master_view'converse;

end package axis_pkg;

package body axis_pkg is

end package body;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.axis_pkg.all;

entity rec2array is
  port (
    m  :  view axis_master_view;
    s : view (axis_slave_view) of axis_rec_array
    );
end entity;

architecture synthesis of rec2array is
begin
	m.ready <= s(0).ready;
    s(0).valid <= m.valid;
    s(0).data <= m.data;
end architecture synthesis;



library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.axis_pkg.all;

entity inner0 is
  port (
    m  :  view axis_master_view
    );
end entity;

architecture synthesis of inner0 is
  signal c : axis_rec_array(0 to 1)(data(7 downto 0));
begin
  reg2array_inst : entity work.rec2array port map (
    m => m,
    s => c
  );
end architecture synthesis;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.axis_pkg.all;

entity tb is
end entity;

architecture synthesis of tb is
  signal a : axis_rec_type(data(7 downto 0));
  signal c : axis_rec_array(0 to 1)(data(7 downto 0));
begin
  reg2array_inst : entity work.rec2array port map (
    m => a,
    s => c
  );
  inner0_inst: entity work.inner0 port map (
      m => c(0)
  );
  inner1_inst: entity work.inner0 port map (
      m => c(1)
  );
end architecture synthesis;
