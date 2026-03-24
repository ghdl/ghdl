library ieee;
use ieee.std_logic_1164.all;
use work.view02_pkg.all;

entity view02 is
  port (
    inp_ctrl   : in  std_logic;
    inp_data_a : in  std_logic;
    inp_data_b : in  std_logic;
    m          :     view outer_master_view);
end;

architecture rtl of view02 is
begin
  m.ctrl   <= inp_ctrl;
  m.data.a <= inp_data_a;
  m.data.b <= inp_data_b;
end architecture rtl;
