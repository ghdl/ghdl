library ieee;
use ieee.std_logic_1164.all;

package view03_pkg is
  type channel_t is record
    data : std_logic_vector(7 downto 0);
    valid : std_logic;
    ready : std_logic;
  end record;

  view channel_master_view of channel_t is
    valid : out;
    data : out;
    ready : in;
  end view channel_master_view;

  alias channel_slave_view is channel_master_view'converse;

  type bidir_t is record
    tx : channel_t;
    rx : channel_t;
  end record;

  view bidir_master_view of bidir_t is
    tx : view channel_master_view;
    rx : view channel_slave_view;
  end view;

  alias bidir_slave_view is bidir_master_view'converse;
end package;
