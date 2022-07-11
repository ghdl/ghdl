library ieee;
use ieee.std_logic_1164.all;
entity afed is
  port (
    sig: in std_logic;
    ack: in std_logic;
    fe: out std_logic
  );
end entity;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture rtl of afed is
  signal fe_h : std_logic;
  signal fe_l : std_logic;
  signal n4_o : std_logic;
  signal n9_o : std_logic;
  signal n10_o : std_logic;
begin
  fe <= fe_l;

  n4_o <= fe_h when sig = '0' else '1';
  fe_h <= n4_o when ack = '0' else '0';

  n9_o <= (not sig) and fe_h;                  -- 0
  n10_o <= fe_l when n9_o = '0' else '1';
  fe_l <= n10_o when ack = '0' else '0';
end rtl;
