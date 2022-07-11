library ieee;
use ieee.std_logic_1164.all;

entity afed is
  port (
    sig : in  std_logic;
    ack : in  std_logic;
    fe  : out std_logic
  );
end entity;

architecture behaviour of afed is

  signal fe_h : std_logic;
  signal fe_l : std_logic;

begin

  fe_h <= '0' when ack = '1' else
          '1' when sig = '1';
  fe_l <= '0' when ack = '1' else
          '1' when sig = '0' and fe_h = '1';

  fe <= fe_l;

end architecture;
