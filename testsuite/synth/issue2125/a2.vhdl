library ieee;
use ieee.std_logic_1164.all;

entity a2 is
  port (
    sig : in  std_logic;
    ack : in  std_logic;
    fe  : out std_logic
  );
end entity;

architecture behaviour of a2 is

  signal fe_h : std_logic;

begin

  fe_h <= '0' when ack = '1' else
          '1' when sig = '1';
  fe <= fe_h;

end architecture;
