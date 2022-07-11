library ieee;
use ieee.std_logic_1164.all;

entity a3 is
  port (
    sig : in  std_logic;
    ack : in  std_logic;
    fe  : out std_logic
  );
end entity;

architecture behaviour of a3 is

  signal fe_h : std_logic;

begin
  fe <= fe_h;

  fe_h <= '0' when ack = '1' else
          '1' when sig = '1' else fe_h;
end architecture;
