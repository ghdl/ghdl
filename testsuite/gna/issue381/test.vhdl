library ieee;
use ieee.std_logic_1164.all;

entity can_rx is
  port (
          demo     : in std_logic;
          status : out std_logic_vector (31 downto 0)
  );
end can_rx;

architecture rtl of can_rx is
begin
 status(0) <= (0=>'0', others => '0') when demo = '1' else (0=>'1', others => '0');
end rtl;
