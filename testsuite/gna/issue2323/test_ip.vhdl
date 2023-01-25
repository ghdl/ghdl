library ieee;
use ieee.std_logic_1164.all;

entity TEST_IP is
port (
  I_CLK                   : in std_logic;
  I_RST                   : in std_logic;

  I_DATA                  : in std_logic;
  O_DATA                  : out std_logic
);
end entity TEST_IP;

architecture TEST_IP_ARCH of TEST_IP is

  signal test_signal      : std_logic_vector(7 downto -1);

begin

end architecture TEST_IP_ARCH;
