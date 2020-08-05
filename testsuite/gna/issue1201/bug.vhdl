library ieee;
use ieee.std_logic_1164.all;

entity bug is
  port (clk : in std_logic;
        rst : in std_logic);
end entity bug;

architecture Behavioral of bug is

  type rc_params is array (natural range <>) of std_logic_vector(7 downto 0);
  type rc_params_array is array (natural range <>) of rc_params;

  type rc_peripheral_1 is record
    clk    : std_logic;
    params : rc_params;
    enable : std_logic;
    rd     : std_logic;
    data   : std_logic_vector(7 downto 0);
    empty  : std_logic;
  end record rc_peripheral_1;

  type rc_peripheral_2 is record
    clk    : std_logic;
    params : rc_params_array;
    enable : std_logic;
    rd     : std_logic;
    data   : std_logic_vector(7 downto 0);
    empty  : std_logic;
  end record rc_peripheral_2;

  signal mysig_ok : rc_params_array(0 to 4)(0 to 4);  -- This is accepted by ghdl

  signal mysig_ok_2 : rc_peripheral_1(params(0 to 4));  -- This is accepted by ghdl

  signal mysig_wrong : rc_peripheral_2(params(0 to 4)(0 to 4));  -- This is not accepted

begin

end architecture Behavioral;
