library ieee;
  use ieee.std_logic_1164.all;

entity ApbMasterBfmE is
  generic (
    G_ADDR_WIDTH  : positive := 8;  --* address bus width
    G_DATA_WIDTH  : positive := 8;  --* data bus width
    G_SLAVE_COUNT : positive := 1
  );
  port (
    PRreset_n_i   : in std_logic;
    PClk_i        : in std_logic
  );
end entity ApbMasterBfmE;


package MyTestPkg is new work.TestPkg generic map (G_TEST => 17);


architecture sim of ApbMasterBfmE is

  use work.MyTestPkg.all;

begin

  assert false report "done" severity note;

end architecture sim;
