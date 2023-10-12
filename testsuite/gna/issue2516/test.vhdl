LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY test IS
  GENERIC(
    MASTER_ID_WIDTH          : positive :=  4;
    ID_EXTENSION_WIDTH       : positive :=  4
  );
  PORT(
    bid_extension  : OUT std_logic_vector(ID_EXTENSION_WIDTH-1 DOWNTO 0);
    rid_extension  : OUT std_logic_vector(ID_EXTENSION_WIDTH-1 DOWNTO 0)
  );
END ENTITY;

ARCHITECTURE rtl OF test IS

  CONSTANT SLAVE_ID_WIDTH : positive := MASTER_ID_WIDTH + ID_EXTENSION_WIDTH;

  SIGNAL master_bid           : std_logic_vector(MASTER_ID_WIDTH-1 DOWNTO 0);
  SIGNAL master_rid           : std_logic_vector(MASTER_ID_WIDTH-1 DOWNTO 0);
  SIGNAL master_bid_extended  : std_logic_vector(SLAVE_ID_WIDTH-1 DOWNTO 0);
  SIGNAL master_rid_extended  : std_logic_vector(SLAVE_ID_WIDTH-1 DOWNTO 0);

BEGIN

  (bid_extension, master_bid) <= master_bid_extended;
  (rid_extension, master_rid) <= master_rid_extended;

END ARCHITECTURE;
