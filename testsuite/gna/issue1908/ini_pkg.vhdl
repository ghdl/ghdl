LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

-----------------------------------------------------------------------------------
-- * Package
-----------------------------------------------------------------------------------
package ini_pkg is
  -----------------------------------------------------------------------------------
  -- * Types
  -----------------------------------------------------------------------------------
  -- With the following values no bug ocurred
  type ini_t is record
    val_of_0 : std_logic;
    val_of_1 : std_logic;
    val_of_2 : std_logic;
    val_of_3 : std_logic;
    val_of_4 : std_logic;
    val_of_5 : std_logic;
  end record;
  
  -- With the following values a an exception rise in GHDL, printing the GHDL Bug Ocurred Message
  -- type ini_t is record
  --   vval_of_0 : std_logic;
  --   val_of_1 : std_logic;
  --   val_of_2 : std_logic;
  --   val_of_3 : std_logic;
  --   val_of_4 : std_logic;
  --   val_of_5 : std_logic;
  -- end record;


-----------------------------------------------------------------------------------
-- * End Package
-----------------------------------------------------------------------------------
end package ini_pkg;

