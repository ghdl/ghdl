-----------------------------------------------------------------------------------
-- * Libs
-----------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

LIBRARY WORK;
USE WORK.ini_pkg.all;

-----------------------------------------------------------------------------------
-- * Entity
-----------------------------------------------------------------------------------
ENTITY map_synchro IS
  PORT (
    CLK_I                : in   std_logic;
    RSTN_I               : in   std_logic;

    A_val_of_0 : in   std_logic;
    A_val_of_1 : in   std_logic;
    A_val_of_2 : in   std_logic;
    A_val_of_3 : in   std_logic;
    A_val_of_4 : in   std_logic;
    A_val_of_5 : in   std_logic;
    A_SYNC_O   : out  ini_t; 

    B_val_of_0 : in   std_logic;
    B_val_of_1 : in   std_logic;
    B_val_of_2 : in   std_logic;
    B_val_of_3 : in   std_logic;
    B_val_of_4 : in   std_logic;
    B_val_of_5 : in   std_logic;
    B_SYNC_O   : out  ini_t 
    
  );
END map_synchro;
-----------------------------------------------------------------------------------
-- * Architecture Begins
-----------------------------------------------------------------------------------
ARCHITECTURE structure OF map_synchro IS
  -----------------------------------------------------------------------------------
  -- * Components Declaration
  -----------------------------------------------------------------------------------
  component sr_synchronizer is
    generic (
      G_INIT       : std_logic := '0';   -- Reset value
      G_STAGES     : natural   := 2      -- Number of synchronization stages
    );
    port (
      CLK_I       : in   std_logic;
      RSTN_I      : in   std_logic;

      X_I         : in   std_logic;
      X_SYNC_O    : out  std_logic
    );
  end component sr_synchronizer;

  -----------------------------------------------------------------------------------
  -- * Signals
  -----------------------------------------------------------------------------------
  -- With the following values a an exception rise in GHDL, printing the GHDL Bug Ocurred Message
  signal s_a_sync_o : std_logic; 
  signal s_b_sync_o : std_logic;
  
  -- With the following values no bug ocurred
  -- signal s_a_sync_o : ini_t; 
  -- signal s_b_sync_o : ini_t;


-----------------------------------------------------------------------------------
-- * Architecture structure
-----------------------------------------------------------------------------------
BEGIN
  -----------------------------------------------------------------------------------
  -- * Mapping IO
  -----------------------------------------------------------------------------------
  A_SYNC_O     <= s_a_sync_o;
  B_SYNC_O     <= s_b_sync_o;

  -----------------------------------------------------------------------------------
  -- * Components Instatiation
  -----------------------------------------------------------------------------------
    -- ** 6 Synchronizers for A
    -----------------------------------------------------------------------------------
    a_synchronizer_0: sr_synchronizer
    port map (
      CLK_I     => CLK_I,
      RSTN_I    => RSTN_I,

      X_I       => A_val_of_0,
      X_SYNC_O  => s_a_sync_o.val_of_0
    );

    a_synchronizer_1: sr_synchronizer
    port map (
      CLK_I     => CLK_I,
      RSTN_I    => RSTN_I,

      X_I       => A_val_of_1,
      X_SYNC_O  => s_a_sync_o.val_of_1
    );

    a_synchronizer_2: sr_synchronizer
    port map (
      CLK_I     => CLK_I,
      RSTN_I    => RSTN_I,

      X_I       => A_val_of_2,
      X_SYNC_O  => s_a_sync_o.val_of_2
    );

    a_synchronizer_3: sr_synchronizer
    port map (
      CLK_I     => CLK_I,
      RSTN_I    => RSTN_I,

      X_I       => A_val_of_3,
      X_SYNC_O  => s_a_sync_o.val_of_3
    );

    a_synchronizer_4: sr_synchronizer
    port map (
      CLK_I     => CLK_I,
      RSTN_I    => RSTN_I,

      X_I       => A_val_of_4,
      X_SYNC_O  => s_a_sync_o.val_of_4
    );

    a_synchronizer_5: sr_synchronizer
    port map (
      CLK_I     => CLK_I,
      RSTN_I    => RSTN_I,

      X_I       => A_val_of_5,
      X_SYNC_O  => s_a_sync_o.val_of_5
    );

    -- ** 6 Synchronizers for B
    -----------------------------------------------------------------------------------
    b_synchronizer_0: sr_synchronizer
    port map (
      CLK_I     => CLK_I,
      RSTN_I    => RSTN_I,

      X_I       => B_val_of_0,
      X_SYNC_O  => s_b_sync_o.val_of_0
    );

    b_synchronizer_1: sr_synchronizer
    port map (
      CLK_I     => CLK_I,
      RSTN_I    => RSTN_I,

      X_I       => B_val_of_1,
      X_SYNC_O  => s_b_sync_o.val_of_1
    );

    b_synchronizer_2: sr_synchronizer
    port map (
      CLK_I     => CLK_I,
      RSTN_I    => RSTN_I,

      X_I       => B_val_of_2,
      X_SYNC_O  => s_b_sync_o.val_of_2
    );

    b_synchronizer_3: sr_synchronizer
    port map (
      CLK_I     => CLK_I,
      RSTN_I    => RSTN_I,

      X_I       => B_val_of_3,
      X_SYNC_O  => s_b_sync_o.val_of_3
    );

    b_synchronizer_4: sr_synchronizer
    port map (
      CLK_I     => CLK_I,
      RSTN_I    => RSTN_I,

      X_I       => B_val_of_4,
      X_SYNC_O  => s_b_sync_o.val_of_4
    );

    b_synchronizer_5: sr_synchronizer
    port map (
      CLK_I     => CLK_I,
      RSTN_I    => RSTN_I,

      X_I       => B_val_of_5,
      X_SYNC_O  => s_b_sync_o.val_of_5
    );

-----------------------------------------------------------------------------------
-- * Architecture Ends
-----------------------------------------------------------------------------------
END structure;
