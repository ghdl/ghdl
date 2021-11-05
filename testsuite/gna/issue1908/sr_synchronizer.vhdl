
-----------------------------------------------------------------------------------
-- * Libs
-----------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

-----------------------------------------------------------------------------------
-- * Entity
-----------------------------------------------------------------------------------
ENTITY sr_synchronizer IS
  GENERIC (
    G_INIT       : std_logic := '0';   -- Reset value
    G_STAGES     : natural   := 2      -- Number of synchronization stages
  );
  PORT (
     CLK_I       : in   std_logic;
     RSTN_I      : in   std_logic;

     X_I         : in   std_logic;
     X_SYNC_O    : out  std_logic
  );
END sr_synchronizer;

-----------------------------------------------------------------------------------
-- * Architecture Begins
-----------------------------------------------------------------------------------
ARCHITECTURE synthetizable OF sr_synchronizer IS
  -----------------------------------------------------------------------------------
  -- * Types
  -----------------------------------------------------------------------------------
  type shift_regsiter_t is array (G_STAGES-1 downto 0) of std_logic;

  -----------------------------------------------------------------------------------
  -- * Signals
  -----------------------------------------------------------------------------------
  signal r_rst_cnt    : natural range 0 to G_STAGES;  -- To reset the shift-register stages
  signal r_x_sync_o   : shift_regsiter_t;             -- Shift register
  signal s_x_i        : std_logic;

-----------------------------------------------------------------------------------
-- * Architecture synthetizable
-----------------------------------------------------------------------------------
BEGIN
  -----------------------------------------------------------------------------------
  -- * Generics Constraints Checking
  -----------------------------------------------------------------------------------
  assert(G_INIT='0' or G_INIT='1')
    report "-ERROR- In 'sr_synchronizer' component, the generic parameter 'G_INIT' is bad defined." &
      " Must be '1' or '0' of std_logic type." severity failure;

  assert(G_STAGES > 0)
    report "-ERROR- In 'sr_synchronizer' component, the generic parameter 'G_STAGES' is bad defined." &
      " Must be 1 at least." severity failure;

  -----------------------------------------------------------------------------------
  -- * Mapping IO
  -----------------------------------------------------------------------------------
  X_SYNC_O  <= r_x_sync_o(r_x_sync_o'high);
  s_x_i     <= X_I;

  -----------------------------------------------------------------------------------
  -- * Process
  -----------------------------------------------------------------------------------
    -- ** Shift Register
    -----------------------------------------------------------------------------------
    shift_register: process(CLK_I)
    begin
      if rising_edge(CLK_I) then
        if RSTN_I='0' then
          r_rst_cnt <= G_STAGES;                                                  -- Reset value of reset counter
        else
          if r_rst_cnt=0 then                                                     -- In case all shift register stages has performed
            r_x_sync_o <=                                                         -- reset, change shift register input to 's_x_i'
              r_x_sync_o(r_x_sync_o'high-1 downto r_x_sync_o'low) & s_x_i;        -- value
          else
            r_rst_cnt  <= r_rst_cnt-1;                                            -- In case not all shift register stages has performed
            r_x_sync_o <=                                                         -- reset, change shift register input to 'G_INIT'
              r_x_sync_o(r_x_sync_o'high-1 downto r_x_sync_o'low) & G_INIT;       -- value and, decrement reset counter value
          end if;
       end if; -- RSTN_I
      end if; -- rising_edge(CLK_I)
    end process;

-----------------------------------------------------------------------------------
-- * Architecture Ends
-----------------------------------------------------------------------------------
END synthetizable;
