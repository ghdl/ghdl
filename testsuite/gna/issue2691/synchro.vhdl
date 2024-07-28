--==================================================================================================
--! @brief Clock Domain Boundary Synchronizer
--!
--!  Synchronizes an arbitrary data type over a clock domain boundary.
--!  Number of register stages is configurable.
--!
--! @author Mitja Vodnik <mitja.vodnik@cern.ch> (CERN - EP-LBC)
--! @date 06-05-2024 Created
--! @version 0.1
--! @copyright CERN 2024
--! **License**: CERN-OHL-W-v2
--==================================================================================================

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

----------------------------------------------------------------------------------------------------

entity synchro is
  generic (
    --! Arbitrary data type
    type data_t;

    --! Initial values for the output data
    g_INIT_VALUE : data_t;

    --! Number of register stages (must be at least 2)
    g_NUM_STAGES : natural range 2 to natural'high := 2
  );
  port (
    --! Destination clock and reset
    clk_i   : in    std_logic;
    reset_i : in    std_logic;

    --! Source data
    data_i : in    data_t;
    --! Destination data
    data_o : out   data_t
  );
end entity synchro;

----------------------------------------------------------------------------------------------------

architecture rtl of synchro is

  type sync_t is array (0 to g_NUM_STAGES - 1) of data_t;

  -- Synchronization chain of registers
  signal sync_chain : sync_t := (others => g_INIT_VALUE);

  -- CDC attributes for ALTERA/INTEL
  attribute preserve                       : boolean;
  attribute altera_attribute               : string;
  attribute preserve of sync_chain         : signal is true;
  attribute altera_attribute of sync_chain : signal is
     "-name SYNCHRONIZER_IDENTIFICATION ""FORCED IF ASYNCHRONOUS""";

  -- CDC attributes for XILINX/AMD
  attribute async_reg : boolean;
  attribute async_reg of sync_chain : signal is TRUE;

begin

  -- Output the last element of the register chain
  data_o <= sync_chain(g_NUM_STAGES - 1);

  -- Synchronous process: shifts the register chain every destination clock cycle
  proc_sync : process (clk_i) is
  begin

    if rising_edge(clk_i) then
      if (reset_i = '1') then
        sync_chain <= (others => g_INIT_VALUE);
      else
        sync_chain(0) <= data_i;

        for i in 0 to g_NUM_STAGES - 2 loop
          sync_chain(i + 1) <= sync_chain(i);
        end loop;

      end if;
    end if;

  end process proc_sync;

end architecture rtl;

----------------------------------------------------------------------------------------------------
