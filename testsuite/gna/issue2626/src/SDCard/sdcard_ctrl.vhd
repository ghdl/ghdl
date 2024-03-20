-- This is the wrapper file for the complete SDCard controller.

-- The SD Card is powered up in the SD mode. It will enter SPI mode if the
-- CS (DAT3) signal is asserted (negative) during the reception of the reset
-- command (CMD0). If the card recognizes that the SD mode is required it
-- will not respond to the command and remain in the SD mode. If SPI mode is
-- required, the card will switch to SPI and respond with the SPI mode R1
-- response.

-- List of used commands:
-- CMD0  : GO_IDLE_STATE: Resets the SD Card.
-- CMD3  : SEND_RCA
-- CMD8  : SEND_IF_COND: Sends SD Memory Card interface condition that includes host supply voltage.
-- ACMD41: SD_SEND_OP_COND: Sends host capacity support information and activated the card's initialization process.
-- CMD13 : SEND_STATUS: Asks the selected card to send its status register.
-- CMD16 : SET_BLOCKLEN: In case of non-SDHC card, this sets the block length. Block length of SDHC/SDXC cards are fixed to 512 bytes
-- CMD17 : READ_SINGLE_BLOCK
-- CMD24 : WRITE_BLOCK
-- CMD55 : APP_CMD: Next command is an application specific command.
-- CMD58 : READ_OCR: Read the OCR register of the card.

-- Created by Michael Jørgensen in 2022 (mjoergen.github.io/SDCard).

library ieee;
   use ieee.std_logic_1164.all;
   use ieee.numeric_std.all;

library work;
   use work.sdcard_globals.all;

entity sdcard_ctrl is
   port (
      -- Avalon Memory Map
      avm_clk_i         : in    std_logic; -- 50 Mhz
      avm_rst_i         : in    std_logic; -- Synchronous reset, active high
      avm_write_i       : in    std_logic;
      avm_read_i        : in    std_logic;
      avm_address_i     : in    std_logic_vector(31 downto 0);
      avm_writedata_i   : in    std_logic_vector(7 downto 0);
      avm_burstcount_i  : in    std_logic_vector(15 downto 0);
      avm_waitrequest_o : out   std_logic;
      avm_init_error_o  : out   std_logic;
      avm_last_state_o  : out   std_logic_vector(7 downto 0);

      sd_clk_o          : out   std_logic;
      cmd_valid_o       : out   std_logic;
      cmd_ready_i       : in    std_logic;
      cmd_index_o       : out   natural range 0 to 63;
      cmd_data_o        : out   std_logic_vector(31 downto 0);
      cmd_resp_o        : out   natural range 0 to 255;
      cmd_timeout_o     : out   natural range 0 to 2 ** 24 - 1;
      resp_valid_i      : in    std_logic;
      resp_ready_o      : out   std_logic;
      resp_data_i       : in    std_logic_vector(135 downto 0);
      resp_timeout_i    : in    std_logic;
      resp_error_i      : in    std_logic;
      dat_ready_i       : in    std_logic
   );
end entity sdcard_ctrl;

architecture synthesis of sdcard_ctrl is

   -- Number of attempts at initiliazing card (ACMD41)
   constant C_INIT_COUNT_MAX : natural                 := 100; -- Approximately one second

   -- An arbitrary 8-bit pattern
   constant C_CMD8_CHECK : std_logic_vector(7 downto 0) := X"5B";

   signal   clk_counter : std_logic_vector(6 downto 0) := (others => '0');
   signal   init_count  : natural range 0 to C_INIT_COUNT_MAX;

   signal   card_ver2 : std_logic;
   signal   card_ccs  : std_logic;
   signal   card_cid  : std_logic_vector(127 downto 0);
   signal   card_csd  : std_logic_vector(127 downto 0);
   signal   card_rca  : std_logic_vector(15 downto 0);

   -- State diagram in Figure 4-7 page 56.
   type     state_type is (
      -- Slow clock
      INIT_ST,
      GO_IDLE_STATE_ST,
      SEND_IF_COND_ST,
      SD_SEND_OP_COND_APP_ST,
      SD_SEND_OP_COND_ST,
      ALL_SEND_CID_ST,
      SEND_RELATIVE_ADDR_ST,
      ERROR_ST,
      -- Fast clock
      SEND_CSD_ST,
      SELECT_CARD_ST,
      SET_BUS_WIDTH_APP_ST,
      SET_BUS_WIDTH_ST,
      READY_ST,
      READ_SINGLE_BLOCK_ST,
      WAITING_ST,
      READING_ST
   );

   signal   state : state_type                         := INIT_ST;

   pure function state_to_slv (
      state_v : state_type
   ) return std_logic_vector is
   begin
      null;

      case state_v is

         when INIT_ST =>
            return X"00";

         when GO_IDLE_STATE_ST =>
            return X"01";

         when SEND_IF_COND_ST =>
            return X"02";

         when SD_SEND_OP_COND_APP_ST =>
            return X"03";

         when SD_SEND_OP_COND_ST =>
            return X"04";

         when ALL_SEND_CID_ST =>
            return X"05";

         when SEND_RELATIVE_ADDR_ST =>
            return X"06";

         when ERROR_ST =>
            return X"07";

         when SEND_CSD_ST =>
            return X"10";

         when SELECT_CARD_ST =>
            return X"11";

         when SET_BUS_WIDTH_APP_ST =>
            return X"12";

         when SET_BUS_WIDTH_ST =>
            return X"13";

         when READY_ST =>
            return X"14";

         when READ_SINGLE_BLOCK_ST =>
            return X"15";

         when WAITING_ST =>
            return X"16";

         when READING_ST =>
            return X"17";

      end case;

      return "XXXXXXXX";
   end function state_to_slv;

begin

   resp_ready_o      <= '1';

   ----------------------------------
   -- Generate SD clock
   ----------------------------------

   -- Divide by 64
   counter_proc : process (avm_clk_i)
   begin
      if rising_edge(avm_clk_i) then
         clk_counter <= std_logic_vector(unsigned(clk_counter) + 1);
      end if;
   end process counter_proc;

   out_proc : process (avm_clk_i)
   begin
      if rising_edge(avm_clk_i) then

         case state is

            when INIT_ST                |
                 GO_IDLE_STATE_ST       |
                 SEND_IF_COND_ST        |
                 SD_SEND_OP_COND_APP_ST |
                 SD_SEND_OP_COND_ST     |
                 ALL_SEND_CID_ST        |
                 SEND_RELATIVE_ADDR_ST  |
                 ERROR_ST               |
                 SEND_CSD_ST            |
                 SELECT_CARD_ST         |
                 SET_BUS_WIDTH_APP_ST   |
                 SET_BUS_WIDTH_ST =>
               sd_clk_o      <= clk_counter(6);  -- 50 MHz / 64 / 2 = 391 kHz
               cmd_timeout_o <= 4000;            -- 10 ms @ 400 kHz

            when READY_ST               |
                 READ_SINGLE_BLOCK_ST   |
                 WAITING_ST             |
                 READING_ST =>
               sd_clk_o      <= clk_counter(0);  -- 50 MHz / 2 = 25 MHz
               cmd_timeout_o <= 12500000;        -- 1 second @ 12.5 MHz

         end case;

      end if;
   end process out_proc;


   -- From Part1_Physical_Layer_Simplified_Specification_Ver8.00.pdf,
   -- Section 4.8 Card State Transition Table, Page 128.
   -- Section 4.2 Card Identification Mode, Page 59.
   -- State Machine shown in Figure 4-2, Page 62.
   -- Section 4.7.4 Detailed Command Description

   avm_waitrequest_o <= '0' when state = READY_ST else
                        '1';

   fsm_proc : process (avm_clk_i)
   begin
      if rising_edge(avm_clk_i) then
         if state /= ERROR_ST then
            avm_last_state_o <= state_to_slv(state);
         end if;

         if cmd_ready_i = '1' then
            cmd_valid_o <= '0';
         end if;

         case state is

            when INIT_ST =>
               if cmd_ready_i = '1' then
                  -- Initialize information about card
                  card_ver2        <= '0';
                  card_ccs         <= '0';
                  card_cid         <= (others => '0');
                  card_csd         <= (others => '0');
                  card_rca         <= (others => '0');
                  avm_init_error_o <= '0';

                  -- Send CMD0 (see section 4.2.1)
                  -- This resets the SD Card
                  cmd_index_o      <= C_CMD_GO_IDLE_STATE;
                  cmd_data_o       <= (others => '0');
                  cmd_resp_o       <= 0;
                  cmd_valid_o      <= '1';
                  -- Retry count for ACMD41
                  init_count       <= C_INIT_COUNT_MAX;
                  state            <= GO_IDLE_STATE_ST;
               end if;

            when GO_IDLE_STATE_ST =>
               -- We've sent CMD0, no response expected
               if cmd_ready_i = '1' then
                  -- Send CMD8 (see sections 4.2.2 and 4.3.13)
                  -- This probes the SD Card for protocol version 2.0 or later.
                  cmd_index_o              <= C_CMD_SEND_IF_COND;
                  cmd_data_o               <= (others => '0');
                  cmd_data_o(R_CMD8_1_2V)  <= "0";
                  cmd_data_o(R_CMD8_PCIE)  <= "0";
                  cmd_data_o(R_CMD8_VHS)   <= C_CMD8_VHS_27_36;
                  cmd_data_o(R_CMD8_CHECK) <= C_CMD8_CHECK;
                  cmd_resp_o               <= C_RESP_R7_LEN;
                  cmd_valid_o              <= '1';
                  state                    <= SEND_IF_COND_ST;
               end if;

            when SEND_IF_COND_ST =>
               -- We've sent CMD8, expecting response R7
               if resp_valid_i = '1' then
                  -- Check response R7
                  if resp_timeout_i = '0' and resp_error_i = '0' and
                     resp_data_i(R_CMD_INDEX) = std_logic_vector(to_unsigned(C_CMD_SEND_IF_COND, 8)) and
                     resp_data_i(R_CMD8_VHS)    = C_CMD8_VHS_27_36 and
                     resp_data_i(R_CMD8_CHECK)  = C_CMD8_CHECK  then
                     -- Valid response means card is Ver 2.X
                     card_ver2             <= '1';

                     -- Send ACMD41 (see section 5.1)
                     cmd_index_o           <= C_CMD_APP_CMD;
                     cmd_data_o            <= (others => '0');
                     cmd_data_o(R_CMD_RCA) <= C_CMD_RCA_DEFAULT;
                     cmd_resp_o            <= C_RESP_R1_LEN;
                     cmd_valid_o           <= '1';
                     state                 <= SD_SEND_OP_COND_APP_ST;
                  elsif resp_timeout_i = '1' then
                     -- Timeout means the card did not respond to our CMD8.
                     -- Most likely, it is a "Ver 1.X Standard Capacity SD Memory Card".

                     -- Send ACMD41 (see section 5.1)
                     cmd_index_o           <= C_CMD_APP_CMD;
                     cmd_data_o            <= (others => '0');
                     cmd_data_o(R_CMD_RCA) <= C_CMD_RCA_DEFAULT;
                     cmd_resp_o            <= C_RESP_R1_LEN;
                     cmd_valid_o           <= '1';
                     state                 <= SD_SEND_OP_COND_APP_ST;
                  else
                     state <= ERROR_ST;
                  end if;
               end if;

            when SD_SEND_OP_COND_APP_ST =>
               -- We've sent CMD55, expecting response R1
               if resp_valid_i = '1' then
                  -- Check response R1
                  if resp_timeout_i = '0' and resp_error_i = '0' and
                     resp_data_i(R_CMD_INDEX) = std_logic_vector(to_unsigned(C_CMD_APP_CMD, 8)) and
                     resp_data_i(R_CARD_STAT_CURRENT_STATE)  = C_CARD_STATE_IDLE and
                     resp_data_i(C_CARD_STAT_READY_FOR_DATA) = '1' and
                     resp_data_i(C_CARD_STAT_APP_CMD)        = '1' then
                     cmd_index_o           <= C_ACMD_SD_SEND_OP_COND;
                     cmd_data_o            <= (others => '0');
                     cmd_data_o(C_ACMD41_OCR_33X) <= '1';
                     if card_ver2 = '1' then
                        cmd_data_o(C_ACMD41_HCS) <= '1';
                        cmd_data_o(C_ACMD41_XPC) <= '1';
                     end if;
                     cmd_resp_o  <= C_RESP_R3_LEN;
                     cmd_valid_o <= '1';
                     state       <= SD_SEND_OP_COND_ST;
                  else
                     state <= ERROR_ST;
                  end if;
               end if;

            when SD_SEND_OP_COND_ST =>
               -- We've sent ACMD41, expecting response R3
               if resp_valid_i = '1' then
                  -- Check response R3
                  if resp_timeout_i = '0' and resp_error_i = '0' and
                     resp_data_i(R_CMD_INDEX) = X"3F" then
                     -- Wait for BUSY bit to be set (de-asserted)
                     if resp_data_i(C_R3_BUSY) = '1' then
                        -- Card Capacity Status
                        if card_ver2 = '1' then
                           card_ccs <= resp_data_i(C_R3_CCS);
                        end if;

                        cmd_index_o <= C_CMD_ALL_SEND_CID;
                        cmd_data_o  <= (others => '0');
                        cmd_resp_o  <= C_RESP_R2_LEN;
                        cmd_valid_o <= '1';
                        state       <= ALL_SEND_CID_ST;
                     elsif init_count > 0 then
                        init_count            <= init_count - 1;

                        -- Send ACMD41 again
                        cmd_index_o           <= C_CMD_APP_CMD;
                        cmd_data_o            <= (others => '0');
                        cmd_data_o(R_CMD_RCA) <= C_CMD_RCA_DEFAULT;
                        cmd_resp_o            <= C_RESP_R1_LEN;
                        cmd_valid_o           <= '1';
                        state                 <= SD_SEND_OP_COND_APP_ST;
                     else
                        state <= ERROR_ST;
                     end if;
                  else
                     state <= ERROR_ST;
                  end if;
               end if;

            when ALL_SEND_CID_ST =>
               -- We've sent CMD2, expecting response R2
               if resp_valid_i = '1' then
                  -- Check response R2
                  if resp_timeout_i = '0' and resp_error_i = '0' and
                     resp_data_i(127 downto 120) = X"3F" then
                     -- Store CID
                     card_cid    <= resp_data_i(R_R2_CID);

                     cmd_index_o <= C_CMD_SEND_RELATIVE_ADDR;
                     cmd_data_o  <= (others => '0');
                     cmd_resp_o  <= C_RESP_R6_LEN;
                     cmd_valid_o <= '1';
                     state       <= SEND_RELATIVE_ADDR_ST;
                  else
                     state <= ERROR_ST;
                  end if;
               end if;

            when SEND_RELATIVE_ADDR_ST =>
               -- We've sent CMD3, expecting response R6
               if resp_valid_i = '1' then
                  -- Check response R6
                  if resp_timeout_i = '0' and resp_error_i = '0' and
                     resp_data_i(R_CMD_INDEX) = std_logic_vector(to_unsigned(C_CMD_SEND_RELATIVE_ADDR, 8)) and
                     resp_data_i(R_R6_STAT_CURRENT_STATE)  = C_CARD_STATE_IDENT and
                     resp_data_i(C_R6_STAT_READY_FOR_DATA) = '1' then
                     card_rca              <= resp_data_i(R_R6_RCA);

                     cmd_index_o           <= C_CMD_SEND_CSD;
                     cmd_data_o            <= (others => '0');
                     cmd_data_o(R_CMD_RCA) <= resp_data_i(R_R6_RCA);
                     cmd_resp_o            <= C_RESP_R2_LEN;
                     cmd_valid_o           <= '1';
                     state                 <= SEND_CSD_ST;
                  else
                     state <= ERROR_ST;
                  end if;
               end if;

            when SEND_CSD_ST =>
               -- We've sent CMD9, expecting response R2
               if resp_valid_i = '1' then
                  -- Check response R2
                  if resp_timeout_i = '0' and resp_error_i = '0' and
                     resp_data_i(127 downto 120) = X"3F" then
                     -- Store CSD
                     card_csd              <= resp_data_i(R_R2_CSD);

                     cmd_index_o           <= C_CMD_SELECT_CARD;
                     cmd_data_o            <= (others => '0');
                     cmd_data_o(R_CMD_RCA) <= card_rca;
                     cmd_resp_o            <= C_RESP_R1_LEN;
                     cmd_valid_o           <= '1';
                     state                 <= SELECT_CARD_ST;
                  else
                     state <= ERROR_ST;
                  end if;
               end if;

            when SELECT_CARD_ST =>
               -- We've sent CMD7, expecting response R1b
               if resp_valid_i = '1' then
                  -- Check response R1b
                  if resp_timeout_i = '0' and resp_error_i = '0' and
                     resp_data_i(R_CMD_INDEX) = std_logic_vector(to_unsigned(C_CMD_SELECT_CARD, 8)) and
                     resp_data_i(R_CARD_STAT_CURRENT_STATE)  = C_CARD_STATE_STBY and
                     resp_data_i(C_CARD_STAT_READY_FOR_DATA) = '1' and
                     resp_data_i(C_CARD_STAT_APP_CMD)        = '0' then
                     cmd_index_o              <= C_CMD_APP_CMD;
                     cmd_data_o               <= (others => '0');
                     cmd_data_o(31 downto 16) <= card_rca;
                     cmd_resp_o               <= C_RESP_R1_LEN;
                     cmd_valid_o              <= '1';
                     state                    <= SET_BUS_WIDTH_APP_ST;
                  else
                     state <= ERROR_ST;
                  end if;
               end if;

            when SET_BUS_WIDTH_APP_ST =>
               -- We've sent CMD55, expecting response R1
               if resp_valid_i = '1' then
                  -- Check response R1
                  if resp_timeout_i = '0' and resp_error_i = '0' and
                     resp_data_i(R_CMD_INDEX) = std_logic_vector(to_unsigned(C_CMD_APP_CMD, 8)) and
                     resp_data_i(R_CARD_STAT_CURRENT_STATE)  = C_CARD_STATE_TRAN and
                     resp_data_i(C_CARD_STAT_READY_FOR_DATA) = '1' and
                     resp_data_i(C_CARD_STAT_APP_CMD)        = '1' then
                     cmd_index_o                   <= C_ACMD_SET_BUS_WIDTH;
                     cmd_data_o                    <= (others => '0');
                     cmd_data_o(R_ACMD6_BUS_WIDTH) <= C_ACMD6_BUS_WIDTH_4;
                     cmd_resp_o                    <= C_RESP_R1_LEN;
                     cmd_valid_o                   <= '1';
                     state                         <= SET_BUS_WIDTH_ST;
                  else
                     state <= ERROR_ST;
                  end if;
               end if;

            when SET_BUS_WIDTH_ST =>
               -- We've sent ACMD6, expecting response R1
               if resp_valid_i = '1' then
                  -- Check response R1
                  if resp_timeout_i = '0' and resp_error_i = '0' and
                     resp_data_i(R_CMD_INDEX) = std_logic_vector(to_unsigned(C_ACMD_SET_BUS_WIDTH, 8)) and
                     resp_data_i(R_CARD_STAT_CURRENT_STATE)  = C_CARD_STATE_TRAN and
                     resp_data_i(C_CARD_STAT_READY_FOR_DATA) = '1' and
                     resp_data_i(C_CARD_STAT_APP_CMD)        = '1' then
                     state <= READY_ST;
                  else
                     state <= ERROR_ST;
                  end if;
               end if;

            when READY_ST =>
               if avm_read_i = '1' then
                  cmd_index_o <= C_CMD_READ_SINGLE_BLOCK;
                  cmd_data_o  <= avm_address_i;
                  cmd_resp_o  <= C_RESP_R1_LEN;
                  cmd_valid_o <= '1';
                  state       <= READ_SINGLE_BLOCK_ST;
               end if;

            when READ_SINGLE_BLOCK_ST =>
               -- We've sent CMD17, expecting response R1
               if resp_valid_i = '1' then
                  -- Check response R1
                  if resp_timeout_i = '0' and resp_error_i = '0' then
                     state <= WAITING_ST;
                  else
                     state <= ERROR_ST;
                  end if;
               end if;
               if dat_ready_i = '0' then
                  state <= READING_ST;
               end if;

            when WAITING_ST =>
               if dat_ready_i = '0' then
                  state <= READING_ST;
               end if;

            when READING_ST =>
               if dat_ready_i = '1' then
                  state <= READY_ST;
               end if;

            when ERROR_ST =>
               avm_init_error_o <= '1';

            when others =>
               null;

         end case;

         if avm_rst_i = '1' then
            state       <= INIT_ST;
            cmd_valid_o <= '0';
         end if;
      end if;
   end process fsm_proc;

end architecture synthesis;

