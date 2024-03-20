-- This block sends commands to the SDCard and receives responses.
-- Only one outstanding command is allowed at any time.
-- This module checks for timeout, and always generates a response, when a response is expected.
-- CRC generation is performed on all commands.

-- Created by Michael Jørgensen in 2022 (mjoergen.github.io/SDCard).

library ieee;
   use ieee.std_logic_1164.all;
   use ieee.numeric_std.all;

library work;
   use work.sdcard_globals.all;

entity sdcard_cmd is
   port (
      clk_i          : in    std_logic;                      -- 50 MHz
      rst_i          : in    std_logic;

      -- Command to send to SDCard
      cmd_valid_i    : in    std_logic;
      cmd_ready_o    : out   std_logic;
      cmd_index_i    : in    natural range 0 to 63;
      cmd_data_i     : in    std_logic_vector(31 downto 0);
      cmd_resp_i     : in    natural range 0 to 255;         -- Expected number of bits in response
      cmd_timeout_i  : in    natural range 0 to 2 ** 24 - 1; -- Timeout in SD card clock cycles (max 1 second)

      -- Response received from SDCard
      resp_valid_o   : out   std_logic;
      resp_ready_i   : in    std_logic;
      resp_data_o    : out   std_logic_vector(135 downto 0);
      resp_timeout_o : out   std_logic;                      -- No response received
      resp_error_o   : out   std_logic;                      -- Reponse with CRC error

      -- SDCard device interface
      sd_clk_i       : in    std_logic;                      -- 25 MHz or 400 kHz
      sd_cmd_in_i    : in    std_logic;
      sd_cmd_out_o   : out   std_logic;
      sd_cmd_oe_n_o  : out   std_logic
   );
end entity sdcard_cmd;

architecture synthesis of sdcard_cmd is

   constant C_IDLE_MAX     : natural := 400; -- Idle for 1 ms after power-on.
   constant C_COOLDOWN_MAX : natural := 5;   -- Wait a few clock cycles before next command.

   type     state_type is (
      INIT_ST,
      IDLE_ST,
      WRITING_ST,
      SEND_CRC_ST,
      WAIT_RESPONSE_ST,
      GET_RESPONSE_ST,
      COOLDOWN_ST
   );

   signal   state : state_type       := INIT_ST;

   signal   idle_count     : natural range 0 to C_IDLE_MAX;
   signal   timeout_count  : natural range 0 to 2 ** 24 - 1;
   signal   cooldown_count : natural range 0 to C_COOLDOWN_MAX;

   signal   sd_clk_d : std_logic;

   signal   send_data  : std_logic_vector(39 downto 0);
   signal   send_count : natural range 0 to 39;
   signal   crc        : std_logic_vector(6 downto 0);
   signal   resp_data  : std_logic_vector(143 downto 0);
   signal   resp_count : natural range 0 to 255;

   -- This calculates the 7-bit CRC using the polynomial x^7 + x^3 + x^0.
   -- See this link: http://www.ghsi.de/pages/subpages/Online%20CRC%20Calculation/indexDetails.php?Polynom=10001001&Message=7700000000

   pure function new_crc (
      cur_crc : std_logic_vector;
      val : std_logic
   ) return std_logic_vector is
      variable inv_v : std_logic;
      variable upd_v : std_logic_vector(6 downto 0);
   begin
      inv_v := val xor cur_crc(6);
      upd_v := (0 => inv_v, 3 => inv_v, others => '0');
      return (cur_crc(5 downto 0) & "0") xor upd_v;
   end function new_crc;

begin

   cmd_ready_o <= '1' when state = IDLE_ST and sd_clk_d = '0' and sd_clk_i = '1' and
                  (sd_cmd_in_i = '1' or sd_cmd_in_i = 'H') else
                  '0';

   fsm_proc : process (clk_i)
   begin
      if rising_edge(clk_i) then
         if resp_ready_i = '1' then
            resp_valid_o <= '0';
         end if;

         sd_clk_d <= sd_clk_i;
         if sd_clk_d = '0' and sd_clk_i = '1' then                                                        -- Rising edge of sd_clk_i

            case state is

               when INIT_ST =>
                  if idle_count > 0 then
                     idle_count <= idle_count - 1;
                  elsif resp_valid_o = '0' then
                     state <= IDLE_ST;
                  end if;

               when IDLE_ST =>
                  if cmd_valid_i = '1' and cmd_ready_o = '1' then
                     resp_timeout_o <= '0';
                     resp_error_o   <= '0';
                     send_data      <= "01" & std_logic_vector(to_unsigned(cmd_index_i, 6)) & cmd_data_i;
                     send_count     <= 39;                                                                -- Commands are always 40 bits excluding CRC
                     resp_count     <= cmd_resp_i;                                                        -- Store expected number of response bits
                     crc            <= (others => '0');
                     state          <= WRITING_ST;
                  end if;

               when WRITING_ST =>
                  if send_count > 0 then
                     send_data  <= send_data(38 downto 0) & "0";
                     send_count <= send_count - 1;
                     crc        <= new_crc(crc, send_data(39));
                  else
                     crc                     <= new_crc(crc, send_data(39));
                     send_data(39 downto 32) <= new_crc(crc, send_data(39)) & "1";
                     send_count              <= 7;
                     state                   <= SEND_CRC_ST;
                  end if;

               when SEND_CRC_ST =>
                  if send_count > 0 then
                     send_data  <= send_data(38 downto 0) & "0";
                     send_count <= send_count - 1;
                  else
                     if resp_count > 0 then
                        resp_count    <= resp_count - 1;
                        crc           <= (others => '0');
                        timeout_count <= cmd_timeout_i;
                        state         <= WAIT_RESPONSE_ST;
                     else
                        cooldown_count <= C_COOLDOWN_MAX;
                        state          <= COOLDOWN_ST;
                     end if;
                  end if;

               when WAIT_RESPONSE_ST =>
                  if sd_cmd_in_i = '0' then
                     resp_data <= (others => '0');
                     state     <= GET_RESPONSE_ST;
                  elsif timeout_count > 0 then
                     timeout_count <= timeout_count - 1;
                  else
                     resp_timeout_o <= '1';
                     resp_valid_o   <= '1';
                     cooldown_count <= C_COOLDOWN_MAX;
                     state          <= COOLDOWN_ST;
                  end if;

               when GET_RESPONSE_ST =>
                  if resp_count > 8 then
                     crc <= new_crc(crc, to_01(sd_cmd_in_i));
                  end if;

                  -- This is an ugly hack because the CRC of the R3 (CID) does not cover the first eight bits.
                  if resp_count = 129 then
                     crc <= (others => '0');
                  end if;

                  if resp_count > 0 then
                     resp_data  <= resp_data(142 downto 0) & to_01(sd_cmd_in_i);
                     resp_count <= resp_count - 1;
                  else
                     if resp_data(7 downto 0) = crc & "1" or resp_data(7 downto 0) = X"FF" then
                        resp_data_o  <= resp_data(143 downto 8);
                        resp_valid_o <= '1';
                        report "Received response 0x" & to_hstring(resp_data(39 downto 8))
                               & " with valid CRC";
                     else
                        resp_error_o <= '1';
                        resp_valid_o <= '1';
                        report "CRC error";
                     end if;
                     cooldown_count <= C_COOLDOWN_MAX;
                     state          <= COOLDOWN_ST;
                  end if;

               when COOLDOWN_ST =>
                  if cooldown_count > 0 then
                     cooldown_count <= cooldown_count - 1;
                  else
                     state <= IDLE_ST;
                  end if;

            end case;

         end if;

         if rst_i = '1' then
            idle_count     <= C_IDLE_MAX;
            state          <= INIT_ST;
            resp_valid_o   <= '0';
            resp_timeout_o <= '0';
            resp_error_o   <= '0';
         end if;
      end if;
   end process fsm_proc;


   -- The SDCard samples on rising edge of sd_clk.
   -- Output is changed one system clock (20 ns) after rising edge of sd_clk.
   sd_cmd_proc : process (all)
   begin
      -- Default is to let CMD float high (internal pull up resistor)
      sd_cmd_out_o  <= '1';
      sd_cmd_oe_n_o <= '1';

      case state is

         when WRITING_ST | SEND_CRC_ST =>
            sd_cmd_out_o  <= send_data(39);
            sd_cmd_oe_n_o <= '0';

         when others =>
            sd_cmd_out_o  <= '1';
            sd_cmd_oe_n_o <= '1';

      end case;

      null;
   end process sd_cmd_proc;

end architecture synthesis;

