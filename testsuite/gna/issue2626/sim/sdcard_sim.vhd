-- Simulation model of SD card

library ieee;
   use ieee.std_logic_1164.all;
   use ieee.numeric_std.all;

entity sdcard_sim is
   port (
      sd_clk_i   : in    std_logic;
      sd_cmd_io  : inout std_logic;
      sd_dat_io  : inout std_logic_vector(3 downto 0);

      mem_addr_o : out   std_logic_vector(31 downto 0);
      mem_data_o : out   std_logic_vector(7 downto 0);
      mem_data_i : in    std_logic_vector(7 downto 0);
      mem_we_o   : out   std_logic
   );
end entity sdcard_sim;

architecture simulation of sdcard_sim is

   signal   cmd           : unsigned(47 downto 0);
   signal   cmd_valid     : boolean;
   signal   cmd_receiving : boolean;
   signal   cmd_bit_count : natural range 0 to 48;

   signal   resp_data : unsigned(47 downto 0);

   signal   card_state         : std_logic_vector(3 downto 0);
   constant C_CARD_STATE_IDLE  : std_logic_vector(3 downto 0) := "0000";
   constant C_CARD_STATE_READY : std_logic_vector(3 downto 0) := "0001";
   constant C_CARD_STATE_IDENT : std_logic_vector(3 downto 0) := "0010";
   constant C_CARD_STATE_STBY  : std_logic_vector(3 downto 0) := "0011";
   constant C_CARD_STATE_TRAN  : std_logic_vector(3 downto 0) := "0100";
   constant C_CARD_STATE_DATA  : std_logic_vector(3 downto 0) := "0101";
   constant C_CARD_STATE_RCV   : std_logic_vector(3 downto 0) := "0110";
   constant C_CARD_STATE_PRG   : std_logic_vector(3 downto 0) := "0111";
   constant C_CARD_STATE_DIS   : std_logic_vector(3 downto 0) := "1000";

   pure function calc_crc (
      arg : unsigned
   ) return unsigned is
      constant C_POLYNOMIAL : unsigned(6 downto 0) := "0001001";
      variable crc_v        : unsigned(6 downto 0);
   begin
      crc_v := (others => '0');

      for i in arg'range loop
         if to_01(arg(i)) /= crc_v(6) then
            crc_v := (crc_v(5 downto 0) & "0") xor C_POLYNOMIAL;
         else
            crc_v := (crc_v(5 downto 0) & "0");
         end if;
      end loop;

      return crc_v;
   end function calc_crc;

   pure function crc_valid (
      arg : unsigned
   ) return boolean is
   begin
      return arg(7 downto 0) = calc_crc(arg(arg'left downto 8)) & "1";
   end function crc_valid;

   pure function append_crc (
      arg : unsigned
   ) return unsigned is
   begin
      return arg & calc_crc(arg) & "1";
   end function append_crc;

begin

   -- TBD
   mem_addr_o <= (others => '0');
   mem_data_o <= (others => '0');
   mem_we_o   <= '0';

   sd_cmd_io  <= 'H';
   sd_dat_io  <= (others => 'H');

   cmd_proc : process (sd_clk_i)
   begin
      if rising_edge(sd_clk_i) then
         cmd_valid <= false;

         if cmd_receiving = false then
            -- Wait for a start bit.
            if to_01(sd_cmd_io) = '0' then
               cmd_receiving <= true;
               cmd           <= (others => '0');
               cmd_bit_count <= 1;
            end if;
         else
            if cmd_bit_count <= 47 then
               cmd           <= cmd(46 downto 0) & to_01(sd_cmd_io);
               cmd_bit_count <= cmd_bit_count + 1;
            else
               if crc_valid(cmd) then
                  cmd_valid <= true;
               end if;
               cmd_receiving <= false;
            end if;
         end if;
      end if;
   end process cmd_proc;

   fsm_proc : process (sd_clk_i)
   begin
      if rising_edge(sd_clk_i) then
         -- Send back response
         sd_cmd_io <= resp_data(47);
         resp_data <= resp_data(46 downto 0) & "H";

         if cmd_valid and to_01(cmd(47 downto 46)) = "01" then

            case to_integer(cmd(45 downto 40)) is

               when 0 =>                                             -- GO_IDLE
                  card_state <= C_CARD_STATE_IDLE;

               when 8 =>                                             -- SEND_IF_COND
                  resp_data <= append_crc(X"08" & cmd(39 downto 8));

               when others =>
                  null;

            end case;

         end if;
      end if;
   end process fsm_proc;

end architecture simulation;

