-- This module is a simple kind of RAM test.
--
-- It generates first a sequence of WRITE operations (writing pseudo-random data),
-- and then a corresponding sequence of READ operations, verifying that the
-- correct values are read back again.
--
-- Created by Michael Jørgensen in 2022 (mjoergen.github.io/HyperRAM).

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity avm_master is
   generic (
      G_ADDRESS_SIZE : integer; -- Number of bits
      G_DATA_SIZE    : integer  -- Number of bits
   );
   port (
      clk_i               : in  std_logic;
      rst_i               : in  std_logic;
      start_i             : in  std_logic;
      wait_o              : out std_logic;
      write_burstcount_i  : in  std_logic_vector(7 downto 0);
      read_burstcount_i   : in  std_logic_vector(7 downto 0);

      avm_write_o         : out std_logic;
      avm_read_o          : out std_logic;
      avm_address_o       : out std_logic_vector(G_ADDRESS_SIZE-1 downto 0);
      avm_writedata_o     : out std_logic_vector(G_DATA_SIZE-1 downto 0);
      avm_byteenable_o    : out std_logic_vector(G_DATA_SIZE/8-1 downto 0);
      avm_burstcount_o    : out std_logic_vector(7 downto 0);
      avm_readdata_i      : in  std_logic_vector(G_DATA_SIZE-1 downto 0);
      avm_readdatavalid_i : in  std_logic;
      avm_waitrequest_i   : in  std_logic;
      -- Debug output
      address_o           : out std_logic_vector(G_ADDRESS_SIZE-1 downto 0);
      data_exp_o          : out std_logic_vector(G_DATA_SIZE-1 downto 0);
      data_read_o         : out std_logic_vector(G_DATA_SIZE-1 downto 0);
      error_o             : out std_logic
   );
end entity avm_master;

architecture synthesis of avm_master is

   constant C_DATA_INIT    : std_logic_vector(63 downto 0) := X"CAFEBABEDEADBEEF";

   signal data_init        : std_logic_vector(63 downto 0);

   signal wr_data          : std_logic_vector(63 downto 0);
   signal rd_data          : std_logic_vector(63 downto 0);
   signal burstcount       : std_logic_vector(7 downto 0);
   signal read_burstcount  : std_logic_vector(7 downto 0);
   signal wordcount        : integer range 0 to 255;
   signal new_address      : std_logic_vector(G_ADDRESS_SIZE-1 downto 0);
   signal new_burstcount   : std_logic_vector(7 downto 0);

   type state_t is (
      INIT_ST,
      WRITING_ST,
      READING_ST
   );

   signal state  : state_t := INIT_ST;
   signal reset_verify : std_logic;

   -- The pseudo-random data is generated using a 64-bit maximal-period Galois LFSR,
   -- see http://users.ece.cmu.edu/~koopman/lfsr/64.txt
   function lfsr (constant old : std_logic_vector(63 downto 0)) return std_logic_vector is
   begin
      if old(63) = '1' then
         return (old(62 downto 0) & "0") xor x"000000000000001b";
      else
         return (old(62 downto 0) & "0");
      end if;
   end function lfsr;

begin

   new_address    <= avm_address_o when unsigned(avm_burstcount_o) > 1 else
                     std_logic_vector(unsigned(avm_address_o) + wordcount);
   new_burstcount <= std_logic_vector(unsigned(avm_burstcount_o) - 1) when unsigned(avm_burstcount_o) > 1 else
                     burstcount;

   p_verifier : process (clk_i)
   begin
      if rising_edge(clk_i) then
         if avm_readdatavalid_i = '1' then
            data_read_o <= avm_readdata_i;
            data_exp_o  <= rd_data(G_DATA_SIZE-1 downto 0);

            if avm_readdata_i /= rd_data(G_DATA_SIZE-1 downto 0) then
               report "ERROR: Expected " & to_hstring(rd_data(G_DATA_SIZE-1 downto 0)) & ", read " & to_hstring(avm_readdata_i)
                  severity failure;
               error_o <= '1';
            else
               rd_data <= lfsr(rd_data);
            end if;
         end if;

         if reset_verify = '1' then
            rd_data    <= data_init;
            error_o    <= '0';
         end if;

         if rst_i = '1' then
            error_o <= '0';
         end if;
      end if;
   end process p_verifier;

   p_fsm : process (clk_i)
   begin
      if rising_edge(clk_i) then
         reset_verify  <= '0';

         if avm_waitrequest_i = '0' then
            avm_write_o <= '0';
            avm_read_o  <= '0';
         end if;

         case state is
            when INIT_ST =>
               if start_i = '1' then
                  wait_o           <= '1';
                  wr_data          <= C_DATA_INIT xor (
                                      write_burstcount_i & read_burstcount_i &
                                      write_burstcount_i & read_burstcount_i &
                                      write_burstcount_i & read_burstcount_i &
                                      write_burstcount_i & read_burstcount_i);
                  data_init        <= C_DATA_INIT xor (
                                      write_burstcount_i & read_burstcount_i &
                                      write_burstcount_i & read_burstcount_i &
                                      write_burstcount_i & read_burstcount_i &
                                      write_burstcount_i & read_burstcount_i);
                  avm_write_o      <= '1';
                  avm_read_o       <= '0';
                  avm_address_o    <= (others => '0');
                  avm_byteenable_o <= (others => '1');
                  avm_burstcount_o <= write_burstcount_i;
                  burstcount       <= write_burstcount_i;
                  read_burstcount  <= read_burstcount_i;
                  wordcount        <= to_integer(unsigned(write_burstcount_i));
                  state            <= WRITING_ST;
               end if;

            when WRITING_ST =>
               if avm_waitrequest_i = '0' then
                  avm_write_o      <= '1';
                  avm_read_o       <= '0';
                  avm_address_o    <= new_address;
                  avm_byteenable_o <= (others => '1');
                  avm_burstcount_o <= new_burstcount;

                  wr_data <= lfsr(wr_data);

                  if signed(avm_address_o) = -wordcount and unsigned(avm_burstcount_o) = 1 then
                     wr_data       <= data_init;
                     avm_write_o   <= '0';
                     avm_address_o <= (others => '0');
                     avm_read_o    <= '1';
                     avm_burstcount_o <= read_burstcount;
                     burstcount       <= read_burstcount;
                     wordcount        <= to_integer(unsigned(read_burstcount));
                     data_read_o   <= (others => '0');
                     data_exp_o    <= (others => '0');
                     reset_verify  <= '1';
                     state         <= READING_ST;
                  end if;
               end if;

            when READING_ST =>
               if avm_waitrequest_i = '0' then

                  if signed(avm_address_o) = -wordcount then
                     wait_o <= '0';
                     state  <= INIT_ST;
                  else
                     avm_address_o <= std_logic_vector(unsigned(avm_address_o) + wordcount);
                     avm_read_o    <= '1';
                  end if;
               end if;

         end case;

         if rst_i = '1' then
            avm_write_o <= '0';
            avm_read_o  <= '0';
            wait_o      <= '0';
            state       <= INIT_ST;
         end if;
      end if;
   end process p_fsm;

   avm_writedata_o <= wr_data(G_DATA_SIZE-1 downto 0);
   address_o <= avm_address_o;

end architecture synthesis;

