-- This module implements a very simple read cache with just one cache line.
--
-- Created by Michael Jørgensen in 2022 (mjoergen.github.io/HyperRAM).

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.numeric_std_unsigned.all;

entity avm_cache is
   generic (
      G_CACHE_SIZE   : integer;
      G_ADDRESS_SIZE : integer; -- Number of bits
      G_DATA_SIZE    : integer  -- Number of bits
   );
   port (
      clk_i                 : in  std_logic;
      rst_i                 : in  std_logic;
      s_avm_waitrequest_o   : out std_logic;
      s_avm_write_i         : in  std_logic;
      s_avm_read_i          : in  std_logic;
      s_avm_address_i       : in  std_logic_vector(G_ADDRESS_SIZE-1 downto 0);
      s_avm_writedata_i     : in  std_logic_vector(G_DATA_SIZE-1 downto 0);
      s_avm_byteenable_i    : in  std_logic_vector(G_DATA_SIZE/8-1 downto 0);
      s_avm_burstcount_i    : in  std_logic_vector(7 downto 0);
      s_avm_readdata_o      : out std_logic_vector(G_DATA_SIZE-1 downto 0);
      s_avm_readdatavalid_o : out std_logic;
      m_avm_waitrequest_i   : in  std_logic;
      m_avm_write_o         : out std_logic;
      m_avm_read_o          : out std_logic;
      m_avm_address_o       : out std_logic_vector(G_ADDRESS_SIZE-1 downto 0);
      m_avm_writedata_o     : out std_logic_vector(G_DATA_SIZE-1 downto 0);
      m_avm_byteenable_o    : out std_logic_vector(G_DATA_SIZE/8-1 downto 0);
      m_avm_burstcount_o    : out std_logic_vector(7 downto 0);
      m_avm_readdata_i      : in  std_logic_vector(G_DATA_SIZE-1 downto 0);
      m_avm_readdatavalid_i : in  std_logic
   );
end entity avm_cache;

architecture synthesis of avm_cache is

   type mem_t is array (0 to G_CACHE_SIZE-1) of std_logic_vector(G_DATA_SIZE-1 downto 0);
   type t_state is (IDLE_ST, READING_ST);

   -- Registers
   signal cache_data    : mem_t;
   signal cache_addr    : std_logic_vector(G_ADDRESS_SIZE-1 downto 0);
   signal cache_valid   : std_logic;
   signal cache_count   : natural range 0 to G_CACHE_SIZE;
   signal rd_burstcount : std_logic_vector(7 downto 0);
   signal state         : t_state := IDLE_ST;

   -- Combinatorial
   signal cache_offset_s : std_logic_vector(G_ADDRESS_SIZE-1 downto 0);

begin

   s_avm_waitrequest_o <= m_avm_waitrequest_i when state = IDLE_ST else
                          '1' when rd_burstcount /= X"00" else
                          '0' when (cache_valid = '1' or cache_offset_s < cache_count) and s_avm_read_i = '1' and s_avm_burstcount_i = X"01" else
                          '1';

   -- Two's complement, i.e. wrap-around
   cache_offset_s <= std_logic_vector(unsigned(s_avm_address_i) - unsigned(cache_addr));

   p_fsm : process (clk_i)
   begin
      if rising_edge(clk_i) then
         s_avm_readdata_o      <= (others => '0');
         s_avm_readdatavalid_o <= '0';

         if m_avm_waitrequest_i = '0' then
            m_avm_write_o      <= '0';
            m_avm_read_o       <= '0';
            m_avm_address_o    <= (others => '0');
            m_avm_writedata_o  <= (others => '0');
            m_avm_byteenable_o <= (others => '0');
            m_avm_burstcount_o <= (others => '0');
         end if;

         case state is
            when IDLE_ST =>
               assert not (s_avm_write_i = '1' and s_avm_read_i = '1');

               if s_avm_write_i = '1' and s_avm_waitrequest_o = '0' then
                  m_avm_write_o      <= s_avm_write_i;
                  m_avm_read_o       <= s_avm_read_i;
                  m_avm_address_o    <= s_avm_address_i;
                  m_avm_writedata_o  <= s_avm_writedata_i;
                  m_avm_byteenable_o <= s_avm_byteenable_i;
                  m_avm_burstcount_o <= s_avm_burstcount_i;
                  if cache_offset_s < G_CACHE_SIZE then
                     cache_valid <= '0';
                     cache_count <= to_integer(cache_offset_s);
                     cache_data(to_integer(cache_offset_s)) <= s_avm_writedata_i;
                  end if;
                  state              <= IDLE_ST;
               end if;

               if s_avm_read_i = '1' and s_avm_waitrequest_o = '0' then
                  if (cache_valid = '1' or cache_offset_s < cache_count) and cache_offset_s < G_CACHE_SIZE and s_avm_burstcount_i = X"01" then
                     s_avm_readdata_o      <= cache_data(to_integer(cache_offset_s));
                     s_avm_readdatavalid_o <= '1';
                  else
                     m_avm_write_o      <= '0';
                     m_avm_read_o       <= '1';
                     m_avm_address_o    <= s_avm_address_i;
                     m_avm_burstcount_o <= to_stdlogicvector(G_CACHE_SIZE, 8);
                     rd_burstcount      <= s_avm_burstcount_i;
                     cache_valid        <= '0';
                     cache_count        <= 0;
                     cache_addr         <= s_avm_address_i;
                     state              <= READING_ST;
                  end if;
               end if;

            -- Reading data into cache
            when READING_ST =>
               if m_avm_readdatavalid_i = '1' then
                  cache_data(cache_count) <= m_avm_readdata_i;
                  if rd_burstcount /= 0 then
                     s_avm_readdata_o        <= m_avm_readdata_i;
                     s_avm_readdatavalid_o   <= '1';
                     rd_burstcount           <= std_logic_vector(unsigned(rd_burstcount) - 1);
                  end if;

                  if cache_count >= G_CACHE_SIZE-1 then
                     cache_count <= G_CACHE_SIZE;
                     cache_valid <= '1';
                     state       <= IDLE_ST;
                  else
                     cache_count <= cache_count + 1;
                  end if;
               end if;

               if s_avm_waitrequest_o = '0' and s_avm_read_i = '1' and s_avm_burstcount_i = X"01" then
                  s_avm_readdata_o      <= cache_data(to_integer(cache_offset_s));
                  s_avm_readdatavalid_o <= '1';
               end if;

            when others =>
               null;

         end case;

         if rst_i = '1' then
            s_avm_readdatavalid_o <= '0';
            m_avm_write_o         <= '0';
            m_avm_read_o          <= '0';
            cache_valid           <= '0';
            cache_count           <= 0;
            state                 <= IDLE_ST;
         end if;
      end if;
   end process p_fsm;

end architecture synthesis;

