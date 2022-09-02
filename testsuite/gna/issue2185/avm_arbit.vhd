library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.numeric_std_unsigned.all;

-- This arbitrates fairly between two Masters connected to a single Slave

entity avm_arbit is
   generic (
      G_ADDRESS_SIZE  : integer;
      G_DATA_SIZE     : integer
   );
   port (
      clk_i                  : in  std_logic;
      rst_i                  : in  std_logic;

      -- Slave interface 0 (input)
      s0_avm_write_i         : in  std_logic;
      s0_avm_read_i          : in  std_logic;
      s0_avm_address_i       : in  std_logic_vector(G_ADDRESS_SIZE-1 downto 0);
      s0_avm_writedata_i     : in  std_logic_vector(G_DATA_SIZE-1 downto 0);
      s0_avm_byteenable_i    : in  std_logic_vector(G_DATA_SIZE/8-1 downto 0);
      s0_avm_burstcount_i    : in  std_logic_vector(7 downto 0);
      s0_avm_readdata_o      : out std_logic_vector(G_DATA_SIZE-1 downto 0);
      s0_avm_readdatavalid_o : out std_logic;
      s0_avm_waitrequest_o   : out std_logic;

      -- Slave interface 1 (input)
      s1_avm_write_i         : in  std_logic;
      s1_avm_read_i          : in  std_logic;
      s1_avm_address_i       : in  std_logic_vector(G_ADDRESS_SIZE-1 downto 0);
      s1_avm_writedata_i     : in  std_logic_vector(G_DATA_SIZE-1 downto 0);
      s1_avm_byteenable_i    : in  std_logic_vector(G_DATA_SIZE/8-1 downto 0);
      s1_avm_burstcount_i    : in  std_logic_vector(7 downto 0);
      s1_avm_readdata_o      : out std_logic_vector(G_DATA_SIZE-1 downto 0);
      s1_avm_readdatavalid_o : out std_logic;
      s1_avm_waitrequest_o   : out std_logic;

      -- Master interface (output)
      m_avm_write_o          : out std_logic;
      m_avm_read_o           : out std_logic;
      m_avm_address_o        : out std_logic_vector(G_ADDRESS_SIZE-1 downto 0);
      m_avm_writedata_o      : out std_logic_vector(G_DATA_SIZE-1 downto 0);
      m_avm_byteenable_o     : out std_logic_vector(G_DATA_SIZE/8-1 downto 0);
      m_avm_burstcount_o     : out std_logic_vector(7 downto 0);
      m_avm_readdata_i       : in  std_logic_vector(G_DATA_SIZE-1 downto 0);
      m_avm_readdatavalid_i  : in  std_logic;
      m_avm_waitrequest_i    : in  std_logic
   );
end entity avm_arbit;

architecture synthesis of avm_arbit is

   signal s0_active_req : std_logic;
   signal s1_active_req : std_logic;

   signal s0_active_grant : std_logic := '0';
   signal s1_active_grant : std_logic := '0';
   signal active_grants   : std_logic_vector(1 downto 0);

   signal last_grant : std_logic := '0';

   signal burstcount : std_logic_vector(7 downto 0);

begin

   s0_avm_waitrequest_o <= not s0_active_grant;
   s1_avm_waitrequest_o <= not s1_active_grant;

   s0_active_req <= s0_avm_write_i or s0_avm_read_i;
   s1_active_req <= s1_avm_write_i or s1_avm_read_i;

   p_burstcount : process (clk_i)
   begin
      if rising_edge(clk_i) then
         if burstcount = X"00" then
            if s0_avm_write_i and not s0_avm_waitrequest_o then
               burstcount <= s0_avm_burstcount_i;
            end if;
            if s1_avm_write_i and not s1_avm_waitrequest_o then
               burstcount <= s1_avm_burstcount_i;
            end if;
         else
            if (s0_avm_write_i and not s0_avm_waitrequest_o) or
               s0_avm_readdatavalid_o or
               (s1_avm_write_i and not s1_avm_waitrequest_o) or
               s1_avm_readdatavalid_o then
               burstcount <= burstcount - 1;
            end if;
         end if;

         if rst_i = '1' then
            burstcount <= X"00";
         end if;
      end if;
   end process p_burstcount;

   active_grants <= s0_active_grant & s1_active_grant;

   p_grant : process (clk_i)
   begin
      if rising_edge(clk_i) then
         case active_grants is
            when "00" =>
               if s0_active_req = '1' and last_grant = '1' then
                  s0_active_grant <= '1';
                  last_grant <= '1';
               end if;
               if s1_active_req = '1' and last_grant = '0' then
                  s1_active_grant <= '1';
                  last_grant <= '0';
               end if;

            when "01" =>
               if burstcount = X"01" then
                  if (s1_avm_write_i and not s1_avm_waitrequest_o) or
                     s1_avm_readdatavalid_o then
                        s1_active_grant <= '0';
                  end if;
               end if;
            when "10" =>
               if burstcount = X"01" then
                  if (s0_avm_write_i and not s0_avm_waitrequest_o) or
                     s0_avm_readdatavalid_o then
                        s0_active_grant <= '0';
                  end if;
               end if;

            when others =>
               report "S0 and S1 both active"
                  severity failure;
         end case;

         if rst_i = '1' then
            s0_active_grant <= '0';
            s1_active_grant <= '0';
            last_grant      <= '0';
         end if;
      end if;
   end process p_grant;

   m_avm_write_o      <= s0_avm_write_i      when last_grant = '0' else s1_avm_write_i;
   m_avm_read_o       <= s0_avm_read_i       when last_grant = '0' else s1_avm_read_i;
   m_avm_address_o    <= s0_avm_address_i    when last_grant = '0' else s1_avm_address_i;
   m_avm_writedata_o  <= s0_avm_writedata_i  when last_grant = '0' else s1_avm_writedata_i;
   m_avm_byteenable_o <= s0_avm_byteenable_i when last_grant = '0' else s1_avm_byteenable_i;
   m_avm_burstcount_o <= s0_avm_burstcount_i when last_grant = '0' else s1_avm_burstcount_i;

   s0_avm_readdata_o      <= m_avm_readdata_i;
   s0_avm_readdatavalid_o <= m_avm_readdatavalid_i when last_grant = '0' else '0';

   s1_avm_readdata_o      <= m_avm_readdata_i;
   s1_avm_readdatavalid_o <= m_avm_readdatavalid_i when last_grant = '1' else '0';

end architecture synthesis;

