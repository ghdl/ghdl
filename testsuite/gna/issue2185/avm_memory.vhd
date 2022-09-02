library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity avm_memory is
   generic (
      G_ADDRESS_SIZE : integer; -- Number of bits
      G_DATA_SIZE    : integer  -- Number of bits
   );
   port (
      clk_i               : in  std_logic;
      rst_i               : in  std_logic;
      avm_write_i         : in  std_logic;
      avm_read_i          : in  std_logic;
      avm_address_i       : in  std_logic_vector(G_ADDRESS_SIZE-1 downto 0);
      avm_writedata_i     : in  std_logic_vector(G_DATA_SIZE-1 downto 0);
      avm_byteenable_i    : in  std_logic_vector(G_DATA_SIZE/8-1 downto 0);
      avm_burstcount_i    : in  std_logic_vector(7 downto 0);
      avm_readdata_o      : out std_logic_vector(G_DATA_SIZE-1 downto 0);
      avm_readdatavalid_o : out std_logic;
      avm_waitrequest_o   : out std_logic
   );
end entity avm_memory;

architecture simulation of avm_memory is

   -- This defines a type containing an array of bytes
   type mem_t is array (0 to 2**G_ADDRESS_SIZE-1) of std_logic_vector(G_DATA_SIZE-1 downto 0);

   signal write_burstcount     : std_logic_vector(7 downto 0);
   signal write_address        : std_logic_vector(G_ADDRESS_SIZE-1 downto 0);

   signal read_burstcount      : std_logic_vector(7 downto 0);
   signal read_address         : std_logic_vector(G_ADDRESS_SIZE-1 downto 0);

   signal mem_write_burstcount : std_logic_vector(7 downto 0);
   signal mem_read_burstcount  : std_logic_vector(7 downto 0);
   signal mem_write_address    : std_logic_vector(G_ADDRESS_SIZE-1 downto 0);
   signal mem_read_address     : std_logic_vector(G_ADDRESS_SIZE-1 downto 0);

begin

   mem_write_address    <= avm_address_i    when write_burstcount = X"00" else write_address;
   mem_read_address     <= avm_address_i    when read_burstcount = X"00" else read_address;
   mem_write_burstcount <= avm_burstcount_i when write_burstcount = X"00" else write_burstcount;
   mem_read_burstcount  <= avm_burstcount_i when read_burstcount = X"00" else read_burstcount;

   avm_waitrequest_o <= '0' when unsigned(read_burstcount) = 0 else '1';

   p_mem : process (clk_i)
      variable mem : mem_t;
   begin
      if rising_edge(clk_i) then
         avm_readdatavalid_o <= '0';

         if avm_write_i = '1' and avm_waitrequest_o = '0' then
            write_address <= std_logic_vector(unsigned(mem_write_address) + 1);
            write_burstcount <= std_logic_vector(unsigned(mem_write_burstcount) - 1);

            report "Writing 0x" & to_hstring(avm_writedata_i) & " to 0x" & to_hstring(mem_write_address) &
                   " with burstcount " & to_hstring(write_burstcount);
            for b in 0 to G_DATA_SIZE/8-1 loop
               if avm_byteenable_i(b) = '1' then
                  mem(to_integer(unsigned(mem_write_address)))(8*b+7 downto 8*b) := avm_writedata_i(8*b+7 downto 8*b);
               end if;
            end loop;
         end if;

         if (avm_read_i = '1' and avm_waitrequest_o = '0') or to_integer(unsigned(read_burstcount)) > 0 then
            read_address <= std_logic_vector(unsigned(mem_read_address) + 1);
            read_burstcount <= std_logic_vector(unsigned(mem_read_burstcount) - 1);

            avm_readdata_o <= mem(to_integer(unsigned(mem_read_address)));
            avm_readdatavalid_o <= '1';

            report "Reading 0x" & to_hstring(mem(to_integer(unsigned(mem_read_address)))) & " from 0x" & to_hstring(mem_read_address) &
                   " with burstcount " & to_hstring(read_burstcount);
         end if;

         if rst_i = '1' then
            write_burstcount <= (others => '0');
            read_burstcount  <= (others => '0');
         end if;
      end if;
   end process p_mem;

end architecture simulation;

