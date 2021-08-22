library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std_unsigned.all;

entity level1 is
   port (
      clk_i      : in  std_logic;
      rst_i      : in  std_logic;
      src_reg_i  : in  std_logic_vector(3 downto 0);
      src_val_o  : out std_logic_vector(15 downto 0);
      dst_reg_i  : in  std_logic_vector(3 downto 0);
      dst_val_o  : out std_logic_vector(15 downto 0);
      flags_o    : out std_logic_vector(15 downto 0);
      flags_we_i : in  std_logic;
      flags_i    : in  std_logic_vector(15 downto 0);
      reg_we_i   : in  std_logic;
      reg_addr_i : in  std_logic_vector(3 downto 0);
      reg_val_i  : in  std_logic_vector(15 downto 0)
   );
end entity level1;

architecture synthesis of level1 is

   signal sr : std_logic_vector(15 downto 0);
   signal dst_val_lower : std_logic_vector(15 downto 0);

begin

   inst : entity work.level2
      generic map (
         G_ADDR_SIZE => 11,
         G_DATA_SIZE => 16
      )
      port map (
         clk_i     => clk_i,
         rst_i     => rst_i,
         rd_addr_i => sr(15 downto 8) & dst_reg_i(2 downto 0),
         rd_data_o => dst_val_lower,
         wr_addr_i => sr(15 downto 8) & reg_addr_i(2 downto 0),
         wr_data_i => reg_val_i,
         wr_en_i   => reg_we_i and not reg_addr_i(3)
      ); -- i_ram_lower_dst

end architecture synthesis;
