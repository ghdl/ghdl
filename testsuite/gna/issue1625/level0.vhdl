library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std_unsigned.all;

entity level0 is
end entity level0;

architecture synthesis of level0 is

   signal clk       : std_logic;
   signal rst       : std_logic;
   signal src_reg   : std_logic_vector(3 downto 0);
   signal src_val   : std_logic_vector(15 downto 0);
   signal dst_reg   : std_logic_vector(3 downto 0);
   signal dst_val   : std_logic_vector(15 downto 0);
   signal flags_out : std_logic_vector(15 downto 0);
   signal flags_we  : std_logic;
   signal flags_in  : std_logic_vector(15 downto 0); 
   signal reg_we    : std_logic;
   signal reg_addr  : std_logic_vector(3 downto 0);
   signal reg_val   : std_logic_vector(15 downto 0);

begin

   inst : entity work.level1
      port map (
         clk_i       => clk,
         rst_i       => rst,
         src_reg_i   => src_reg,
         src_val_o   => src_val,
         dst_reg_i   => dst_reg,
         dst_val_o   => dst_val,
         flags_o     => flags_out,
         flags_we_i  => flags_we,
         flags_i     => flags_in,
         reg_we_i    => reg_we,
         reg_addr_i  => reg_addr,
         reg_val_i   => reg_val
      );

end architecture synthesis;
