library ieee;
  use ieee.std_logic_1164.all;

entity calculate_crc_bits is
  generic (width_p     : integer := 4);
  port (
    clk          : in    std_logic; -- Clock signal
    rst          : in    std_logic; -- Reset signal
    input_valid  : in    std_logic; -- Input valid signal
    g_max        : in    std_logic_vector(width_p - 1 downto 0);
    p            : out   std_logic_vector(width_p - 1 downto 0) 
  );
end entity calculate_crc_bits;

architecture behavioral of calculate_crc_bits is
begin
  crc_calc_proc : process (clk, rst) is
    variable p_hold            : std_logic_vector(width_p - 1 downto 0);
  begin
    if (rst = '1') then
      p <= (others => '0');
    elsif rising_edge(clk) then
      p_hold := p;
      if input_valid = '1' then
        p_hold := g_max;
      end if;
      p <= p_hold;
    end if;
  end process crc_calc_proc;
end architecture behavioral;

