library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity top is
  generic (
    data_width : natural := 4
  );
  port (
    clk_i         : in    std_logic;
    rst_i         : in    std_logic;
    data_i        : in    std_logic_vector(data_width - 1 downto 0);
    shift_value_i : in    integer;
    data_o        : out   std_logic_vector(data_width - 1 downto 0)
  );
end entity top;

architecture rtl of top is

  signal data, next_data : std_logic_vector(data_width - 1 downto 0);

begin

  reg_proc : process (clk_i, rst_i) is
  begin

    if (rst_i = '1') then
      data <= (others => '0');
    elsif rising_edge(clk_i) then
      data <= next_data;
    end if;

  end process reg_proc;

  next_data <= std_logic_vector(shift_right(signed(data), shift_value_i));

  data_o <= data;

end architecture rtl;

