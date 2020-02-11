library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity foo2 is
  generic ( TDATA_WIDTH : integer := 8);
  port (
    clk        : in std_logic;
    wr_field_0 : in std_logic_vector(TDATA_WIDTH - 1 downto 0);
    wr_field_1 : in std_logic;
    wr_en      : in std_logic;

    rd_field_0 : out std_logic_vector(TDATA_WIDTH - 1 downto 0);
    rd_field_1 : out std_logic
  );
end foo2;

architecture foo of foo2 is

  type data_array_t is array (3 downto 0) of std_logic_vector(TDATA_WIDTH downto 0);
  signal data_buffer : data_array_t;

  signal addr : unsigned(3 downto 0) := (others => '0');

begin

  rd_field_0 <= data_buffer(to_integer(addr))(TDATA_WIDTH - 1 downto 0);
  rd_field_1 <= data_buffer(to_integer(addr))(TDATA_WIDTH);

  process(clk)
  begin
    if rising_edge(clk) then
      addr <= addr + 1;

      if wr_en = '1' then
        data_buffer(to_integer(addr)) <= wr_field_0 & wr_field_1;
      end if;

    end if;
  end process;

end foo;
