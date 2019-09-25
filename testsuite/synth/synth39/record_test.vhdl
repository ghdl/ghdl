library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity record_test is
  port (
    clk : in std_logic;

    sl_in : in std_logic;
    slv_in : in std_logic_vector(7 downto 0);
    int_in : in integer range 0 to 15;
    usig_in : in unsigned(7 downto 0);

    sl_out : out std_logic;
    slv_out : out std_logic_vector(7 downto 0);
    int_out : out integer range 0 to 15;
    usig_out : out unsigned(7 downto 0)
  );
end record_test;

architecture rtl of record_test is
  type t_record is record
    sl   : std_logic;
    slv  : std_logic_vector(7 downto 0);
    int  : integer range 0 to 15;
    usig : unsigned(7 downto 0);
  end record t_record;
  signal sample_record : t_record := ('0', (others => '0'), 0, (others => '0'));
begin
  process(clk)
  begin
    if rising_edge(clk) then
      sample_record.sl <= sl_in;
      sample_record.slv <= slv_in;
      sample_record.int <= int_in;
      sample_record.usig <= usig_in;
    end if;
  end process;
  sl_out <= sample_record.sl;
  slv_out <= sample_record.slv;
  int_out <= sample_record.int;
  usig_out <= sample_record.usig;
end rtl;
