entity arr is
  generic (width : natural := 4);
end arr;

architecture behav of arr is
  subtype line is bit_vector (1 to width);
  type memory is array (0 to 7) of line;
begin
  process is
    variable l : line;
    variable mem : memory;
  begin
    wait;
  end process;
end behav;
