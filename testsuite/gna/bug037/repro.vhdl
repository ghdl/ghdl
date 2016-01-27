entity repro is
end;

architecture behav of repro is
  subtype byte is bit_vector (7 downto 0);
  type byte_array is array (1 to 10, boolean, 'a' to 'c') of byte;

  signal s : byte_array;
begin
  process
  begin
    wait;
  end process;
end behav;
