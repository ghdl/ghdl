entity repro is
end;

architecture behav of repro is
  subtype byte is bit_vector (7 downto 0);
  type byte_array is array (1 to 2, boolean, 'a' to 'c') of byte;

  type bv_array is array (integer range <>) of bit_vector;

  type my_rec is record
    s1 : string;
    s2 : string;
  end record;
  signal s : byte_array;
  signal s2 : bv_array (3 downto 0)(15 downto 0);
  signal s3 : my_rec (s1 (1 to 4), s2 (1 to 5)) := (s1 => "hi!!",
                                                    s2 => "world");
begin
  process
  begin
    wait;
  end process;
end behav;
