entity repro2 is
end;

architecture behav of repro2 is
    type bv_array      is array (natural range <>) of bit_vector;
    subtype byte_array  is bv_array(open)(7 downto 0);

    type mrec is record
      b                   : boolean;
      data                : byte_array;
    end record;

    signal s : mrec (data(0 to 3));
begin
  process
    variable a : mrec (data(1 to 4));
  begin
    s <= a; -- after 1 ns;
    wait for 2 ns;
    s <= s;
    wait;
  end process;
end behav;
