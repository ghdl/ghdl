entity repro3 is
end;

architecture behav of repro3 is
    type bv_array      is array (natural range <>) of bit_vector;
    subtype byte_array  is bv_array(open)(7 downto 0);

    type mrec is record
      b                   : boolean;
      data                : byte_array;
    end record;

    signal s : mrec (data(0 to 3));
  procedure assign (signal sig : out mrec) is
    variable a : mrec (data(1 to 4));
  begin
    sig <= a;
  end assign;
begin
  process
  begin
    assign (s);
    wait;
  end process;
end behav;
