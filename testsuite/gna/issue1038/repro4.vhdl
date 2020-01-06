entity repro4 is
end;

architecture behav of repro4 is
    type bv_array      is array (natural range <>) of bit_vector;
    subtype byte_array  is bv_array(open)(7 downto 0);

    type mrec is record
      b                   : boolean;
      data                : byte_array;
    end record;

    signal s : mrec (data(0 to 3));

  function get_val return mrec is
    variable a : mrec (data(1 to 4));
  begin
    return a;
  end get_val;
begin
  process
  begin
    s <= get_val;
    wait;
  end process;
end behav;
