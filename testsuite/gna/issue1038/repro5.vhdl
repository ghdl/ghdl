entity repro5 is
end;

architecture behav of repro5 is
    type bv_array      is array (natural range <>) of bit_vector;
    subtype byte_array  is bv_array(open)(7 downto 0);

    type mrec is record
      b                   : boolean;
      data                : byte_array;
      d2                  : bit_vector;
    end record;

    procedure assign (signal s : out mrec; v : mrec) is
  begin
    s <= v;
  end assign;
  signal s : mrec (data(0 to 3), d2(0 to 7));
begin
  process
    variable a : mrec (data(1 to 4), d2 (1 to 8));
  begin
    assign (s, a);
    wait;
  end process;
end behav;
