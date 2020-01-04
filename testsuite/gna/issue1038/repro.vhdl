package pkg is
    type bv_array      is array (natural range <>) of bit_vector;
    subtype byte_array  is bv_array(open)(7 downto 0);

    type mrec is record
      b                   : boolean;
      data                : byte_array;
    end record;
end pkg;

use work.pkg.all;

entity repro is
end;

architecture behav of repro is
begin
  process
    variable a, b : mrec (data(0 to 3));
  begin
    assert a = b;
    wait;
  end process;
end behav;
