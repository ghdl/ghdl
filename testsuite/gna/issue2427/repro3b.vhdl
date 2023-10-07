package repro3b is
  type rec is record
    a : bit_vector(1 downto 0);
  end record;

  attribute attr : string;

  attribute attr of rec : subtype is "yes";
end repro3b;
