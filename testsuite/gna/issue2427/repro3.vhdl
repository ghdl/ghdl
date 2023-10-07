package repro3 is
  type rec is record
    a : bit_vector(1 downto 0);
  end record;

  attribute attr : string;

  --  rec is in fact a subtype...
  attribute attr of rec : type is "yes";
end repro3;
