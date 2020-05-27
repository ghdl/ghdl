entity myentity is
end myentity;

architecture arch of myentity is
  type myrecord is record
    a : bit_vector;
  end record;
  subtype myboundedrecord is myrecord(a(1 downto 0));
  type myarray is array (natural range <>) of myboundedrecord;
begin
end arch;
