entity record_test is
  port (
    o : out integer
  );
end record_test;

architecture rtl of record_test is
  type t_record is record
    int  : integer range 0 to 15;
  end record t_record;
  constant rec_constant : t_record := (int => 8);
  signal int_signal : integer range 0 to rec_constant.int := 4;
begin
  o <= int_signal;
end rtl;
