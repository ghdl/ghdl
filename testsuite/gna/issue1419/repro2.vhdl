entity repro2 is
end;

architecture behav of repro2 is
  type t_record is record
    str : string;
    num : positive;
  end record;

  constant k : t_record := ("abc", 1);
begin
end;
