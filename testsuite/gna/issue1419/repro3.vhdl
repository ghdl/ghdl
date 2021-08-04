entity repro3 is
end;

architecture behav of repro3 is
  type t_record is record
    str : string;
    num : positive;
  end record;

  constant k : t_record := ("abc", 0); --  BOUND error
begin
end;
