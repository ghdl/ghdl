entity repro is
end;

architecture behav of repro is
  type string_vector is array (natural range <>) of string;

  type tokenized_line is record
	op: 	string;
	params: string_vector;
  end record;

  constant k : tokenized_line := ("", ("", ""));
begin
end;
