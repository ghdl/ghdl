entity repro1 is
end;

architecture behav of repro1 is
    type t_record is
        record
          --srt : string (1 downto 1); -- works
            srt : string;
            num : positive;
        end record;

    type t_record_array is array (natural range <>) of t_record;

    constant k : t_record_array := (("a", 1), ("b", 2));
begin
end;
