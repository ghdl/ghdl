package model_pkg is
    type t_record is
        record
          --srt : string (1 downto 1); -- works
            srt : string;
            num : natural;
        end record;

    type t_record_array is array (natural range <>) of t_record;

    constant k : t_record_array := (("a", 0), ("b", 1));
end package;
