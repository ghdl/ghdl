entity tb3 is
    generic(DATA_WIDTH  : natural := 64);
end;

architecture arch of tb3 is
    type UNCONSTRAINED_RANGE_RECORD is record
        data : bit_vector;
    end record UNCONSTRAINED_RANGE_RECORD;

    procedure procedure_issue_with_range (signal b : inout UNCONSTRAINED_RANGE_RECORD(data(DATA_WIDTH - 1 downto 0)));
    
    procedure procedure_issue_with_range (signal b : inout UNCONSTRAINED_RANGE_RECORD(data(DATA_WIDTH - 1 downto 0))) is
    begin
    end procedure procedure_issue_with_range;
begin
end;
