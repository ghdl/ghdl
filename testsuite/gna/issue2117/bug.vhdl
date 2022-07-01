entity bug is end; 

architecture a of bug is 
    type t1 is (enum_val_1);

    procedure p is 
    begin
        enum_val_1.missing_identifier;
    end;
begin 
end;
