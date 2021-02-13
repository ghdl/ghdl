entity test is
end entity;

architecture arch of test is
    -- This works:
    --subtype y_vec is real_vector(0 to 3);
    --type realvec_file is file of y_vec;

    -- This complains bout 'index constraint not allowed here'
    type realvec_file is file of real_vector(0 to 3);
begin
end arch;
