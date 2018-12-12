package t is
  type str_acc is access string;

  function my_alloc (v : natural) return str_acc;
  attribute foreign of my_alloc : function is "VHPIDIRECT";
end t;

