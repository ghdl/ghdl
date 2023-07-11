package test_pkg is

    type test_t is record
        data : bit_vector ;
    end record ;

    type config_t is record
        width : positive ;
    end record ;

    constant DEFAULT_CONFIG : config_t := config_t'(width => 10) ;

    package make is
      generic (
        CONFIG : config_t := DEFAULT_CONFIG
      ) ;

        subtype test_t is work.test_pkg.test_t(data(CONFIG.WIDTH-1 downto 0)) ;

    end package ;

end package ;
