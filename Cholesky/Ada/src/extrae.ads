with Interfaces.C;

package Extrae is
    package C renames Interfaces.C;
    
    procedure Ada_Extrae_event(
            C_Type: in C.unsigned;
            C_Value: in C.unsigned_long
            );
    pragma Import (Convention => C,
                    Entity => Ada_Extrae_event,
                    External_Name => "Extrae_event");

    procedure Ada_Extrae_init;
    pragma Import (Convention => C,
                    Entity => Ada_Extrae_init,
                    External_Name => "Extrae_init");

    procedure Ada_Extrae_fini;
    pragma Import (Convention => C,
                    Entity => Ada_Extrae_fini,
                    External_Name => "Extrae_fini");
end Extrae;