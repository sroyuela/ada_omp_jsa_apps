with Interfaces.C;
with System;
with Ada.Unchecked_Conversion;

package OpenMP is

     -- renaming Interfaces.C
   
   package C renames Interfaces.C;
   
   
   ----------------------------------------------------
   -- Interface types 
   ----------------------------------------------------
   
    type Void is null record;
    pragma Convention (C, Void);

    type Void_Ptr is access all Void;
    pragma Convention (C, Void_Ptr);

    type Void_Ptr_Ptr is access all Void_Ptr;
    pragma Convention (C, Void_Ptr_Ptr);

   
    type Task_Fn_Type is access procedure (Task_Params: System.Address);
    pragma Convention(C, Task_Fn_Type);

   
    type Parallel_Fn_Type is access procedure (Parallel_Params: System.Address);
    pragma Convention(C, Parallel_Fn_Type);
   
    type Copy_Fn_Type is access procedure (Copy_param_1: System.Address;
                                           Copy_param_2: System.Address);
    pragma Convention(C, Copy_Fn_Type);
   ----------------------------------------------------
   -- Interface subprograms 
   ----------------------------------------------------
  
   
    --  Declare an Ada function spec for Ada_OMP_Get_Thread_Num, then use
    --  C function omp_get_thread_num for the implementation.
    function Ada_OMP_Get_Thread_Num return Integer;
    pragma Import (Convention => C,
                   Entity => Ada_OMP_Get_Thread_Num,
                   External_Name => "omp_get_thread_num");

   
    --  Declare an Ada procedure spec for Ada_GOMP_parallel_start, then use
    --  C function GOMP_parallel_start for the implementation.
    procedure Ada_GOMP_Parallel_Start(
            C_Function: in not null Parallel_Fn_Type;
            C_Data: System.Address;
            C_Num_Threads: in C.unsigned
            );
    pragma Import (Convention => C,
                   Entity => Ada_GOMP_Parallel_Start,
                   External_Name => "GOMP_parallel_start");

    --  Declare an Ada procedure spec for Ada_GOMP_parallel_end, then use
    --  C function GOMP_parallel_end for the implementation.
    procedure Ada_GOMP_Parallel_End;
    pragma Import (Convention => C,
                   Entity => Ada_GOMP_Parallel_End,
                   External_Name => "GOMP_parallel_end");

   
    --  Declare an Ada procedure spec for Ada_GOMP_taskwait, then use
    --  C function GOMP_task for the implementation.
    procedure Ada_GOMP_Taskwait;
    pragma Import (Convention => C,
                   Entity => Ada_GOMP_Taskwait,
                   External_Name => "GOMP_taskwait");

   
    --  Declare an Ada procedure spec for Ada_GOMP_task, then use
    --  C function GOMP_task for the implementation.
    procedure Ada_GOMP_task(
            C_Function: in not null Task_Fn_Type;     -- void (*fn) (void *)
            C_Data: System.Address;
            C_Copy_Function: Copy_Fn_Type;    -- void (*cpyfn) (void *, void *)
            C_Arg_Size: C.long;      -- Long_Integer
            C_Arg_Align: C.long;     -- Long_Integer
            C_If_Clause: Boolean;     -- Boolean
            C_Flags: C.unsigned;     -- Unisgned_8
            C_Depend: Void_Ptr_Ptr;
            C_priority: C.int        -- Integer
            );
    pragma Import (Convention => C,
                   Entity => Ada_GOMP_task,
                   External_Name => "GOMP_task");

 

end OpenMP;
