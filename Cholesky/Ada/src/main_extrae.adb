
with Ada.Calendar; use Ada.Calendar;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Unchecked_Conversion;
with Interfaces.C; use type Interfaces.C.unsigned;
with System;
with OpenMP;
with Parallel.Loops.Work_Sharing;

with Ada.Command_Line;
with Ada.Text_IO;
with Extrae;

use Parallel;

procedure Main_Extrae is

    ----------------------------------------------------
    -- Definition of constants, types and auxiliary renames and packages
    ----------------------------------------------------

    -- Matrix type
    -- pragma import does not work with C defines, so we have to define new variables for the limits
    -- chaning here also needs to be changed in cholesky.h
    DIM: Integer := 8192;
    NB: constant Integer := 64;
    BS: Integer := DIM / NB;
    BSBS: Integer := BS * BS;

    type Long_Float_Access is access all Long_Float;
    pragma Convention(C, Long_Float_Access);

    type Submatrix_Type is array (0 .. BSBS-1) of aliased Long_Float;
    pragma Convention(C, Submatrix_Type);

    type Matrix_Type is array (0 .. NB-1, 0 .. NB-1) of aliased Submatrix_Type;
    pragma Convention(C, Matrix_Type);

    type Matrix_Access is access all Matrix_Type;
    pragma Convention(C, Matrix_Access);


    -- auxiliary
    GOMP_TASK_UNTIED: Interfaces.C.unsigned := 1;
    GOMP_TASK_DEPEND: Interfaces.C.unsigned := 8;
    GOMP_TASK_UNTIED_DEPEND: Interfaces.C.unsigned := GOMP_TASK_DEPEND + GOMP_TASK_UNTIED;

    DEPS_PTR_1U: OpenMP.Void_Ptr;
    pragma Import(C, DEPS_PTR_1U, "ptr_1U");

    DEPS_PTR_2U: OpenMP.Void_Ptr;
    pragma Import(C, DEPS_PTR_2U, "ptr_2U");

    DEPS_PTR_3U: OpenMP.Void_Ptr;
    pragma Import(C, DEPS_PTR_3U, "ptr_3U");

    type Submatrix_Type_Access is access all Submatrix_Type;
    pragma Convention(C, Submatrix_Type_Access);
    function Submatrix_Access_to_Void_Ptr is new Ada.Unchecked_Conversion(Source => Submatrix_Type_Access, Target => OpenMP.Void_Ptr);


    -- Global values to parametrize the executions
    Nargs : Integer;
    Num_Threads : Integer;
    OutFile: File_Type;   -- Global values to parametrize the executions

        
    --------------------------------------------------
    -- Importing C changed functions
    -------------------------------------------------

    procedure Omp_potrf (A: Submatrix_Type;
                         Ts: Interfaces.C.Int; Ld: Interfaces.C.Int);
    pragma Import (Convention => C,
                    Entity => Omp_potrf,
                    External_Name => "omp_potrf");

    procedure Omp_trsm (A: Submatrix_Type; B: Submatrix_Type;
                        Ts: Interfaces.C.Int; Ld: Interfaces.C.Int);
    pragma Import (Convention => C,
                    Entity => Omp_trsm,
                    External_Name => "omp_trsm");

    procedure Omp_syrk (A: Submatrix_Type; B: Submatrix_Type;
                        Ts: Interfaces.C.Int; Ld: Interfaces.C.Int);
    pragma Import (Convention => C,
                    Entity => Omp_syrk,
                    External_Name => "omp_syrk");

    procedure Omp_gemm (A: Submatrix_Type; B: Submatrix_Type; C: Submatrix_Type;
                        Ts: Interfaces.C.Int; Ld: Interfaces.C.Int);
    pragma Import (Convention => C,
                    Entity => Omp_gemm,
                    External_Name => "omp_gemm");

    -------------------------------------------------------
    -- sequential
    -------------------------------------------------------

    procedure Process_Sequential (M : in out Matrix_Type) is
        function Convert_to_C is new Ada.Unchecked_Conversion(Source=>Integer,
                                                              Target=>Interfaces.C.Int);
        BS_C: Interfaces.C.Int;
    begin
        if (OpenMP.Ada_OMP_Get_Thread_Num = 0)
        then
            BS_C := Convert_to_C(Bs);
            for K in 0 .. NB-1 loop
                Omp_potrf(M(K, K), BS_C, BS_C);
                for I in K + 1 .. NB-1 loop
                    Omp_trsm(M(K, K), M(K, I), BS_C, BS_C);
                end loop;

                for I in K + 1 .. NB-1 loop
                    for J in K + 1 .. NB-1 loop
                        Omp_gemm(M(K, I), M(K, J), M(J, I), BS_C, BS_C);
                    end loop;
                    Omp_syrk(M(K, I), M(I, I), BS_C, BS_C);
                end loop;
            end loop;
        end if;
    end Process_Sequential;


    ------------------------------------------------------
    -- Ada Tasks
    -------------------------------------------------------
    -- to be fair with OpenMP, tasks are created inside the timing measurement
    -- like the openmp thread pool
    -- however it is not fair because the task is statically divided in Num_Task
    -- while the openMP has the extra overhead of the thousands of tasks
    procedure Cholesky_Ada_Tasks (M : in out Matrix_Type) is
        function Convert_to_C is new Ada.Unchecked_Conversion(Source=>Integer,
                                                              Target=>Interfaces.C.Int);
        BS_C: Interfaces.C.Int;
        Size: Integer := NB;
        
        task type Process is
            entry Start (First_Pos, Last_Pos: Integer);
        end Process;

        task body Process is
            Local_First_Pos, Local_Last_Pos: Integer;
        begin
            accept  Start (First_Pos, Last_Pos: Integer) do
                Local_Last_Pos := Last_Pos;
                Local_First_Pos := First_Pos;
            end Start;
            
            BS_C := Convert_to_C(BS);
            for K in Local_First_Pos .. Local_Last_Pos loop
                Omp_potrf(M(K, K), BS_C, BS_C);
                for I in K + 1 .. NB-1 loop
                    Omp_trsm(M(K, K), M(K, I), BS_C, BS_C);
                end loop;
                
                for I in K + 1 .. NB-1 loop
                    for J in K + 1 .. NB-1 loop
                        Omp_gemm(M(K, I), M(K, J), M(J, I), BS_C, BS_C);
                    end loop;
                    Omp_syrk(M(K, I), M(I, I), BS_C, BS_C);
                end loop;
            end loop;
        end Process;

        First_Pos, Last_Pos: Integer;
        Process_Tasks: array (0..Num_threads-1) of Process;
    begin
        for I in 0..Num_threads-1 loop
            First_Pos := I * (Size / Num_threads);
            Last_Pos := (I+1) * (Size / Num_threads) - 1;
            if I = Num_threads-1 then
                Last_Pos := Size - 1;
            end if;
            Process_Tasks(I).Start(First_Pos, Last_Pos);
        end loop;
    end Cholesky_Ada_Tasks;


   -------------------------------------------------------
   -- Paraffin
   -------------------------------------------------------
   -- not quite fair, since paraffin does not create one tasklet per individual matrix element
   -- instead it divides each row in blocks
   -- so it is equivalent to an approach where each openmp tasks processes a block of elements

   procedure Cholesky_Paraffin (M : in out Matrix_Type) is
      function Convert_to_C is new Ada.Unchecked_Conversion(Source=>Integer,
                                                            Target=>Interfaces.C.Int);
      BS_C: Interfaces.C.Int;
      type Matrix_Dim is range 0 .. NB - 1;

      package Parallel_Loops is new Parallel.Loops (Matrix_Dim);
      package Iterate is new Parallel_Loops.Work_Sharing;

      procedure Generic_Iterate (Start, Finish: Matrix_Dim) is
         Local_Start: Integer := Integer(Start);
         Local_Finish: Integer := Integer(Finish);
      begin
            BS_C := Convert_to_C(BS);
            for K in Local_Start .. Local_Finish loop
                Omp_potrf(M(K, K), BS_C, BS_C);
                for I in K + 1 .. NB-1 loop
                    Omp_trsm(M(K, K), M(K, I), BS_C, BS_C);
                end loop;

                for I in K + 1 .. NB-1 loop
                    for J in K + 1 .. NB-1 loop
                        Omp_gemm(M(K, I), M(K, J), M(J, I), BS_C, BS_C);
                    end loop;
                    Omp_syrk(M(K, I), M(I, I), BS_C, BS_C);
                end loop;
            end loop;
      end Generic_Iterate;

      function Convert_to_WorkerCount is new Ada.Unchecked_Conversion(Source=>Integer,
                                                                      Target=>Worker_Count_Type);
      Num_Workers : Parallel.Worker_Count_Type := Convert_to_WorkerCount(Num_threads);
      Manager : Iterate.Work_Sharing_Manager := Iterate.Create ;
   begin
      Manager.Execute_Parallel_Loop
        (Process => Generic_Iterate'Access,
         Worker_Count => Num_Workers);
   end Cholesky_Paraffin;

   
    -------------------------------------------------------
    -- parallel Tasks
    -------------------------------------------------------

    type Tasks_Data_Type_Task_2 is -- 16 bytes
        record
            M_Address: System.Address; -- procedure uses in out parameters, so passing by pointer
            K: Integer;
            I: Integer;
        end record;
    pragma Convention(C, Tasks_Data_Type_Task_2);
    type Tasks_Data_Access_Task_2_Type is access Tasks_Data_Type_Task_2;
    pragma Convention(C, Tasks_Data_Access_Task_2_Type);
    procedure Tasks_Function_Task_2(Task_Params: System.Address);
    pragma Convention(C, Tasks_Function_Task_2);
    procedure Tasks_Function_Task_2(Task_Params: System.Address) is
        function Convert_to_Task is new Ada.Unchecked_Conversion(Source=>System.Address,
                                                                Target=>Tasks_Data_Access_Task_2_Type);
        function Convert_to_Matrix is new Ada.Unchecked_Conversion(Source=>System.Address,
                                                                    Target=>Matrix_Access);
        Task_Data_Access: Tasks_Data_Access_Task_2_Type := Convert_to_Task(Task_Params);
        M_Access: Matrix_Access := Convert_to_Matrix(Task_Data_Access.M_Address);

        function Convert_to_C is new Ada.Unchecked_Conversion(Source=>Integer,
                                                              Target=>Interfaces.C.Int);
        BS_C: Interfaces.C.Int;
    begin
        BS_C := Convert_to_C(Bs);
        Extrae.Ada_Extrae_event(6000, 2);
        Omp_trsm(M_Access.all(Task_Data_Access.K, Task_Data_Access.K),
                 M_Access.all(Task_Data_Access.K, Task_Data_Access.I),
                 BS_C, BS_C);
        Extrae.Ada_Extrae_event(6000, 0);
    end Tasks_Function_Task_2;


    type Tasks_Data_Type_Task_3 is -- 24 bytes
        record
            M_Address: System.Address; -- procedure uses in out parameters, so passing by pointer
            K: Integer;
            I: Integer;
            J: Integer;
        end record;
    pragma Convention(C, Tasks_Data_Type_Task_3);
    type Tasks_Data_Access_Task_3_Type is access Tasks_Data_Type_Task_3;
    pragma Convention(C, Tasks_Data_Access_Task_3_Type);
    procedure Tasks_Function_Task_3(Task_Params: System.Address);
    pragma Convention(C, Tasks_Function_Task_3);
    procedure Tasks_Function_Task_3(Task_Params: System.Address) is
        function Convert_to_Task is new Ada.Unchecked_Conversion(Source=>System.Address,
                                                                Target=>Tasks_Data_Access_Task_3_Type);
        function Convert_to_Matrix is new Ada.Unchecked_Conversion(Source=>System.Address,
                                                                    Target=>Matrix_Access);
        Task_Data_Access: Tasks_Data_Access_Task_3_Type := Convert_to_Task(Task_Params);
        M_Access: Matrix_Access := Convert_to_Matrix(Task_Data_Access.M_Address);
        function Convert_to_C is new Ada.Unchecked_Conversion(Source=>Integer,
                                                              Target=>Interfaces.C.Int);
        BS_C: Interfaces.C.Int;
    begin
        BS_C := Convert_to_C(Bs);
        Extrae.Ada_Extrae_event(6000, 3);
        Omp_gemm(M_Access.all(Task_Data_Access.K, Task_Data_Access.I),
                 M_Access.all(Task_Data_Access.K, Task_Data_Access.J),
                 M_Access.all(Task_Data_Access.J, Task_Data_Access.I),
                 BS_C, BS_C);
        Extrae.Ada_Extrae_event(6000, 0);
    end Tasks_Function_Task_3;

    type Parallel_Data_Type is record
        M_Address: System.Address;
    end record;
    pragma Convention(C, Parallel_Data_Type);
    type Parallel_Data_Type_Access is access all Parallel_Data_Type;
    pragma Convention(C, Parallel_Data_Type_Access);
    procedure Parallel_Function_Tasks(Parallel_Params: System.Address);
    pragma Convention(C, Parallel_Function_Tasks);
    procedure Parallel_Function_Tasks(Parallel_Params: System.Address) is
        function Convert_to_Parallel is new Ada.Unchecked_Conversion(Source=>System.Address,
                                                                    Target=>Parallel_Data_Type_Access);
        function Convert_to_Matrix is new Ada.Unchecked_Conversion(Source=>System.Address,
                                                                    Target=>Matrix_Access);
        Data_Access: Parallel_Data_Type_Access := Convert_to_Parallel(Parallel_Params);
        M_Access: Matrix_Access := Convert_to_Matrix(Data_Access.M_Address);
        Depend_Clauses: OpenMP.Void_Ptr_Ptr := null;
        function Convert_to_C is new Ada.Unchecked_Conversion(Source=>Integer,
                                                              Target=>Interfaces.C.Int);
        BS_C: Interfaces.C.Int;
    begin
        if (OpenMP.Ada_OMP_Get_Thread_Num = 0)
        then
            BS_C := Convert_to_C(Bs);
            for K in 0 .. NB-1 loop
                Extrae.Ada_Extrae_event(6000, 1);
                Omp_potrf(M_Access.all(K, K), BS_C, BS_C);
                Extrae.Ada_Extrae_event(6000, 0);
             
                for I in K + 1 .. NB-1 loop
                    declare
                        Task_Data_Access: Tasks_Data_Access_Task_2_Type;
                        Task_Data_Address: System.Address;
                    begin
                        Task_Data_Access := new Tasks_Data_Type_Task_2;
                        Task_Data_Access.M_Address := Data_Access.M_Address;
                        Task_Data_Access.K := K;
                        Task_Data_Access.I := I;
                        Task_Data_Address := Task_Data_Access.all'Address;
                        OpenMP.Ada_GOMP_Task(Tasks_Function_Task_2'Unrestricted_Access, Task_Data_Address, null, 16, 8, TRUE, GOMP_TASK_UNTIED, Depend_Clauses, 0);
                    end;
                end loop;

                OpenMP.Ada_GOMP_Taskwait;

                for I in K + 1 .. NB-1 loop
                    for J in K + 1 .. NB-1 loop
                        declare
                            Task_Data_Access: Tasks_Data_Access_Task_3_Type;
                            Task_Data_Address: System.Address;
                        begin
                            Task_Data_Access := new Tasks_Data_Type_Task_3;
                            Task_Data_Access.M_Address := Data_Access.M_Address;
                            Task_Data_Access.K := K;
                            Task_Data_Access.I := I;
                            Task_Data_Access.J := J;
                            Task_Data_Address := Task_Data_Access.all'Address;
                            OpenMP.Ada_GOMP_Task( Tasks_Function_Task_3'Unrestricted_Access, Task_Data_Address, null, 20, 8, TRUE, GOMP_TASK_UNTIED, Depend_Clauses, 0);
                        end;
                    end loop;
                    OpenMP.Ada_GOMP_Taskwait;
                    Extrae.Ada_Extrae_event(6000, 4);
                    Omp_syrk(M_Access.all(K, I), M_Access.all(I, I), BS_C, BS_C);
                    Extrae.Ada_Extrae_event(6000, 0);
                end loop;
            end loop;
        end if;
    end Parallel_Function_Tasks;

    procedure Process_Parallel_Tasks (M : in out Matrix_Type) is
        Parallel_Data : Parallel_Data_Type;
        function Convert_to_C is new Ada.Unchecked_Conversion(Source=>Integer,
                                                              Target=>Interfaces.C.unsigned);
        Num_threads_C : Interfaces.C.unsigned := Convert_to_C(Num_threads);
    begin
        Parallel_Data.M_Address := M'Address;
        OpenMP.Ada_GOMP_parallel_start(Parallel_Function_Tasks'Unrestricted_Access, Parallel_Data'Address, Num_threads_C);
        Parallel_Function_Tasks(Parallel_Data'Address);
        OpenMP.Ada_GOMP_parallel_end;
    end Process_Parallel_Tasks;

    -------------------------------------------------------
    -- parallel Deps
    -------------------------------------------------------

    -- openmp task data types and code is the same as in the tasks version plus type 1
    type Tasks_Data_Type_Task_1 is -- 12 bytes
        record
            M_Address: System.Address; -- procedure uses in out parameters, so passing by pointer
            K: Integer;
        end record;
    pragma Convention(C, Tasks_Data_Type_Task_1);
    type Tasks_Data_Access_Task_1_Type is access Tasks_Data_Type_Task_1;
    pragma Convention(C, Tasks_Data_Access_Task_1_Type);
    procedure Tasks_Function_Task_1(Task_Params: System.Address);
    pragma Convention(C, Tasks_Function_Task_1);
    procedure Tasks_Function_Task_1(Task_Params: System.Address) is
        function Convert_to_Task is new Ada.Unchecked_Conversion(Source=>System.Address,
                                                                Target=>Tasks_Data_Access_Task_1_Type);
        function Convert_to_Matrix is new Ada.Unchecked_Conversion(Source=>System.Address,
                                                                    Target=>Matrix_Access);
        Task_Data_Access: Tasks_Data_Access_Task_1_Type := Convert_to_Task(Task_Params);
        M_Access: Matrix_Access := Convert_to_Matrix(Task_Data_Access.M_Address);

        function Convert_to_C is new Ada.Unchecked_Conversion(Source=>Integer,
                                                              Target=>Interfaces.C.Int);
        BS_C: Interfaces.C.Int;
    begin
        Extrae.Ada_Extrae_event(6000, 1);
        BS_C := Convert_to_C(Bs);
        Omp_potrf(M_Access.all(Task_Data_Access.K, Task_Data_Access.K),
                 BS_C, BS_C);
        Extrae.Ada_Extrae_event(6000, 0);
    end Tasks_Function_Task_1;

    type Tasks_Data_Type_Task_4 is -- 20 bytes
        record
            M_Address: System.Address; -- procedure uses in out parameters, so passing by pointer
            K: Integer;
            I: Integer;
        end record;
    pragma Convention(C, Tasks_Data_Type_Task_4);
    type Tasks_Data_Access_Task_4_Type is access Tasks_Data_Type_Task_4;
    pragma Convention(C, Tasks_Data_Access_Task_4_Type);
    procedure Tasks_Function_Task_4(Task_Params: System.Address);
    pragma Convention(C, Tasks_Function_Task_4);

    procedure Tasks_Function_Task_4(Task_Params: System.Address) is
        function Convert_to_Task is new Ada.Unchecked_Conversion(Source=>System.Address,
                                                                Target=>Tasks_Data_Access_Task_4_Type);
        function Convert_to_Matrix is new Ada.Unchecked_Conversion(Source=>System.Address,
                                                                    Target=>Matrix_Access);
        Task_Data_Access: Tasks_Data_Access_Task_4_Type := Convert_to_Task(Task_Params);
        M_Access: Matrix_Access := Convert_to_Matrix(Task_Data_Access.M_Address);

        function Convert_to_C is new Ada.Unchecked_Conversion(Source=>Integer,
                                                              Target=>Interfaces.C.Int);
        BS_C: Interfaces.C.Int;
    begin
        Extrae.Ada_Extrae_event(6000, 4);
        BS_C := Convert_to_C(Bs);
        Omp_syrk(M_Access.all(Task_Data_Access.K, Task_Data_Access.I),
                 M_Access.all(Task_Data_Access.I, Task_Data_Access.I),
                 BS_C, BS_C);
        Extrae.Ada_Extrae_event(6000, 0);
    end Tasks_Function_Task_4;

    procedure Parallel_Function_Deps(Parallel_Params: System.Address);
    pragma Convention(C, Parallel_Function_Deps);
    procedure Parallel_Function_Deps(Parallel_Params: System.Address) is
        function Convert_to_Parallel is new Ada.Unchecked_Conversion(Source=>System.Address,
                                                                    Target=>Parallel_Data_Type_Access);
        function Convert_to_Matrix is new Ada.Unchecked_Conversion(Source=>System.Address,
                                                                    Target=>Matrix_Access);
        Data_Access: Parallel_Data_Type_Access := Convert_to_Parallel(Parallel_Params);
        M_Access: Matrix_Access := Convert_to_Matrix(Data_Access.M_Address);

        function Convert_to_C is new Ada.Unchecked_Conversion(Source=>Integer,
                                                              Target=>Interfaces.C.Int);
        BS_C: Interfaces.C.Int;
    begin
        if (OpenMP.Ada_OMP_Get_Thread_Num = 0) then
            BS_C := Convert_to_C(Bs);
            for K in 0 .. NB-1 loop
                declare
                    Task_Data_Access: Tasks_Data_Access_Task_1_Type;
                    Task_Data_Address: System.Address;
                    Depend_Clauses_Array: aliased array(1..3) of aliased OpenMP.Void_Ptr :=
                        (DEPS_PTR_1U, DEPS_PTR_1U, Submatrix_Access_to_Void_Ptr(M_Access.all(K,K)'Access));
                    Depend_Clauses : OpenMP.Void_Ptr_Ptr := Depend_Clauses_Array(1)'Unrestricted_Access;
                begin
                    Task_Data_Access := new Tasks_Data_Type_Task_1;
                    Task_Data_Access.M_Address := Data_Access.M_Address;
                    Task_Data_Access.K := K;
                    Task_Data_Address := Task_Data_Access.all'Address;

                    OpenMP.Ada_GOMP_Task(Tasks_Function_Task_1'Unrestricted_Access, Task_Data_Address, null, 12, 8, TRUE, GOMP_TASK_UNTIED_DEPEND, Depend_Clauses, 0);
                end;

                for I in K + 1 .. NB-1 loop
                    declare
                        Task_Data_Access: Tasks_Data_Access_Task_2_Type;
                        Task_Data_Address: System.Address;
                        Depend_Clauses_Array: aliased array(1..4) of aliased OpenMP.Void_Ptr :=
                            (DEPS_PTR_2U, DEPS_PTR_1U, Submatrix_Access_to_Void_Ptr(M_Access.all(K,K)'Access), Submatrix_Access_to_Void_Ptr(M_Access.all(K,I)'Access));
                        Depend_Clauses : OpenMP.Void_Ptr_Ptr := Depend_Clauses_Array(1)'Unrestricted_Access;
                    begin
                        Task_Data_Access := new Tasks_Data_Type_Task_2;
                        Task_Data_Access.M_Address := Data_Access.M_Address;
                        Task_Data_Access.K := K;
                        Task_Data_Access.I := I;
                        Task_Data_Address := Task_Data_Access.all'Address;
                        OpenMP.Ada_GOMP_Task(Tasks_Function_Task_2'Unrestricted_Access, Task_Data_Address, null, 16, 8, TRUE, GOMP_TASK_UNTIED, Depend_Clauses, 0);
                    end;
                end loop;

                for I in K + 1 .. NB-1 loop
                    for J in K + 1 .. NB-1 loop
                        declare
                            Task_Data_Access: Tasks_Data_Access_Task_3_Type;
                            Task_Data_Address: System.Address;
                            Depend_Clauses_Array: aliased array(1..5) of aliased OpenMP.Void_Ptr :=
                                (DEPS_PTR_3U, DEPS_PTR_1U, Submatrix_Access_to_Void_Ptr(M_Access.all(K,I)'Access), Submatrix_Access_to_Void_Ptr(M_Access.all(K,J)'Access), Submatrix_Access_to_Void_Ptr(M_Access.all(J,I)'Access));
                            Depend_Clauses : OpenMP.Void_Ptr_Ptr := Depend_Clauses_Array(1)'Unrestricted_Access;
                        begin
                            Task_Data_Access := new Tasks_Data_Type_Task_3;
                            Task_Data_Access.M_Address := Data_Access.M_Address;
                            Task_Data_Access.K := K;
                            Task_Data_Access.I := I;
                            Task_Data_Access.J := J;
                            Task_Data_Address := Task_Data_Access.all'Address;
                            OpenMP.Ada_GOMP_Task(Tasks_Function_Task_3'Unrestricted_Access, Task_Data_Address, null, 20, 8, TRUE, GOMP_TASK_UNTIED, Depend_Clauses, 0);
                        end;
                    end loop;

                    declare
                        Task_Data_Access: Tasks_Data_Access_Task_4_Type;
                        Task_Data_Address: System.Address;
                        Depend_Clauses_Array: aliased array(1..4) of aliased OpenMP.Void_Ptr :=
                            (DEPS_PTR_2U, DEPS_PTR_1U, Submatrix_Access_to_Void_Ptr(M_Access.all(K,I)'Access), Submatrix_Access_to_Void_Ptr(M_Access.all(I,I)'Access));
                        Depend_Clauses : OpenMP.Void_Ptr_Ptr := Depend_Clauses_Array(1)'Unrestricted_Access;
                    begin
                        Task_Data_Access := new Tasks_Data_Type_Task_4;
                        Task_Data_Access.M_Address := Data_Access.M_Address;
                        Task_Data_Access.K := K;
                        Task_Data_Access.I := I;
                        Task_Data_Address := Task_Data_Access.all'Address;
                        OpenMP.Ada_GOMP_Task(Tasks_Function_Task_4'Unrestricted_Access, Task_Data_Address, null, 16, 8, TRUE, GOMP_TASK_UNTIED, Depend_Clauses, 0);
                    end;
                end loop;
            end loop;
        end if;
    end Parallel_Function_Deps;

    procedure Process_Parallel_Deps (M : in out Matrix_Type) is
        Parallel_Data : Parallel_Data_Type;
        function Convert_to_C is new Ada.Unchecked_Conversion(Source=>Integer,
                                                              Target=>Interfaces.C.unsigned);
        Num_threads_C : Interfaces.C.unsigned := Convert_to_C(Num_threads);
    begin
        Parallel_Data.M_Address := M'Address;
        OpenMP.Ada_GOMP_parallel_start(Parallel_Function_Deps'Unrestricted_Access, Parallel_Data'Address, Num_threads_C);
        Parallel_Function_Deps(Parallel_Data'Address);
        OpenMP.Ada_GOMP_parallel_end;
     end Process_Parallel_Deps;


    --------------------------------------------------
    -- Auxiliary subprograms
    -- Init external to initialize the matrix
    -- Print prints the matrix
    --------------------------------------------------

    --  Declare an Ada procedure spec for initialize_matrix, then use
    --  C function initialize_matrix for the implementation.
    procedure Initialize_matrix (M: in out Matrix_Type);
    pragma Import (Convention => C,
                    Entity => Initialize_matrix,
                    External_Name => "initialize_matrix");
--     procedure Print_Matrix (M: Matrix_Type);
--     pragma Import (Convention => C,
--                     Entity => Print_Matrix,
--                     External_Name => "sparselu_print");
--     procedure Check_Matrix (M2: Matrix_Type; M1: Matrix_Type);
--     pragma Import (Convention => C,
--                     Entity => Check_Matrix,
--                     External_Name => "sparselu_check");
--     procedure SparseLU_Seq (M: Matrix_Type);
--     pragma Import (Convention => C,
--                     Entity => SparseLU_Seq,
--                     External_Name => "sparselu_seq");

    --------------------------------------------------
    -- Definition of program variables
    --------------------------------------------------

    -- Matrix
    M_Access, M_old: Matrix_Access;

    -- Time variables
    Start_Time, Finish_Time: Time;

    package IO renames Ada.Text_IO;
    package CLI renames Ada.Command_Line;

begin

    -- Read arguments
    Nargs := CLI.Argument_Count;
    if Nargs /= 3 then
        IO.Put_Line ("Use: ./main version n_threads out_file.csv");
        return;
    end if;

    Extrae.Ada_Extrae_init;

    Num_Threads := Integer'Value(CLI.Argument (Number => 2));

    begin
    Open (File => OutFile,
            Mode => Append_File,
            Name => CLI.Argument (Number => 3));
    exception
    when Name_Error =>
        Create (
            File => OutFile,
            Mode => Append_File,
            Name => CLI.Argument (Number => 3));
--        when others =>
--            Put_Line (Standard_Error,
--                    "Can not create a file named '" & CLI.Argument (Number => 4) & "'.");
--            CLI.Set_Exit_Status (CLI.Failure);
--            return;
        end;

    M_Access := new Matrix_Type;
    M_Old := new Matrix_Type;

    Initialize_matrix(M_Access.all);

    --------------------------------------------------
    -- Execution of Sequential
    --------------------------------------------------
    if CLI.Argument (Number => 1) = "seq" then
        Start_Time := Clock;
        Process_Sequential(M_Access.all);
        Finish_Time := Clock;
        Put_Line("Time for sequential: " & Duration'Image(Finish_Time - Start_Time) & " seconds.");
        Put_Line(OutFile, Duration'Image(Finish_Time - Start_Time)&",");
        M_old.all := M_Access.all;
    end if;

    --------------------------------------------------
    -- Execution of Ada tasks
    --------------------------------------------------
    if CLI.Argument (Number => 1) = "ada_tasks" then
        Start_Time := Clock;
        Cholesky_Ada_Tasks(M_Access.all);
        Finish_Time := Clock;
        Put_Line("Time for ada tasks: " & Duration'Image(Finish_Time - Start_Time) & " seconds.");
        Put_Line(OutFile, Duration'Image(Finish_Time - Start_Time)&",");
        M_old.all := M_Access.all;
    end if;

    --------------------------------------------------
    -- Execution of Paraffin
    --------------------------------------------------
    if CLI.Argument (Number => 1) = "ada_paraffin" then
        Start_Time := Clock;
        Cholesky_Paraffin(M_Access.all);
        Finish_Time := Clock;
        Put_Line("Time for ada tasks: " & Duration'Image(Finish_Time - Start_Time) & " seconds.");
        Put_Line(OutFile, Duration'Image(Finish_Time - Start_Time)&",");
        M_old.all := M_Access.all;
    end if;

    --------------------------------------------------
    -- Execution with the new Ada parallel functions
    --------------------------------------------------
    -- Put_Line("With Ada parallel code");
    -- Execution of Parallel tasks

    if CLI.Argument (Number => 1) = "ada_omp_tasks" then
        Start_Time := Clock;
        Process_Parallel_Tasks(M_Access.all);
        Finish_Time := Clock;
        Put_Line("Time for parallel tasks: " & Duration'Image(Finish_Time - Start_Time) & " seconds.");
        Put_Line(OutFile, Duration'Image(Finish_Time - Start_Time)&",");
    end if;

    -- Execution of Parallel deps
    if CLI.Argument (Number => 1) = "ada_omp_deps" then
        Start_Time := Clock;
        Process_Parallel_Deps(M_Access.all);
        Finish_Time := Clock;
        Put_Line("Time for parallel deps: " & Duration'Image(Finish_Time - Start_Time) & " seconds.");
        Put_Line(OutFile, Duration'Image(Finish_Time - Start_Time)&",");
        -- Put_Line("Checking ...");
        -- Check_Matrix(M_Access.all, M_old.all);
    end if;

    Extrae.Ada_Extrae_fini;

end Main_Extrae;


