
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
-- with Extrae;

use Parallel;

procedure Main is

    ----------------------------------------------------
    -- Definition of constants, types and auxiliary renames and packages
    ----------------------------------------------------

    -- Matrix type
    -- pragma import does not work with C defines, so we have to define new variables for the limits
    -- changing here also needs to be changed in cholesky.h
    DIM: Integer := 8192;
    DIMDIM : Integer := DIM * DIM;
    NB: constant Integer := 256;
    BS: Integer := DIM / NB;
    BSBS: Integer := BS * BS;

    type Long_Float_Access is access all Long_Float;
    pragma Convention(C, Long_Float_Access);

    -- Linear Matrix
    type Linear_Matrix_Type is array (0 .. DIM*DIM-1) of aliased Long_Float;
    type Linear_Matrix_Access is access all Linear_Matrix_Type;
    pragma Convention(C, Linear_Matrix_Type);
    pragma Convention(C, Linear_Matrix_Access);

    -- Blocked Matrix
    type Blocked_Submatrix_Type is array (0 .. BSBS-1) of aliased Long_Float;
    type Blocked_Submatrix_Access is access all Blocked_Submatrix_Type;
    pragma Convention(C, Blocked_Submatrix_Type);
    pragma Convention(C, Blocked_Submatrix_Access);

    type Blocked_Matrix_Type is array (0 .. NB-1, 0 .. NB-1) of aliased Blocked_Submatrix_Access;
    type Blocked_Matrix_Access is access all Blocked_Matrix_Type;
    pragma Convention(C, Blocked_Matrix_Type);
    pragma Convention(C, Blocked_Matrix_Access);


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

    function Blocked_Submatrix_to_Void_Ptr is new Ada.Unchecked_Conversion(Source => Blocked_Submatrix_Access, Target => OpenMP.Void_Ptr);

    -- Global values to parametrize the executions
    Nargs : Integer;
    Num_threads : Integer;
    OutFile: File_Type;   -- Global values to parametrize the executions
    Check : Boolean;

        
    --------------------------------------------------
    -- Importing C changed functions
    -------------------------------------------------

    procedure Omp_potrf (A: Blocked_Submatrix_Access;
                         Ts: Interfaces.C.Int; Ld: Interfaces.C.Int);
    pragma Import (Convention => C,
                    Entity => Omp_potrf,
                    External_Name => "omp_potrf");

    procedure Omp_trsm (A: Blocked_Submatrix_Access; B: Blocked_Submatrix_Access;
                        Ts: Interfaces.C.Int; Ld: Interfaces.C.Int);
    pragma Import (Convention => C,
                    Entity => Omp_trsm,
                    External_Name => "omp_trsm");

    procedure Omp_syrk (A: Blocked_Submatrix_Access; B: Blocked_Submatrix_Access;
                        Ts: Interfaces.C.Int; Ld: Interfaces.C.Int);
    pragma Import (Convention => C,
                    Entity => Omp_syrk,
                    External_Name => "omp_syrk");

    procedure Omp_gemm (A: Blocked_Submatrix_Access; B: Blocked_Submatrix_Access; C: Blocked_Submatrix_Access;
                        Ts: Interfaces.C.Int; Ld: Interfaces.C.Int);
    pragma Import (Convention => C,
                    Entity => Omp_gemm,
                    External_Name => "omp_gemm");

    -------------------------------------------------------
    -- utils
    -------------------------------------------------------
    procedure Save_original_matrix(M_source: in out Linear_Matrix_Type;
                                   M_target: in out Linear_Matrix_Type) is
    begin
        for I in 0 .. DIMDIM-1 loop
            M_target(I) := M_source(I);
        end loop;
    end;

    -------------------------------------------------------
    -- sequential
    -------------------------------------------------------
    procedure Process_Sequential (M : in out Blocked_Matrix_Type) is
        function Convert_to_C is new Ada.Unchecked_Conversion(Source=>Integer,
                                                              Target=>Interfaces.C.Int);
        BS_C: Interfaces.C.Int;
    begin
        BS_C := Convert_to_C(Bs);
        for K in 0 .. NB-1 loop
            Omp_potrf(M(K, K), BS_C, BS_C);
            for I in K + 1 .. NB-1 loop
                Omp_trsm(M(K, K), M(K, I), BS_C, BS_C);
            end loop;

            for I in K + 1 .. NB-1 loop
                for J in K + 1 .. I-1 loop
                    Omp_gemm(M(K, I), M(K, J), M(J, I), BS_C, BS_C);
                end loop;
                Omp_syrk(M(K, I), M(I, I), BS_C, BS_C);
            end loop;
        end loop;
    end Process_Sequential;

    ------------------------------------------------------
    -- Ada Tasks
    -------------------------------------------------------
    -- to be fair with OpenMP, tasks are created inside the timing measurement
    -- like the openmp thread pool
    -- however it is not fair because the task is statically divided in Num_Task
    -- while the openMP has the extra overhead of the thousands of tasks
    procedure Cholesky_Ada_Tasks (M : in out Blocked_Matrix_Type) is
        function Convert_to_C is new Ada.Unchecked_Conversion(Source=>Integer,
                                                              Target=>Interfaces.C.Int);
        BS_C: Interfaces.C.Int := Convert_to_C(BS);
        Num_Tasks: Integer := Num_Threads;
        Size: Integer := NB;
        Min_Chunk_Size : Integer := 1;
        
        protected type My_Barrier is
            entry Wait;
            procedure Finished;
            procedure Reset;
        private
            Finished_Tasks : Integer := 0;
            N_tasks: Integer := Num_Tasks;
        end My_Barrier;
        protected body My_Barrier is
            entry Wait when Finished_Tasks = N_tasks is
            begin
                Finished_Tasks := 0;
            end;
            procedure Finished is
            begin
                Finished_Tasks := Finished_Tasks + 1;
            end Finished;
            procedure Reset is
            begin
                Finished_Tasks := 0;
            end Reset;
        end My_Barrier;

        Phase_1, Phase_2: My_Barrier;

        protected type Phases_1 is
            entry Wait_1 (First_Pos, Last_Pos, K: out Integer; ToEnd: out Boolean);
            procedure Start_1 (First_Pos, Last_Pos, K: Integer);
            procedure Finished;
        private
            Local_First_Pos_1, Local_Last_Pos_1, Local_K_1: Integer;
            Local_Next_1 : Boolean := False;
            Local_ToEnd : Boolean := False;
        end Phases_1;
        protected body Phases_1 is
            entry Wait_1 (First_Pos, Last_Pos, K: out Integer; ToEnd: out Boolean)
            when Local_Next_1 = True or Local_ToEnd = True is
            begin
                First_Pos := Local_First_Pos_1;
                Last_Pos := Local_Last_Pos_1;
                K := Local_K_1;
                ToEnd := Local_ToEnd;
                Local_Next_1 := False;
            end Wait_1;

            procedure Start_1 (First_Pos, Last_Pos, K: Integer) is
            begin
                Local_First_Pos_1 := First_Pos;
                Local_Last_Pos_1 := Last_Pos;
                Local_K_1 := K;
                Local_Next_1 := True;
            end Start_1;

            procedure Finished is
            begin
                Local_ToEnd := True;
            end Finished;
        end Phases_1;

        protected type Phases_2 is
            entry Wait_2 (First_Pos, Last_Pos, K, I: out Integer; ToEnd: out Boolean);
            procedure Start_2 (First_Pos, Last_Pos, K, I: Integer);
            procedure Finished;
        private
            Local_First_Pos_2, Local_Last_Pos_2, Local_K_2, Local_I: Integer;
            Local_Next_2 : Boolean := False;
            Local_ToEnd : Boolean := False;
        end Phases_2;
        protected body Phases_2 is
            entry Wait_2 (First_Pos, Last_Pos, K, I: out Integer; ToEnd: out Boolean)
            when Local_Next_2 = True or Local_ToEnd = True is
            begin
                First_Pos := Local_First_Pos_2;
                Last_Pos := Local_Last_Pos_2;
                K := Local_K_2;
                I := Local_I;
                ToEnd := Local_ToEnd;
                Local_Next_2 := False;
            end Wait_2;

            procedure Start_2 (First_Pos, Last_Pos, K, I: Integer) is
            begin
                Local_First_Pos_2 := First_Pos;
                Local_Last_Pos_2 := Last_Pos;
                Local_K_2 := K;
                Local_I := I;
                Local_Next_2 := True;
            end Start_2;

            procedure Finished is
            begin
                Local_ToEnd := True;
            end Finished;
        end Phases_2;

        Process_Phases_1: array (0..Num_Tasks-1) of Phases_1;
        Process_Phases_2: array (0..Num_Tasks-1) of Phases_2;
        
        task type Process_1 is
            entry Id(My_Id: Integer);
        end Process_1;
        task body Process_1 is
            Local_First_Pos, Local_Last_Pos, Local_K: Integer;
            Local_Id : Integer;
            ToEnd: Boolean;
        begin
            accept Id(My_Id: Integer) do
                Local_Id := My_Id;
            end Id;

            loop
                Process_Phases_1(Local_Id).wait_1(Local_First_Pos, Local_Last_Pos, Local_K, ToEnd);
                exit when ToEnd = True;
                for I in Local_First_Pos .. Local_Last_Pos loop
                    Omp_trsm(M(Local_K, Local_K), M(Local_K, I), BS_C, BS_C);
                end loop;
                Phase_1.Finished; 
            end loop;
        end Process_1;

        task type Process_2 is
            entry Id(My_Id: Integer);
        end Process_2;
        task body Process_2 is
            Local_First_Pos, Local_Last_Pos, Local_K, Local_I: Integer;
            Local_Id : Integer;
            ToEnd: Boolean;
        begin
            accept Id(My_Id: Integer) do
                Local_Id := My_Id;
            end Id;

            loop
                Process_Phases_2(Local_Id).Wait_2(Local_First_Pos, Local_Last_Pos, Local_K, Local_I, ToEnd);
                exit when ToEnd = True;

                for J in Local_First_Pos .. Local_Last_Pos loop
                    Omp_gemm(M(Local_K, Local_I), M(Local_K, J), M(J, Local_I), BS_C, BS_C);
                end loop;
                Phase_2.Finished;
            end loop;
        end Process_2;

        First_Pos, Last_Pos: Integer;
        Temp_Size, Offset: Integer;
        Process_Tasks_1: array (0..Num_Tasks-1) of Process_1;
        Process_Tasks_2: array (0..Num_Tasks-1) of Process_2;
    begin
        for I in 0..Num_Tasks-1 loop
            Process_Tasks_1(I).Id(I);
            Process_Tasks_2(I).Id(I);            
        end loop;

        for K in 0 .. NB - 1 loop
            Omp_potrf(M(K, K), BS_C, BS_C);
            Temp_Size := (NB-1) - (K + 1) + 1;
            Offset := K+1;
            If Temp_Size < Min_Chunk_Size * Num_Tasks then
                for I in K + 1  .. NB-1 loop
                Omp_trsm(M(K, K), M(K, I), BS_C, BS_C);
                end loop;
            else
                for TI in 0..Num_Tasks-1 loop
                    First_Pos := Offset + TI * (Temp_Size / Num_Tasks);
                    Last_Pos := Offset + (TI+1) * (Temp_Size / Num_Tasks) - 1;
                    if TI = Num_Tasks-1 then
                        Last_Pos := Offset + Temp_Size - 1;
                    end if;
                    Process_Phases_1(TI).Start_1(First_Pos, Last_Pos, K);
                end loop;
                Phase_1.Wait; 
            end if;

            for I in K + 1 .. NB-1 loop
                If Temp_Size < Min_Chunk_Size * Num_Tasks then
                    for J in K + 1 .. I-1 loop
                        Omp_gemm(M(K, I), M(K, J), M(J, I), BS_C, BS_C);
                    end loop;
                else
                    for TI in 0..Num_Tasks-1 loop
                        First_Pos := Offset + TI * (Temp_Size / Num_Tasks);
                        Last_Pos := Offset + (TI+1) * (Temp_Size / Num_Tasks) - 1;
                        if TI = Num_Tasks-1 then
                            Last_Pos := Offset + Temp_Size - 1;
                        end if;
                        Process_Phases_2(TI).Start_2(First_Pos, Last_Pos, K, I);
                    end loop;
                    Phase_2.Wait;
                end if;
                Omp_syrk(M(K, I), M(I, I), BS_C, BS_C);
            end loop;
        end loop;

        for I in 0..Num_Tasks-1 loop
            Process_Phases_1(I).Finished;
            Process_Phases_2(I).Finished;
        end loop;
    end Cholesky_Ada_Tasks;

   -------------------------------------------------------
   -- Paraffin
   -------------------------------------------------------
   -- not quite fair, since paraffin does not create one tasklet per individual matrix element
   -- instead it divides each row in blocks
   -- so it is equivalent to an approach where each openmp tasks processes a block of elements
    procedure Cholesky_Paraffin (M : in out Blocked_Matrix_Type) is
        function Convert_to_C is new Ada.Unchecked_Conversion(Source=>Integer,
                                                              Target=>Interfaces.C.Int);
        BS_C: Interfaces.C.Int := Convert_to_C(BS);
        type Matrix_Dim is range 0 .. NB - 1;

        package Parallel_Loops is new Parallel.Loops (Matrix_Dim);
        package Iterate is new Parallel_Loops.Work_Sharing;

        Global_K, Global_I: Integer;

        procedure Generic_Iterate_Phase_1 (Start, Finish: Matrix_Dim) is
            Local_Start: Integer := Integer(Start);
            Local_Finish: Integer := Integer(Finish);
        begin
            for I in Local_Start .. Local_Finish loop
                Omp_trsm(M(Global_K, Global_K), M(Global_K, I), BS_C, BS_C);
            end loop;
        end Generic_Iterate_Phase_1;

        procedure Generic_Iterate_Phase_2 (Start, Finish: Matrix_Dim) is
            Local_Start: Integer := Integer(Start);
            Local_Finish: Integer := Integer(Finish);
        begin
            for J in Local_Start .. Local_Finish loop
                Omp_gemm(M(Global_K, Global_I), M(Global_K, J), M(J, Global_I), BS_C, BS_C);
            end loop;
        end Generic_Iterate_Phase_2;

        Num_Workers : Parallel.Worker_Count_Type := Parallel.Worker_Count_Type(Num_Threads);
        Min_Chunk_Size : Integer := 1;
        Manager : Iterate.Work_Sharing_Manager := Iterate.Create ;
        Temp_Size: Integer;
    begin
        for K in 0 .. NB-1  loop
            Global_K := K;
            Omp_potrf(M(K, K), BS_C, BS_C);
            Temp_Size := (NB-1) - (K + 1) + 1;
            If Temp_Size < Min_Chunk_Size * Integer(Num_Workers) then
                for I in K + 1  .. NB-1 loop
                    Omp_trsm(M(K, K), M(K, I), BS_C, BS_C);
                end loop;
            else
                Manager.Execute_Parallel_Loop
                (Process => Generic_Iterate_Phase_1'Access,
                From => Matrix_Dim(K + 1),
                Worker_Count => Num_Workers);
            end if;

            for I in K + 1 .. NB-1 loop
                Global_I := I;
                If Temp_Size < Min_Chunk_Size * Integer(Num_Workers) then
                    for J in K + 1 .. I-1 loop
                        Omp_gemm(M(K, I), M(K, J), M(J, I), BS_C, BS_C);
                    end loop;
                else
                    Manager.Execute_Parallel_Loop
                        (Process => Generic_Iterate_Phase_2'Access,
                        From => Matrix_Dim(K + 1),
                        Worker_Count => Num_Workers);
                end if;
                Omp_syrk(M(K, I), M(I, I), BS_C, BS_C);
            end loop;
        end loop;
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
                                                                    Target=>Blocked_Matrix_Access);
        Task_Data_Access: Tasks_Data_Access_Task_2_Type := Convert_to_Task(Task_Params);
        M_Access: Blocked_Matrix_Access := Convert_to_Matrix(Task_Data_Access.M_Address);

        function Convert_to_C is new Ada.Unchecked_Conversion(Source=>Integer,
                                                              Target=>Interfaces.C.Int);
        BS_C: Interfaces.C.Int;
    begin
        BS_C := Convert_to_C(Bs);
--         Extrae.Ada_Extrae_event(6000, 2);
        Omp_trsm(M_Access.all(Task_Data_Access.K, Task_Data_Access.K),
                 M_Access.all(Task_Data_Access.K, Task_Data_Access.I),
                 BS_C, BS_C);
--         Extrae.Ada_Extrae_event(6000, 0);
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
                                                                    Target=>Blocked_Matrix_Access);
        Task_Data_Access: Tasks_Data_Access_Task_3_Type := Convert_to_Task(Task_Params);
        M_Access: Blocked_Matrix_Access := Convert_to_Matrix(Task_Data_Access.M_Address);
        function Convert_to_C is new Ada.Unchecked_Conversion(Source=>Integer,
                                                              Target=>Interfaces.C.Int);
        BS_C: Interfaces.C.Int;
    begin
        BS_C := Convert_to_C(Bs);
--         Extrae.Ada_Extrae_event(6000, 3);
        Omp_gemm(M_Access.all(Task_Data_Access.K, Task_Data_Access.I),
                 M_Access.all(Task_Data_Access.K, Task_Data_Access.J),
                 M_Access.all(Task_Data_Access.J, Task_Data_Access.I),
                 BS_C, BS_C);
--         Extrae.Ada_Extrae_event(6000, 0);
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
                                                                    Target=>Blocked_Matrix_Access);
        Data_Access: Parallel_Data_Type_Access := Convert_to_Parallel(Parallel_Params);
        M_Access: Blocked_Matrix_Access := Convert_to_Matrix(Data_Access.M_Address);
        Depend_Clauses: OpenMP.Void_Ptr_Ptr := null;
        function Convert_to_C is new Ada.Unchecked_Conversion(Source=>Integer,
                                                              Target=>Interfaces.C.Int);
        BS_C: Interfaces.C.Int;
    begin
        if (OpenMP.Ada_OMP_Get_Thread_Num = 0)
        then
            BS_C := Convert_to_C(Bs);
            for K in 0 .. NB-1 loop
--                 Extrae.Ada_Extrae_event(6000, 1);
                Omp_potrf(M_Access.all(K, K), BS_C, BS_C);
--                 Extrae.Ada_Extrae_event(6000, 0);
             
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
                    for J in K + 1 .. I-1 loop
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
--                     Extrae.Ada_Extrae_event(6000, 4);
                    declare
                        Task_Data_Access: Tasks_Data_Access_Task_2_Type;
                        Task_Data_Address: System.Address;
                    begin
                        Task_Data_Access := new Tasks_Data_Type_Task_2;
                        Task_Data_Access.M_Address := Data_Access.M_Address;
                        Task_Data_Access.K := K;
                        Task_Data_Access.I := I;
                        Task_Data_Address := Task_Data_Access.all'Address;
                        OpenMP.Ada_GOMP_Task(Tasks_Function_Task_4'Unrestricted_Access, Task_Data_Address, null, 16, 8, TRUE, GOMP_TASK_UNTIED, Depend_Clauses, 0);
                    end;
--                     Extrae.Ada_Extrae_event(6000, 0);
                end loop;
            end loop;
        end if;
    end Parallel_Function_Tasks;

    procedure Process_Parallel_Tasks (M : in out Blocked_Matrix_Type) is
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
                                                                    Target=>Blocked_Matrix_Access);
        Task_Data_Access: Tasks_Data_Access_Task_1_Type := Convert_to_Task(Task_Params);
        M_Access: Blocked_Matrix_Access := Convert_to_Matrix(Task_Data_Access.M_Address);

        function Convert_to_C is new Ada.Unchecked_Conversion(Source=>Integer,
                                                              Target=>Interfaces.C.Int);
        BS_C: Interfaces.C.Int;
    begin
--         Extrae.Ada_Extrae_event(6000, 1);
        BS_C := Convert_to_C(Bs);
        Omp_potrf(M_Access.all(Task_Data_Access.K, Task_Data_Access.K),
                 BS_C, BS_C);
--         Extrae.Ada_Extrae_event(6000, 0);
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
                                                                    Target=>Blocked_Matrix_Access);
        Task_Data_Access: Tasks_Data_Access_Task_4_Type := Convert_to_Task(Task_Params);
        M_Access: Blocked_Matrix_Access := Convert_to_Matrix(Task_Data_Access.M_Address);

        function Convert_to_C is new Ada.Unchecked_Conversion(Source=>Integer,
                                                              Target=>Interfaces.C.Int);
        BS_C: Interfaces.C.Int;
    begin
--         Extrae.Ada_Extrae_event(6000, 4);
        BS_C := Convert_to_C(Bs);
        Omp_syrk(M_Access.all(Task_Data_Access.K, Task_Data_Access.I),
                 M_Access.all(Task_Data_Access.I, Task_Data_Access.I),
                 BS_C, BS_C);
--         Extrae.Ada_Extrae_event(6000, 0);
    end Tasks_Function_Task_4;

    procedure Parallel_Function_Deps(Parallel_Params: System.Address);
    pragma Convention(C, Parallel_Function_Deps);
    procedure Parallel_Function_Deps(Parallel_Params: System.Address) is
        function Convert_to_Parallel is new Ada.Unchecked_Conversion(Source=>System.Address,
                                                                    Target=>Parallel_Data_Type_Access);
        function Convert_to_Matrix is new Ada.Unchecked_Conversion(Source=>System.Address,
                                                                    Target=>Blocked_Matrix_Access);
        Data_Access: Parallel_Data_Type_Access := Convert_to_Parallel(Parallel_Params);
        M_Access: Blocked_Matrix_Access := Convert_to_Matrix(Data_Access.M_Address);

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
                        (DEPS_PTR_1U, DEPS_PTR_1U, Blocked_Submatrix_to_Void_Ptr(M_Access.all(K,K)));
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
                            (DEPS_PTR_2U, DEPS_PTR_1U, Blocked_Submatrix_to_Void_Ptr(M_Access.all(K,K)), Blocked_Submatrix_to_Void_Ptr(M_Access.all(K,I)));
                        Depend_Clauses : OpenMP.Void_Ptr_Ptr := Depend_Clauses_Array(1)'Unrestricted_Access;
                    begin
                        Task_Data_Access := new Tasks_Data_Type_Task_2;
                        Task_Data_Access.M_Address := Data_Access.M_Address;
                        Task_Data_Access.K := K;
                        Task_Data_Access.I := I;
                        Task_Data_Address := Task_Data_Access.all'Address;
                        OpenMP.Ada_GOMP_Task(Tasks_Function_Task_2'Unrestricted_Access, Task_Data_Address, null, 16, 8, TRUE, GOMP_TASK_UNTIED_DEPEND, Depend_Clauses, 0);
                    end;
                end loop;

                for I in K + 1 .. NB-1 loop
                    for J in K + 1 .. I-1 loop
                        declare
                            Task_Data_Access: Tasks_Data_Access_Task_3_Type;
                            Task_Data_Address: System.Address;
                            Depend_Clauses_Array: aliased array(1..5) of aliased OpenMP.Void_Ptr :=
                                (DEPS_PTR_3U, DEPS_PTR_1U, Blocked_Submatrix_to_Void_Ptr(M_Access.all(K,I)), Blocked_Submatrix_to_Void_Ptr(M_Access.all(K,J)), Blocked_Submatrix_to_Void_Ptr(M_Access.all(J,I)));
                            Depend_Clauses : OpenMP.Void_Ptr_Ptr := Depend_Clauses_Array(1)'Unrestricted_Access;
                        begin
                            Task_Data_Access := new Tasks_Data_Type_Task_3;
                            Task_Data_Access.M_Address := Data_Access.M_Address;
                            Task_Data_Access.K := K;
                            Task_Data_Access.I := I;
                            Task_Data_Access.J := J;
                            Task_Data_Address := Task_Data_Access.all'Address;
                            OpenMP.Ada_GOMP_Task(Tasks_Function_Task_3'Unrestricted_Access, Task_Data_Address, null, 20, 8, TRUE, GOMP_TASK_UNTIED_DEPEND, Depend_Clauses, 0);
                        end;
                    end loop;

                    declare
                        Task_Data_Access: Tasks_Data_Access_Task_4_Type;
                        Task_Data_Address: System.Address;
                        Depend_Clauses_Array: aliased array(1..4) of aliased OpenMP.Void_Ptr :=
                            (DEPS_PTR_2U, DEPS_PTR_1U, Blocked_Submatrix_to_Void_Ptr(M_Access.all(K,I)), Blocked_Submatrix_to_Void_Ptr(M_Access.all(I,I)));
                        Depend_Clauses : OpenMP.Void_Ptr_Ptr := Depend_Clauses_Array(1)'Unrestricted_Access;
                    begin
                        Task_Data_Access := new Tasks_Data_Type_Task_4;
                        Task_Data_Access.M_Address := Data_Access.M_Address;
                        Task_Data_Access.K := K;
                        Task_Data_Access.I := I;
                        Task_Data_Address := Task_Data_Access.all'Address;
                        OpenMP.Ada_GOMP_Task(Tasks_Function_Task_4'Unrestricted_Access, Task_Data_Address, null, 16, 8, TRUE, GOMP_TASK_UNTIED_DEPEND, Depend_Clauses, 0);
                    end;
                end loop;
            end loop;
        end if;
    end Parallel_Function_Deps;

    procedure Process_Parallel_Deps (M : in out Blocked_Matrix_Type) is
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
    procedure Initialize_matrix (M: in out Linear_Matrix_Type);
    pragma Import (Convention => C,
                    Entity => Initialize_matrix,
                    External_Name => "initialize_matrix");
    procedure Check_Factorization (M_orig: in out Linear_Matrix_Type;
                                   M_modif: in out Linear_Matrix_Type);
    pragma Import (Convention => C,
                    Entity => Check_Factorization,
                    External_Name => "check_factorization");
    procedure Convert_to_blocks (M_lin: in out Linear_Matrix_Type;
                                 M_block: in out Blocked_Matrix_Type);
    pragma Import (Convention => C,
                    Entity => Convert_to_blocks,
                    External_Name => "convert_to_blocks");
    procedure Convert_to_linear (M_block: in out Blocked_Matrix_Type;
                                 M_lin: in out Linear_Matrix_Type);
    pragma Import (Convention => C,
                    Entity => Convert_to_linear,
                    External_Name => "convert_to_linear");

    --------------------------------------------------
    -- Definition of program variables
    --------------------------------------------------

    -- Matrix
--     M_Access: Matrix_Access;
    Blocked_M_Access: Blocked_Matrix_Access;
    Linear_M_Access: Linear_Matrix_Access;
    Original_Linear_M_Access: Linear_Matrix_Access;

    -- Time variables
    Start_Time, Finish_Time: Time;

    package IO renames Ada.Text_IO;
    package CLI renames Ada.Command_Line;

begin

    --------------------------------------------------
    -- Read arguments
    --------------------------------------------------
    Nargs := CLI.Argument_Count;
    if Nargs /= 4 then
        IO.Put_Line ("Use: ./main version n_threads out_file.csv check");
        return;
    end if;

--     Extrae.Ada_Extrae_init;

    Num_threads := Integer'Value(CLI.Argument (Number => 2));

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

    Check := Boolean'Value(CLI.Argument (Number => 4));

    --------------------------------------------------
    -- Initialize matrices
    --------------------------------------------------
--     M_Access := new Matrix_Type;
    Linear_M_Access := new Linear_Matrix_Type;
    Original_Linear_M_Access := new Linear_Matrix_Type;

    Initialize_matrix(Linear_M_Access.all);
    Save_original_matrix(Linear_M_Access.all, Original_Linear_M_Access.all);

    Blocked_M_Access := new Blocked_Matrix_Type;
    for I in 0 .. NB-1 loop
        for J in 0 .. NB-1 loop
            Blocked_M_Access(I, J) := new Blocked_Submatrix_Type;
        end loop;
    end loop;
    Convert_to_blocks(Linear_M_Access.all, Blocked_M_Access.all);

    --------------------------------------------------
    -- Execution of Sequential
    --------------------------------------------------
    if CLI.Argument (Number => 1) = "seq" then
        Start_Time := Clock;
        Process_Sequential(Blocked_M_Access.all);
        Finish_Time := Clock;
        Put_Line("Time for sequential: " & Duration'Image(Finish_Time - Start_Time) & " seconds.");
        Put_Line(OutFile, Duration'Image(Finish_Time - Start_Time)&",");
    end if;

    --------------------------------------------------
    -- Execution of Ada tasks
    --------------------------------------------------
    if CLI.Argument (Number => 1) = "ada_tasks" then
        Start_Time := Clock;
        Cholesky_Ada_Tasks(Blocked_M_Access.all);
        Finish_Time := Clock;
        Put_Line("Time for ada tasks: " & Duration'Image(Finish_Time - Start_Time) & " seconds.");
        Put_Line(OutFile, Duration'Image(Finish_Time - Start_Time)&",");
    end if;

    --------------------------------------------------
    -- Execution of Paraffin
    --------------------------------------------------
    if CLI.Argument (Number => 1) = "ada_paraffin" then
        Start_Time := Clock;
        Cholesky_Paraffin(Blocked_M_Access.all);
        Finish_Time := Clock;
        Put_Line("Time for paraffin: " & Duration'Image(Finish_Time - Start_Time) & " seconds.");
        Put_Line(OutFile, Duration'Image(Finish_Time - Start_Time)&",");
    end if;

    --------------------------------------------------
    -- Execution of Parallel tasks
    --------------------------------------------------
    if CLI.Argument (Number => 1) = "ada_omp_tasks" then
        Start_Time := Clock;
        Process_Parallel_Tasks(Blocked_M_Access.all);
        Finish_Time := Clock;
        Put_Line("Time for parallel tasks: " & Duration'Image(Finish_Time - Start_Time) & " seconds.");
        Put_Line(OutFile, Duration'Image(Finish_Time - Start_Time)&",");
    end if;

    --------------------------------------------------
    -- Execution of Parallel deps
    --------------------------------------------------
    if CLI.Argument (Number => 1) = "ada_omp_deps" then
        Start_Time := Clock;
        Process_Parallel_Deps(Blocked_M_Access.all);
        Finish_Time := Clock;
        Put_Line("Time for parallel deps: " & Duration'Image(Finish_Time - Start_Time) & " seconds.");
        Put_Line(OutFile, Duration'Image(Finish_Time - Start_Time)&",");
    end if;

--     Extrae.Ada_Extrae_fini;

    Convert_to_linear(Blocked_M_Access.all, Linear_M_Access.all);

    --------------------------------------------------
    -- Check results if applies
    --------------------------------------------------
    if Check then
        Check_Factorization(Original_Linear_M_Access.all, Linear_M_Access.all);
    end if;

end Main;
