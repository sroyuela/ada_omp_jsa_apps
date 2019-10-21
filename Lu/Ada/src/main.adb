
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

    pragma Suppress(All_Checks);
    
    ----------------------------------------------------
    -- Definition of constants, types and auxiliary renames and packages
    ----------------------------------------------------

    -- Matrix type
    -- pragma import does not work with C defines, so we have to define new variables for the limits
    -- chaning here also needs to be changed in sparselu.h

    S: constant Integer := 64;
    BS: constant Integer := 32;

    type Submatrix_Type is array (0 .. BS-1, 0 .. BS-1) of aliased Float;
    type Matrix_Type is array ( 0 .. S-1, 0 .. S-1) of aliased Submatrix_Type;
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

    type Float_Access is access all Float;
    pragma Convention(C, Float_Access);

    function Float_Access_to_Void_Ptr is new Ada.Unchecked_Conversion(Source => Float_Access, Target => OpenMP.Void_Ptr);

    pragma Convention(C, Submatrix_Type);
    function Submatrix_to_Void_Ptr is new Ada.Unchecked_Conversion(Source => Submatrix_Type, Target => OpenMP.Void_Ptr);


    -- Global values to parametrize the executions
    Nargs : Integer;
    Num_Threads : Integer;
    OutFile: File_Type;   -- Global values to parametrize the executions

        
    --------------------------------------------------
    -- Importing C changed functions
    -------------------------------------------------

    procedure Changed_Lu0 (M: Matrix_Type; KK: Integer);
    pragma Import (Convention => C,
                    Entity => Changed_Lu0,
                    External_Name => "changed_lu0");

    procedure Changed_Bdiv (M: Matrix_Type; KK: Integer; II: Integer);
    pragma Import (Convention => C,
                    Entity => Changed_Bdiv,
                    External_Name => "changed_bdiv");

    procedure Changed_Bmod (M: Matrix_Type; KK: Integer; II: Integer; JJ: Integer);
    pragma Import (Convention => C,
                    Entity => Changed_Bmod,
                    External_Name => "changed_bmod");

    procedure Changed_Fwd (M: Matrix_Type; KK: Integer; JJ: Integer);
    pragma Import (Convention => C,
                    Entity => Changed_Fwd,
                    External_Name => "changed_fwd");

    procedure Lu0 (M: Submatrix_Type);
    pragma Import (Convention => C,
                    Entity => Lu0,
                    External_Name => "lu0");

    procedure Bdiv (Diag: Submatrix_Type; Row: Submatrix_Type);
    pragma Import (Convention => C,
                    Entity => Bdiv,
                    External_Name => "bdiv");

    procedure Bmod (Row: Submatrix_Type; Col: Submatrix_Type; Inner: Submatrix_Type);
    pragma Import (Convention => C,
                    Entity => Bmod,
                    External_Name => "bmod");

    procedure Fwd (Diag: Submatrix_Type; Col: Submatrix_Type);
    pragma Import (Convention => C,
                    Entity => Fwd,
                    External_Name => "fwd");

   -------------------------------------------------------
   -- Ada Tasks
   -------------------------------------------------------

   -- to be fair with OpenMP, tasks are created inside the timing measurement
   -- like the openmp thread pool
   -- however it is not fair because the task is statically divided in Num_Task
   -- while the openMP has the extra overhead of the thousands of tasks

    procedure LU_Ada_Tasks (M : in out Matrix_Type) is
        Num_Tasks: Integer := Num_Threads;
        Min_Chunk_Size : Integer := 1;
        Size: Integer := S;

        protected type My_Barrier is
            entry Wait;
            procedure Finished;
            procedure Reset;
        private
            Finished_Tasks : Integer := 0;
            N_Tasks: Integer := Num_Tasks;
        end My_Barrier;
        protected body My_Barrier is
            entry Wait when Finished_Tasks = N_Tasks is
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

        Phase_1, Phase_2, Phase_3: My_Barrier;

        protected type Phases is
            entry Wait_1 (First_Pos, Last_Pos, KK: out Integer; ToEnd: out Boolean);
            procedure Start_1 (First_Pos, Last_Pos, KK: Integer);

            entry Wait_2 (First_Pos, Last_Pos, KK: out Integer; ToEnd: out Boolean);
            procedure Start_2 (First_Pos, Last_Pos, KK: Integer);

            entry Wait_3 (First_Pos, Last_Pos, KK: out Integer; ToEnd: out Boolean);
            procedure Start_3 (First_Pos, Last_Pos, KK: Integer);

            procedure Finished;
        private
            Local_First_Pos_1, Local_Last_Pos_1, Local_KK_1: Integer;
            Local_Next_1 : Boolean := False;
            Local_First_Pos_2, Local_Last_Pos_2, Local_KK_2: Integer;
            Local_Next_2 : Boolean := False;
            Local_First_Pos_3, Local_Last_Pos_3, Local_KK_3: Integer;
            Local_Next_3 : Boolean := False;

            Local_ToEnd : Boolean := False;
        end Phases;

        protected body Phases is
            entry Wait_1 (First_Pos, Last_Pos, KK: out Integer; ToEnd: out Boolean)
            when Local_Next_1 = True or Local_ToEnd = True is
            begin
                First_Pos := Local_First_Pos_1;
                Last_Pos := Local_Last_Pos_1;
                KK := Local_KK_1;
                ToEnd := Local_ToEnd;
                Local_Next_1 := False;
            end Wait_1;

            procedure Start_1 (First_Pos, Last_Pos, KK: Integer) is
            begin
                Local_First_Pos_1 := First_Pos;
                Local_Last_Pos_1 := Last_Pos;
                Local_KK_1 := KK;
                Local_Next_1 := True;
            end Start_1;

            entry Wait_2 (First_Pos, Last_Pos, KK: out Integer; ToEnd: out Boolean)
            when Local_Next_2 = True or Local_ToEnd = True is
            begin
                First_Pos := Local_First_Pos_2;
                Last_Pos := Local_Last_Pos_2;
                KK := Local_KK_2;
                ToEnd := Local_ToEnd;
                Local_Next_2 := False;
            end Wait_2;

            procedure Start_2 (First_Pos, Last_Pos, KK: Integer) is
            begin
                Local_First_Pos_2 := First_Pos;
                Local_Last_Pos_2 := Last_Pos;
                Local_KK_2 := KK;
                Local_Next_2 := True;
            end Start_2;

            entry Wait_3 (First_Pos, Last_Pos, KK: out Integer; ToEnd: out Boolean)
            when Local_Next_3 = True or Local_ToEnd = True is
            begin
                First_Pos := Local_First_Pos_3;
                Last_Pos := Local_Last_Pos_3;
                KK := Local_KK_3;
                ToEnd := Local_ToEnd;
                Local_Next_3 := False;
            end Wait_3;

            procedure Start_3 (First_Pos, Last_Pos, KK: Integer) is
            begin
                Local_First_Pos_3 := First_Pos;
                Local_Last_Pos_3 := Last_Pos;
                Local_KK_3 := KK;
                Local_Next_3 := True;
            end Start_3;

            procedure Finished is
            begin
                Local_ToEnd := True;
            end Finished;
        end Phases;

        Process_Phases: array (0..Num_Tasks-1) of Phases;

        task type Process is
            entry Id(My_Id: Integer);
        end Process;

        task body Process is
            Local_First_Pos, Local_Last_Pos, Local_KK: Integer;
            Local_Id : Integer;
            ToEnd: Boolean;
        begin
            accept Id(My_Id: Integer) do
                Local_Id := My_Id;
            end Id;

            loop
                Process_Phases(Local_Id).wait_1(Local_First_Pos, Local_Last_Pos, Local_KK, ToEnd);
                exit when ToEnd = True;

                for JJ in Local_First_Pos .. Local_Last_Pos loop
                    Changed_Fwd(M, Local_KK, JJ);
                end loop;
                -- Phase_1.Finished; -- not needed as there is a nowait

                Process_Phases(Local_Id).wait_2(Local_First_Pos, Local_Last_Pos, Local_KK, ToEnd);
                exit when ToEnd = True;

                for II in Local_First_Pos .. Local_Last_Pos loop
                    Changed_Bdiv(M, Local_KK, II);
                end loop;

                Phase_2.Finished;

                Process_Phases(Local_Id).wait_3(Local_First_Pos, Local_Last_Pos, Local_KK, ToEnd);
                exit when ToEnd = True;

                for II in Local_First_Pos .. Local_Last_Pos loop
                    for JJ in Local_KK + 1 .. S -1 loop
                        Changed_Bmod(M, Local_KK, II, JJ);
                    end loop;
                end loop;

                Phase_3.Finished;
            end loop;
        end Process;

        First_Pos, Last_Pos: Integer;

        Process_Tasks: array (0..Num_Tasks-1) of Process;
        Temp_Size, Offset: Integer;
    begin
        for I in 0..Num_Tasks-1 loop
            Process_Tasks(I).Id(I);
        end loop;


        for KK in 0 .. S - 1 loop
            Changed_Lu0(M, KK);

            Temp_Size := (S-1) - (KK + 1) + 1;
            Offset := KK +1;

            --Put_Line("Size = " & Integer'Image(Temp_Size)& ", Offset = " & Integer'Image(Offset));

            If Temp_Size < Min_Chunk_Size * Num_Tasks then
                for JJ in KK+1 .. S -1 loop
                    Changed_Fwd(M, KK, JJ);
                end loop;
                for II in KK+1 .. S-1 loop
                    Changed_Bdiv(M, KK, II);
                end loop;
                for II in KK +1 .. S -1 loop
                    for JJ in KK + 1 .. S -1 loop
                        Changed_Bmod(M, KK, II, JJ);
                    end loop;
                end loop;
            else
                for I in 0..Num_Tasks-1 loop
                    First_Pos := Offset + I * (Temp_Size / Num_Tasks);
                    Last_Pos := Offset + (I+1) * (Temp_Size / Num_Tasks) - 1;
                    if I = Num_Tasks-1 then
                        Last_Pos := Offset + Temp_Size - 1;
                    end if;
                    -- Put_Line("First = " & Integer'Image(First_Pos)& ", Last = " & Integer'Image(Last_Pos));
                    Process_Phases(I).Start_1(First_Pos, Last_Pos, KK);
                end loop;

                -- Phase_1.Wait; -- not needed as there is a nowait
                -- Put_Line("Phase 1 finished");

                for I in 0..Num_Tasks-1 loop
                    First_Pos := Offset + I * (Temp_Size / Num_Tasks);
                    Last_Pos := Offset + (I+1) * (Temp_Size / Num_Tasks) - 1;
                    if I = Num_Tasks-1 then
                        Last_Pos := Offset + Temp_Size - 1;
                    end if;
                    Process_Phases(I).Start_2(First_Pos, Last_Pos, KK);
                end loop;

                Phase_2.Wait;

                for I in 0..Num_Tasks-1 loop
                    First_Pos := Offset + I * (Temp_Size / Num_Tasks);
                    Last_Pos := Offset + (I+1) * (Temp_Size / Num_Tasks) - 1;
                    if I = Num_Tasks-1 then
                        Last_Pos := Offset + Temp_Size - 1;
                    end if;
                    Process_Phases(I).Start_3(First_Pos, Last_Pos, KK);
                end loop;
                Phase_3.Wait;
            end if;
        end loop;
        for I in 0..Num_Tasks-1 loop
            Process_Phases(I).Finished;
        end loop;
    end LU_Ada_Tasks;


   -------------------------------------------------------
   -- Paraffin
   -------------------------------------------------------

   -- not quite fair, since paraffin does not create one tasklet per individual matrix element
   -- instead it divides each row in blocks
   -- so it is equivalent to an approach where each openmp tasks processes a block of elements

    procedure LU_Paraffin (M : in out Matrix_Type) is
        type Matrix_Dim is range 0 .. S - 1;
        Min_Chunk_Size : Integer := 1;

        package Parallel_Loops is new Parallel.Loops (Matrix_Dim);
        package Iterate is new Parallel_Loops.Work_Sharing;

        Num_Workers : Parallel.Worker_Count_Type := Parallel.Worker_Count_Type(Num_Threads);
        Global_KK: Integer := 0;

        procedure Generic_Iterate_Phase_1_2 (Start, Finish: Matrix_Dim) is
            Local_Start: Integer := Integer(Start);
            Local_Finish: Integer := Integer(Finish);
        begin
            for JJ in Local_Start .. Local_Finish loop
                Changed_Fwd(M, Global_KK, JJ);
            end loop;
            for II in Local_Start .. Local_Finish loop
                Changed_Bdiv(M, Global_KK, II);
            end loop;
        end Generic_Iterate_Phase_1_2;

        procedure Generic_Iterate_Phase_3 (Start, Finish: Matrix_Dim) is
            Local_Start: Integer := Integer(Start);
            Local_Finish: Integer := Integer(Finish);
        begin
            for II in Local_Start .. Local_Finish loop
                for JJ in Global_KK + 1 .. S -1 loop
                    Changed_Bmod(M, Global_KK, II, JJ);
                end loop;
            end loop;
        end Generic_Iterate_Phase_3;

        Manager : Iterate.Work_Sharing_Manager := Iterate.Create ;
        Temp_Size: Integer;
    begin
        for KK in 0 .. S - 1 loop
            Changed_Lu0(M, KK);
            Global_KK := KK;
            Temp_Size := (S-1) - (KK + 1) + 1;

            If Temp_Size < Min_Chunk_Size * Integer(Num_Workers) then
                for JJ in KK+1 .. S -1 loop
                    Changed_Fwd(M, KK, JJ);
                end loop;
                for II in KK+1 .. S-1 loop
                    Changed_Bdiv(M, KK, II);
                end loop;
                for II in KK +1 .. S -1 loop
                    for JJ in KK + 1 .. S -1 loop
                        Changed_Bmod(M, KK, II, JJ);
                    end loop;
                end loop;
            else
                Manager.Execute_Parallel_Loop
                    (Process => Generic_Iterate_Phase_1_2'Access,
                    From => Matrix_Dim(KK + 1),
                    Worker_Count => Num_Workers);

                Manager.Execute_Parallel_Loop
                    (Process => Generic_Iterate_Phase_3'Access,
                    From => Matrix_Dim(KK + 1),
                    Worker_Count => Num_Workers);
            end if;
        end loop;
    end LU_Paraffin;


    -------------------------------------------------------
    -- parallel Tasks
    -------------------------------------------------------

    type Tasks_Data_Type_Task_2 is -- 16 bytes
        record
            M_Address: System.Address; -- procedure uses in out parameters, so passing by pointer
            KK: Integer;
            JJ: Integer;
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
    begin
--         Extrae.Ada_Extrae_event(6000, 2);
        Fwd(M_Access.all(Task_Data_Access.KK, Task_Data_Access.KK), M_Access.all(Task_Data_Access.KK, Task_Data_Access.JJ));
--         Extrae.Ada_Extrae_event(6000, 0);
    end Tasks_Function_Task_2;


    type Tasks_Data_Type_Task_3 is -- 16 bytes
        record
            M_Address: System.Address; -- procedure uses in out parameters, so passing by pointer
            KK: Integer;
            II: Integer;
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
    begin
--         Extrae.Ada_Extrae_event(6000, 3);
        Bdiv(M_Access.all(Task_Data_Access.KK, Task_Data_Access.KK), M_Access.all(Task_Data_Access.II, Task_Data_Access.KK));
--         Extrae.Ada_Extrae_event(6000, 0);
    end Tasks_Function_Task_3;

    type Tasks_Data_Type_Task_4 is -- 20 bytes
        record
            M_Address: System.Address; -- procedure uses in out parameters, so passing by pointer
            KK: Integer;
            JJ: Integer;
            II: Integer;
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
    begin
--         Extrae.Ada_Extrae_event(6000, 4);
        Bmod(M_Access.all(Task_Data_Access.II, Task_Data_Access.KK), M_Access.all(Task_Data_Access.KK, Task_Data_Access.JJ), M_Access.all(Task_Data_Access.II, Task_Data_Access.JJ));
--         Extrae.Ada_Extrae_event(6000, 0);
    end Tasks_Function_Task_4;

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
    begin
        if (OpenMP.Ada_OMP_Get_Thread_Num = 0) then
            for KK in 0 .. S-1 loop
--                 Extrae.Ada_Extrae_event(6000, 1);
                Lu0(M_Access.all(KK, KK));
--                 Extrae.Ada_Extrae_event(6000, 0);
             
                for JJ in KK + 1 .. S-1 loop
                declare
                    Task_Data_Access: Tasks_Data_Access_Task_2_Type;
                    Task_Data_Address: System.Address;
                begin
                    Task_Data_Access := new Tasks_Data_Type_Task_2;
                    Task_Data_Access.M_Address := Data_Access.M_Address;
                    Task_Data_Access.KK := KK;
                    Task_Data_Access.JJ := JJ;
                    Task_Data_Address := Task_Data_Access.all'Address;
                    OpenMP.Ada_GOMP_Task( Tasks_Function_Task_2'Unrestricted_Access, Task_Data_Address, null, 16, 8, TRUE, GOMP_TASK_UNTIED, Depend_Clauses, 0); -- is alignment 4 or 8?
                end;
                end loop;

                for II in KK + 1 .. S -1 loop
                declare
                    Task_Data_Access: Tasks_Data_Access_Task_3_Type;
                    Task_Data_Address: System.Address;
                begin
                    Task_Data_Access := new Tasks_Data_Type_Task_3;
                    Task_Data_Access.M_Address := Data_Access.M_Address;
                    Task_Data_Access.KK := KK;
                    Task_Data_Access.II := II;
                    Task_Data_Address := Task_Data_Access.all'Address;
                    OpenMP.Ada_GOMP_Task( Tasks_Function_Task_3'Unrestricted_Access, Task_Data_Address, null, 16, 8, TRUE, GOMP_TASK_UNTIED, Depend_Clauses, 0); -- is alignment 4 or 8?
                end;
                end loop;

                OpenMP.Ada_GOMP_Taskwait;

                for II in KK + 1 .. S -1 loop
                    for JJ in KK + 1 .. S-1 loop
                        declare
                            Task_Data_Access: Tasks_Data_Access_Task_4_Type;
                            Task_Data_Address: System.Address;
                        begin
                            Task_Data_Access := new Tasks_Data_Type_Task_4;
                            Task_Data_Access.M_Address := Data_Access.M_Address;
                            Task_Data_Access.KK := KK;
                            Task_Data_Access.JJ := JJ;
                            Task_Data_Access.II := II;
                            Task_Data_Address := Task_Data_Access.all'Address;
                            OpenMP.Ada_GOMP_Task( Tasks_Function_Task_4'Unrestricted_Access, Task_Data_Address, null, 20, 8, TRUE, GOMP_TASK_UNTIED, Depend_Clauses, 0); -- is alignment 4 or 8?
                        end;
                    end loop;
                end loop;
                OpenMP.Ada_GOMP_Taskwait;
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
        --Put_Line("Calling end ...");
        OpenMP.Ada_GOMP_parallel_end;
    end Process_Parallel_Tasks;

    -------------------------------------------------------
    -- parallel Deps
    -------------------------------------------------------

    -- openmp task data types and code is the same as in the tasks version plus type 1
    type Tasks_Data_Type_Task_1 is -- 12 bytes
        record
            M_Address: System.Address; -- procedure uses in out parameters, so passing by pointer
            KK: Integer;
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
    begin
--         Extrae.Ada_Extrae_event(6000, 1);
        Changed_Lu0(M_Access.all, Task_Data_Access.KK);
--         Extrae.Ada_Extrae_event(6000, 0);
    end Tasks_Function_Task_1;

    procedure Parallel_Function_Deps(Parallel_Params: System.Address);
    pragma Convention(C, Parallel_Function_Deps);
    procedure Parallel_Function_Deps(Parallel_Params: System.Address) is
        function Convert_to_Parallel is new Ada.Unchecked_Conversion(Source=>System.Address,
                                                                    Target=>Parallel_Data_Type_Access);
        function Convert_to_Matrix is new Ada.Unchecked_Conversion(Source=>System.Address,
                                                                    Target=>Matrix_Access);
        Data_Access: Parallel_Data_Type_Access := Convert_to_Parallel(Parallel_Params);
        M_Access: Matrix_Access := Convert_to_Matrix(Data_Access.M_Address);

    begin
        if (OpenMP.Ada_OMP_Get_Thread_Num = 0) then
            for KK in 0 .. S-1 loop
                declare
                    Task_Data_Access: Tasks_Data_Access_Task_1_Type;
                    Task_Data_Address: System.Address;
                    Depend_Clauses_Array: aliased array(1..3) of aliased OpenMP.Void_Ptr :=
                        (DEPS_PTR_1U, DEPS_PTR_1U, Submatrix_to_Void_Ptr(M_Access.all(kk,kk)));
                    -- void *task_deps_1[3L] = {[0] = (void *)1U, [1] = (void *)1U, [2] = (*M)[kk][kk]};
                    Depend_Clauses : OpenMP.Void_Ptr_Ptr := Depend_Clauses_Array(1)'Unrestricted_Access;
                begin
                    Task_Data_Access := new Tasks_Data_Type_Task_1;
                    Task_Data_Access.M_Address := Data_Access.M_Address;
                    Task_Data_Access.KK := KK;
                    Task_Data_Address := Task_Data_Access.all'Address;

                    OpenMP.Ada_GOMP_Task(Tasks_Function_Task_1'Unrestricted_Access, Task_Data_Address, null, 12, 8, TRUE, GOMP_TASK_UNTIED_DEPEND, Depend_Clauses, 0); -- is alignment 4 or 8?
                end;

                for JJ in KK + 1 .. S-1 loop
                    declare
                        Task_Data_Access: Tasks_Data_Access_Task_2_Type;
                        Task_Data_Address: System.Address;
                        Depend_Clauses_Array: aliased array(1..4) of aliased OpenMP.Void_Ptr:=
                        (DEPS_PTR_2U, DEPS_PTR_1U, Submatrix_to_Void_Ptr(M_Access.all(kk,kk)), Submatrix_to_Void_Ptr(M_Access.all(kk,jj)));

                        -- void *task_deps_2[4L] = {[0] = (void *)2U, [1] = (void *)1U, [2] = (*M)[kk][kk], [3] = (*M)[kk][jj]};
                        Depend_Clauses : OpenMP.Void_Ptr_Ptr := Depend_Clauses_Array(1)'Unrestricted_Access;
                    begin
                        Task_Data_Access := new Tasks_Data_Type_Task_2;
                        Task_Data_Access.M_Address := Data_Access.M_Address;
                        Task_Data_Access.KK := KK;
                        Task_Data_Access.JJ := JJ;
                        Task_Data_Address := Task_Data_Access.all'Address;
                        OpenMP.Ada_GOMP_Task( Tasks_Function_Task_2'Unrestricted_Access, Task_Data_Address, null, 16, 8, TRUE, GOMP_TASK_UNTIED_DEPEND, Depend_Clauses, 0); -- is alignment 4 or 8?
                    end;
                end loop;

                for II in KK + 1 .. S -1 loop
                    declare
                        Task_Data_Access: Tasks_Data_Access_Task_3_Type;
                        Task_Data_Address: System.Address;
                        Depend_Clauses_Array: aliased array(1..4) of aliased OpenMP.Void_Ptr:=
                        (DEPS_PTR_2U, DEPS_PTR_1U, Submatrix_to_Void_Ptr(M_Access.all(kk,kk)), Submatrix_to_Void_Ptr(M_Access.all(ii,kk)));
                        -- void *task_deps_3[4L] = {[0] = (void *)2U, [1] = (void *)1U, [2] = (*M)[kk][kk], [3] = (*M)[ii][kk]};
                        Depend_Clauses : OpenMP.Void_Ptr_Ptr := Depend_Clauses_Array(1)'Unrestricted_Access;

                    begin
                        Task_Data_Access := new Tasks_Data_Type_Task_3;
                        Task_Data_Access.M_Address := Data_Access.M_Address;
                        Task_Data_Access.KK := KK;
                        Task_Data_Access.II := II;
                        Task_Data_Address := Task_Data_Access.all'Address;
                        OpenMP.Ada_GOMP_Task( Tasks_Function_Task_3'Unrestricted_Access, Task_Data_Address, null, 16, 8, TRUE, GOMP_TASK_UNTIED_DEPEND, Depend_Clauses, 0); -- is alignment 4 or 8?
                    end;
                end loop;

                for II in KK + 1 .. S -1 loop
                    for JJ in KK + 1 .. S-1 loop
                        declare
                            Task_Data_Access: Tasks_Data_Access_Task_4_Type;
                            Task_Data_Address: System.Address;
                            Depend_Clauses_Array: aliased array(1..5) of aliased OpenMP.Void_Ptr:=
                            (DEPS_PTR_3U, DEPS_PTR_1U, Submatrix_to_Void_Ptr(M_Access.all(ii,kk)), Submatrix_to_Void_Ptr(M_Access.all(kk,jj)),Submatrix_to_Void_Ptr(M_Access.all(ii,jj)));
                            -- void *task_deps_4[5L] = {[0] = (void *)3U, [1] = (void *)1U, [2] = (*M)[ii][kk], [3] = (*M)[kk][jj], [4] = (*M)[ii][jj]};
                            Depend_Clauses : OpenMP.Void_Ptr_Ptr := Depend_Clauses_Array(1)'Unrestricted_Access;

                        begin
                            Task_Data_Access := new Tasks_Data_Type_Task_4;
                            Task_Data_Access.M_Address := Data_Access.M_Address;
                            Task_Data_Access.KK := KK;
                            Task_Data_Access.JJ := JJ;
                            Task_Data_Access.II := II;
                            Task_Data_Address := Task_Data_Access.all'Address;
                            OpenMP.Ada_GOMP_Task( Tasks_Function_Task_4'Unrestricted_Access, Task_Data_Address, null, 20, 8, TRUE, GOMP_TASK_UNTIED_DEPEND, Depend_Clauses, 0); -- is alignment 4 or 8?
                        end;
                    end loop;
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
        --Put_Line("Calling end ...");
        OpenMP.Ada_GOMP_parallel_end;
     end Process_Parallel_Deps;


    --------------------------------------------------
    -- Auxiliary subprograms
    -- Init external to initialize the matrix
    -- Print prints the matrix
    --------------------------------------------------

    --  Declare an Ada procedure spec for sparselu_init, then use
    --  C function sparselu_init for the implementation.
    procedure Init (M: in out Matrix_Type);
    pragma Import (Convention => C,
                    Entity => Init,
                    External_Name => "sparselu_init");
    procedure Print_Matrix (M: Matrix_Type);
    pragma Import (Convention => C,
                    Entity => Print_Matrix,
                    External_Name => "sparselu_print");
    procedure Check_Matrix (M2: Matrix_Type; M1: Matrix_Type);
    pragma Import (Convention => C,
                    Entity => Check_Matrix,
                    External_Name => "sparselu_check");
    procedure SparseLU_Seq (M: Matrix_Type);
    pragma Import (Convention => C,
                    Entity => SparseLU_Seq,
                    External_Name => "sparselu_seq");

    --------------------------------------------------
    -- Directly calling C parallel functions
    -------------------------------------------------

    procedure SparseLU_Par_Tasks (M: Matrix_Type; NTH: Interfaces.C.unsigned);
    pragma Import (Convention => C,
                    Entity => SparseLU_Par_Tasks,
                    External_Name => "sparselu_task");
    procedure SparseLU_Par_Deps (M: Matrix_Type; NTH: Interfaces.C.unsigned);
    pragma Import (Convention => C,
                    Entity => SparseLU_Par_Deps,
                    External_Name => "sparselu_deps");

    --------------------------------------------------
    -- Definition of program variables
    --------------------------------------------------

    -- Matrix
    M_Access, M_old: Matrix_Access;

    -- Time variables
    Start_Time, Finish_Time: Time;

    package IO renames Ada.Text_IO;
    package CLI renames Ada.Command_Line;

    function Convert_to_C is new Ada.Unchecked_Conversion(Source=>Integer,
                                                          Target=>Interfaces.C.unsigned);
    Num_Threads_C: Interfaces.C.unsigned;

begin

    -- Read arguments
    Nargs := CLI.Argument_Count;
    if Nargs /= 3 then
        IO.Put_Line ("Use: ./main version n_threads out_file.csv");
        return;
    end if;

--     Extrae.Ada_Extrae_init;

    Num_Threads := Integer'Value(CLI.Argument (Number => 2));
    Num_Threads_C := Convert_to_C(Num_Threads);

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

    --------------------------------------------------
    -- Execution of Sequential
    --------------------------------------------------
    if CLI.Argument (Number => 1) = "seq" then
        -- Put_Line("Sequential");
        Init(M_Access.all);
        --Print_Matrix(M_Access.all);

        Start_Time := Clock;
        SparseLU_Seq(M_Access.all);
        Finish_Time := Clock;
        --Print_Matrix(M_Access.all);
        Put_Line("Time for sequential: " & Duration'Image(Finish_Time - Start_Time) & " seconds.");
        Put_Line(OutFile, Duration'Image(Finish_Time - Start_Time)&",");
        M_old.all := M_Access.all;
    end if;
   
    --------------------------------------------------
    -- Execution with the C parallel functions (called from Ada)
    --------------------------------------------------
    -- Execution of Parallel tasks
    if CLI.Argument (Number => 1) = "c_omp_tasks" then
        -- Put_Line("With C parallel code called from Ada");
        Init(M_Access.all);
        --Print_Matrix(M_Access.all);
        Start_Time := Clock;
        SparseLU_Par_Tasks(M_Access.all, Num_Threads_C); -- directly calling C sparselu_task function
        --Print_Matrix(M_Access.all);
        Finish_Time := Clock;

        Put_Line("Time for parallel tasks: " & Duration'Image(Finish_Time - Start_Time) & " seconds.");
        Put_Line(OutFile, Duration'Image(Finish_Time - Start_Time)&",");
        --Print_Matrix(M_Access.all);
    end if;

   -- Execution of Parallel deps
    if CLI.Argument (Number => 1) = "c_omp_deps" then
        Init(M_Access.all);
        Start_Time := Clock;
        SparseLU_Par_Deps(M_Access.all, Num_Threads_C);
        Finish_Time := Clock;
        Put_Line("Time for parallel deps: " & Duration'Image(Finish_Time - Start_Time) & " seconds.");
        Put_Line(OutFile, Duration'Image(Finish_Time - Start_Time)&",");
    end if;


    --------------------------------------------------
    -- Execution with the new Ada parallel functions
    --------------------------------------------------
    -- Put_Line("With Ada parallel code");
    -- Execution of Parallel tasks
    if CLI.Argument (Number => 1) = "ada_omp_tasks" then
        Init(M_Access.all);
        --Print_Matrix(M_Access.all);
        Start_Time := Clock;
        Process_Parallel_Tasks (M_Access.all);
        --Print_Matrix(M_Access.all);
        Finish_Time := Clock;
        Put_Line("Time for parallel tasks: " & Duration'Image(Finish_Time - Start_Time) & " seconds.");
        Put_Line(OutFile, Duration'Image(Finish_Time - Start_Time)&",");
        --Print_Matrix(M_Access.all);
    end if;

    -- Execution of Parallel deps
    if CLI.Argument (Number => 1) = "ada_omp_deps" then
        Init(M_Access.all);
        Start_Time := Clock;
        Process_Parallel_Deps (M_Access.all);
        Finish_Time := Clock;
        Put_Line("Time for parallel deps: " & Duration'Image(Finish_Time - Start_Time) & " seconds.");
        Put_Line(OutFile, Duration'Image(Finish_Time - Start_Time)&",");
        -- Put_Line("Checking ...");
        -- Check_Matrix(M_Access.all, M_old.all);
    end if;

    --------------------------------------------------
    -- Execution with Ada tasks
        --------------------------------------------------
    if CLI.Argument (Number => 1) = "ada_tasks" then
        -- Put_Line("With Ada Tasks");
        Init(M_Access.all);
        --Print_Matrix(M_Access.all);
        Start_Time := Clock;
        LU_Ada_Tasks(M_Access.all);
        Finish_Time := Clock;
        --Print_Matrix(M_Access.all);
        Put_Line("Time for ada tasks: " & Duration'Image(Finish_Time - Start_Time) & " seconds.");
        Put_Line(OutFile, Duration'Image(Finish_Time - Start_Time)&",");
    end if;

    --------------------------------------------------
    -- Execution with Paraffin
    --------------------------------------------------
    if CLI.Argument (Number => 1) = "paraffin" then
        -- Put_Line("With Paraffin");
        Init(M_Access.all);
        --Print_Matrix(M_Access.all);
        Start_Time := Clock;
        LU_Paraffin(M_Access.all);
        Finish_Time := Clock;
        --Print_Matrix(M_Access.all);
        Put_Line("Time for paraffin: " & Duration'Image(Finish_Time - Start_Time) & " seconds.");
        Put_Line(OutFile, Duration'Image(Finish_Time - Start_Time)&",");
    end if;

--     Extrae.Ada_Extrae_fini;

end Main;


