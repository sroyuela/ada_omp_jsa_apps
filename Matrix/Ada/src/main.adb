with Ada.Calendar; use Ada.Calendar;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Unchecked_Conversion;
with Interfaces.C;
with System;
with OpenMP;
with Parallel.Loops.Work_Sharing;

with Ada.Command_Line;
with Ada.Text_IO;

use Parallel;

procedure Main is

   ----------------------------------------------------
   -- Definition of constants, types and auxiliary renames and packages
   ----------------------------------------------------

   -- Simul_Load defines the number of iterations of heavy computation simulation
   Simul_Load : constant Integer := 50_000;
   -- Size defines the size of the matrix
   Size: constant Integer := 512;   -- less than 4 the procedure with Ada tasks blocks (needs to be solved)

   -- Global values to parametrize the executions
    Nargs : Integer;
    Nthreads : Integer;
    Ntasks: Integer;
    OutFile: File_Type;
   
   -- Random_Range defines the range of values for the matrix elements
   -- This impacts the variability of the heavy computation unbalanced
   subtype Random_Range is Positive range 1..4;
   package Random_Value is new Ada.Numerics.Discrete_Random(Random_Range);


   -- Definition of the Matrix types
   type Matrix_Dim is range 1 .. Size;
   type Matrix is array (Matrix_Dim, Matrix_Dim) of Float;


   --------------------------------------------------
   -- Heavy computation simulates floating point operations in a matrix element
   -- There are two variants
   --   balanced all invocations execute the same number of iterations
   --   unbalanced the number of iterations depends on the input value
   --------------------------------------------------


   procedure Heavy_Computation_Balanced (A: in out Float) is
      Res: Float := 0.0;
   begin
      for I in 1 .. Simul_Load loop
         Res := Res + A * 2.0;
      end loop;
      A := Res;
   end Heavy_Computation_Balanced;


   procedure Heavy_Computation_Unbalanced (A: in out Float) is
      Res: Float := 0.0;
      -- the goal of unbalanced is in average being equal to balanced
      New_Simul_Load: Integer := Simul_Load * (Random_Range'Last / 2) / Integer(A);
   begin
      for I in 1 .. New_Simul_Load loop
         Res := Res + A * 2.0;
      end loop;
      A := Res;
   end Heavy_Computation_Unbalanced;

   --------------------------------------------------
   -- Process iterates the matrix, calling Heavy_Computation in each I,J element
   -- There are several variants
   --   Seq is the sequential version
   --   Parallel balanced iterates in parallel with OpenMP using balanced computation
   --   Parallel unbalanced iterates in parallel with OpenMP using unbalanced computation
   --   Parallel with Ada tasks uses Ada tasks directly
   --   Paraffin uses paraffin
   --------------------------------------------------


   -- Sequential

   procedure Process_Seq (M : in out Matrix) is
   begin
      for I in Matrix_Dim loop
         for J in Matrix_Dim loop
            Heavy_Computation_Balanced(M(I, J));
         end loop;
      end loop;
   end Process_Seq;



   -- Parallel balanced

   type Matrix_Access is access all Matrix;
   type Float_Access is access all Float;


   type Task_Data_Type is
      record
          M_Address: System.Address; -- procedure uses in out parameters, so passing by pointer
          First_Pos: Positive;
          Last_Pos: Positive;
      end record;
   pragma Convention(C, Task_Data_Type);

   type Task_Data_Access_Type is access Task_Data_Type;
   pragma Convention(C, Task_Data_Access_Type);

   procedure Task_Function_Balanced(Task_Params: System.Address);
   pragma Convention(C, Task_Function_Balanced);

   procedure Task_Function_Balanced(Task_Params: System.Address) is
      function Convert_to_Task is new Ada.Unchecked_Conversion(Source=>System.Address,
                                                               Target=>Task_Data_Access_Type);
--       function Convert_to_Float is new Ada.Unchecked_Conversion(Source=>System.Address,
--                                                                 Target=>Float_Access);
      Task_Data_Access: Task_Data_Access_Type := Convert_to_Task(Task_Params);
--       M_Access: Float_Access := Convert_to_Float(Task_Data_Access.M_Address);
      function Convert_to_Matrix is new Ada.Unchecked_Conversion(Source=>System.Address,
                                                                 Target=>Matrix_Access);
      M_Access: Matrix_Access := Convert_to_Matrix(Task_Data_Access.M_Address);
      
   begin
       declare
           Local_First_Pos, Local_Last_Pos: Positive;
        begin
                -- /Put_Line("Within task with thread =" & Integer'Image(OpenMP.Ada_Omp_Get_Thread_Num)) ;
            Local_First_Pos := Task_Data_Access.First_Pos;
            Local_Last_Pos := Task_Data_Access.Last_Pos;
            for I in Local_First_Pos .. Local_Last_Pos loop
                for J in Matrix_Dim loop
                Heavy_Computation_Balanced(M_Access.all(Matrix_Dim(I), Matrix_Dim(J)));
                end loop;
            end loop;
        end;
   end Task_Function_Balanced;


   procedure Process_Parallel_Balanced_Internal (M : in out Matrix) is
      Depend_Clauses: OpenMP.Void_Ptr_Ptr := null;
   begin
--       for I in Matrix_Dim loop
       --          for J in Matrix_Dim loop
       for K in 0 .. Ntasks-1 loop
            declare
               Task_Data_Access: Task_Data_Access_Type;
               Task_Data_Address: System.Address;
               First_Pos: Positive;
               Last_Pos: Positive;
            begin
                First_Pos := K * (Size / Ntasks) + 1;
                Last_Pos := (K+1) * (Size / Ntasks);

                if K = Ntasks then
                    Last_Pos := Integer(Size);
                end if;
         
               Task_Data_Access := new Task_Data_Type;
               Task_Data_Access.M_Address := M'Address;
               Task_Data_Access.First_Pos := First_Pos;
               Task_Data_Access.Last_Pos := Last_Pos;
               Task_Data_Address := Task_Data_Access.all'Address;
               OpenMP.Ada_GOMP_Task(Task_Function_Balanced'Unrestricted_Access, Task_Data_Address, null, 16, 4, TRUE, 0, Depend_Clauses, 0);
            end;
       end loop;
--          end loop;
--       end loop;
      OpenMP.Ada_GOMP_Taskwait;
   end Process_Parallel_Balanced_Internal;


   type Parallel_Data_Type is record
      M_Address: System.Address;
   end record;
   pragma Convention(C, Parallel_Data_Type);
   type Parallel_Data_Type_Access is access all Parallel_Data_Type;
   pragma Convention(C, Parallel_Data_Type_Access);

   procedure Parallel_Function_Balanced(Parallel_Params: System.Address);
   pragma Convention(C, Parallel_Function_Balanced);

   procedure Parallel_Function_Balanced(Parallel_Params: System.Address) is
      function Convert_to_Parallel is new Ada.Unchecked_Conversion(Source=>System.Address,
                                                                   Target=>Parallel_Data_Type_Access);
      function Convert_to_Matrix is new Ada.Unchecked_Conversion(Source=>System.Address,
                                                                 Target=>Matrix_Access);
      Data_Access: Parallel_Data_Type_Access := Convert_to_Parallel(Parallel_Params);
      M_Access: Matrix_Access := Convert_to_Matrix(Data_Access.M_Address);
   begin

      if (OpenMP.Ada_OMP_Get_Thread_Num = 0) then
         Process_Parallel_Balanced_Internal(M_Access.all);
      end if;

   end Parallel_Function_Balanced;



   procedure Process_Parallel_Balanced (M : in out Matrix) is
       Parallel_Data : Parallel_Data_Type;
        function Convert_to_C is new Ada.Unchecked_Conversion(Source=>Integer,
                                                              Target=>Interfaces.C.unsigned);
        Nthreads_C : Interfaces.C.unsigned := Convert_to_C(Nthreads);
   begin
      Parallel_Data.M_Address := M'Address;

      OpenMP.Ada_GOMP_parallel_start(Parallel_Function_Balanced'Unrestricted_Access, Parallel_Data'Address, Nthreads_C);

      Parallel_Function_Balanced(Parallel_Data'Address);
      --Put_Line("Calling end ...");
      OpenMP.Ada_GOMP_parallel_end;

   end Process_Parallel_Balanced;



   -------------------------------------------------------
   -- parallel unbalanced
   -------------------------------------------------------


--    procedure Task_Function_Unbalanced(Task_Params: System.Address);
--    pragma Convention(C, Task_Function_Unbalanced);
-- 
--    procedure Task_Function_Unbalanced(Task_Params: System.Address) is
--       function Convert_to_Task is new Ada.Unchecked_Conversion(Source=>System.Address,
--                                                                Target=>Task_Data_Access_Type);
--       function Convert_to_Float is new Ada.Unchecked_Conversion(Source=>System.Address,
--                                                                 Target=>Float_Access);
--       Task_Data_Access: Task_Data_Access_Type := Convert_to_Task(Task_Params);
--       F_Access: Float_Access := Convert_to_Float(Task_Data_Access.F_Address);
--    begin
-- 
-- --       Put_Line("Within task with thread =" & Integer'Image(OpenMP.Ada_Omp_Get_Thread_Num)) ;
--        Heavy_Computation_Unbalanced(F_Access.all);
--    end Task_Function_Unbalanced;
-- 
-- 
--    procedure Process_Parallel_Unbalanced_Internal (M : in out Matrix) is
--       Depend_Clauses: OpenMP.Void_Ptr_Ptr := null;
--    begin
--       for I in Matrix_Dim loop
--          for J in Matrix_Dim loop
--             declare
--                Task_Data_Access: Task_Data_Access_Type;
--                Task_Data_Address: System.Address;
--             begin
--                Task_Data_Access := new Task_Data_Type;
--                Task_Data_Access.all.F_Address := M(I,J)'Address;
--                Task_Data_Address := Task_Data_Access.all'Address;
--                OpenMP.Ada_GOMP_Task(Task_Function_Unbalanced'Unrestricted_Access, Task_Data_Address, null, 8, 4, TRUE, 0, Depend_Clauses, 0);
--             end;
--          end loop;
--       end loop;
--       OpenMP.Ada_GOMP_Taskwait;
--    end Process_Parallel_Unbalanced_Internal;
-- 
-- 
--    procedure Parallel_Function_Unbalanced(Parallel_Params: System.Address);
--    pragma Convention(C, Parallel_Function_Unbalanced);
-- 
--    procedure Parallel_Function_Unbalanced(Parallel_Params: System.Address) is
--       function Convert_to_Parallel is new Ada.Unchecked_Conversion(Source=>System.Address,
--                                                                    Target=>Parallel_Data_Type_Access);
--       function Convert_to_Matrix is new Ada.Unchecked_Conversion(Source=>System.Address,
--                                                                  Target=>Matrix_Access);
--       Data_Access: Parallel_Data_Type_Access := Convert_to_Parallel(Parallel_Params);
--       M_Access: Matrix_Access := Convert_to_Matrix(Data_Access.M_Address);
--    begin
-- 
--       if (OpenMP.Ada_OMP_Get_Thread_Num = 0) then
--          Process_Parallel_Unbalanced_Internal(M_Access.all);
--       end if;
-- 
--    end Parallel_Function_Unbalanced;
-- 
-- 
-- 
--    procedure Process_Parallel_Unbalanced (M : in out Matrix) is
--       Parallel_Data : Parallel_Data_Type;
--       Num_Threads: Interfaces.C.unsigned := 4;
--    begin
--       Parallel_Data.M_Address := M'Address;
-- 
--       OpenMP.Ada_GOMP_parallel_start(Parallel_Function_Unbalanced'Unrestricted_Access, Parallel_Data'Address, Num_Threads);
-- 
--       Parallel_Function_Unbalanced(Parallel_Data'Address);
--       Put_Line("Calling end ...");
--       OpenMP.Ada_GOMP_parallel_end;
-- 
--    end Process_Parallel_Unbalanced;




   -- with Ada tasks

   -- to be fair with OpenMP, tasks are created inside the timing measurement
   -- like the openmp thread pool
   -- SRA: Now OpenMP also defines different number of blocks depending on Ntasks
   -- however it is not fair because the task is statically divided in Ntasks
   -- while the openMP has the extra overhead of the thousands of tasks


   procedure Process_Tasks (M : in out Matrix) is

      task type Process is
         entry Start (First_Pos, Last_Pos: Positive);
      end Process;

      task body Process is
         Local_First_Pos, Local_Last_Pos: Positive;
      begin
         accept  Start (First_Pos, Last_Pos: Positive) do
            Local_Last_Pos := Last_Pos;
            Local_First_Pos := First_Pos;
         end Start;
         for I in Local_First_Pos .. Local_Last_Pos loop
            for J in Matrix_Dim loop
               Heavy_Computation_Balanced(M(Matrix_Dim(I), Matrix_Dim(J)));
            end loop;
         end loop;
      end Process;

      First_Pos, Last_Pos: Positive;

      Process_Tasks: array (1..Ntasks) of Process;

   begin
      for I in 0..Ntasks-1 loop
         First_Pos := I * (Size / Ntasks)+1;
         Last_Pos := (I+1) * (Size / Ntasks);
         if I = Ntasks then
           Last_Pos := Integer(Matrix_Dim'Last);
         end if;

         Process_Tasks(I+1).Start(First_Pos, Last_Pos);

      end loop;
   end Process_Tasks;


   -- Paraffin
   -- not quite fair, since paraffin does not create one tasklet per individual matrix element
   -- instead it divides each row in blocks
   -- so it is equivalent to an approach where each openmp tasks processes a block of elements

   procedure Process_Paraffin (M : in out Matrix) is

      package Parallel_Loops is new Parallel.Loops (Matrix_Dim);
      package Iterate is new Parallel_Loops.Work_Sharing;

      procedure Generic_Iterate (Start, Finish: Matrix_Dim; Row: Matrix_Dim) is
      begin
      --   Put_Line("Row: " & Integer'Image(Integer(Row)) & ", Start: " & Integer'Image(Integer(Start)) & ", Finish: " & Integer'Image(Integer(Finish)));

         for I in Start..Finish loop
            Heavy_Computation_Balanced(M(Row, I));
         end loop;

      end Generic_Iterate;

      procedure Process_Row(Row: Matrix_Dim) is
          Manager : Iterate.Work_Sharing_Manager := Iterate.Create ;
        function Convert_to_WorkerCount is new Ada.Unchecked_Conversion(Source=>Integer,
                                                                        Target=>Worker_Count_Type);
        Nthreads_WC : Worker_Count_Type := Convert_to_WorkerCount(Nthreads);
        
         procedure Iteration(Start, Finish: Matrix_Dim) is
         begin
            Generic_Iterate(Start, Finish, Row);
         end Iteration;
      begin
         Manager.Execute_Parallel_Loop
           (Process => Iteration'Access,
            Worker_Count => Nthreads_WC);
      end Process_Row;


   begin

      for I in Matrix_Dim loop
         Process_Row(I);
      end loop;



   end Process_Paraffin;


   --------------------------------------------------
   -- Auxiliary subprograms
   -- Init initializes the matrix with random values
   -- Print prints the matrix
   --------------------------------------------------


   procedure Init (M: out Matrix) is
      Gen: Random_Value.Generator;
   begin
      Random_Value.Reset(Gen);
      for I in Matrix_Dim loop
         for J in Matrix_Dim loop
            M(I, J) := Float(Random_Value.Random(Gen));
         end loop;
      end loop;
   end Init;


   procedure Print_Matrix (M: Matrix) is
   begin
      for I in Matrix_Dim loop
         for J in Matrix_Dim loop
            Put (M(I,J));
            Put(",");
         end loop;
         New_Line;
      end loop;
   end Print_Matrix;


   --------------------------------------------------
   -- Definition of program variables
   --------------------------------------------------


   -- Matrix
   M: Matrix;

   -- Time variables
   Start_Time, Finish_Time: Time;

    package IO renames Ada.Text_IO;
    package CLI renames Ada.Command_Line;
    
begin

    -- Read arguments
    Nargs := CLI.Argument_Count;
    if Nargs /= 4 then
        IO.Put_Line ("Use: ./main version n_threads n_tasks out_file.csv");
        return;
    end if;

    Nthreads := Integer'Value(CLI.Argument (Number => 2));
    Ntasks := Integer'Value(CLI.Argument (Number => 3));

    begin
        Open (File => OutFile,
              Mode => Append_File,
              Name => CLI.Argument (Number => 4));
        exception
      when Name_Error =>
         Create (
            File => OutFile,
            Mode => Append_File,
            Name => CLI.Argument (Number => 4));
--        when others =>
--            Put_Line (Standard_Error,
--                    "Can not create a file named '" & CLI.Argument (Number => 4) & "'.");
--            CLI.Set_Exit_Status (CLI.Failure);
--            return;
    end;

    -- Execution of Sequential
    if CLI.Argument (Number => 1) = "seq" then
        Init(M);

        Start_Time := Clock;
        Process_Seq (M);
        Finish_Time := Clock;

        Put_Line("Time for sequential: " & Duration'Image(Finish_Time - Start_Time) & " seconds.");
        Put_Line(OutFile, Duration'Image(Finish_Time - Start_Time)&",");
    end if;

    -- Execution of Parallel balanced
    if CLI.Argument (Number => 1) = "omp" then
        Init(M);
        --Print_Matrix(M);
        Start_Time := Clock;
        Process_Parallel_Balanced (M);
        Finish_Time := Clock;
        Put_Line("Time for parallel balanced: " & Duration'Image(Finish_Time - Start_Time) & " seconds.");
        Put_Line(OutFile, Duration'Image(Finish_Time - Start_Time)&",");
    end if;

   --Print_Matrix(M);

   -- Execution of Parallel unbalanced
--     Init(M);
--     Start_Time := Clock;
--     Process_Parallel_Unbalanced (M);
--     Finish_Time := Clock;
--     Put_Line("Time for parallel unbalanced: " & Duration'Image(Finish_Time - Start_Time) & " seconds.");


    -- Execution of Parallel with Ada tasks
    if CLI.Argument (Number => 1) = "tasks" then
        Init(M);
        Start_Time := Clock;
        Process_Tasks (M);
        Finish_Time := Clock;
        Put_Line("Time for parallel with Ada tasks: " & Duration'Image(Finish_Time - Start_Time) & " seconds.");
        Put_Line(OutFile, Duration'Image(Finish_Time - Start_Time)&",");
    end if;

    -- Execution with paraffin
    if CLI.Argument (Number => 1) = "paraffin" then
        Init(M);
        Start_Time := Clock;
        Process_Paraffin (M);
        Finish_Time := Clock;

        Put_Line("Time for paraffin: " & Duration'Image(Finish_Time - Start_Time) & " seconds.");
        Put_Line(OutFile, Duration'Image(Finish_Time - Start_Time)&",");
    end if;
   --Print_Matrix(M);

   Close (OutFile);

end Main;
