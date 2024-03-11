--  This program calculates the sinus value of a point. The table for
--  pwm sine generation is produced knowing the number of points to
--  complete 1/4 sine period. The semi-sinusoid, or 1/2 sine period
--  is completed with these same points in reverse order.
--  The equation which defines each point is:
--
--  D = A * sin(pi/2 * x/N)
--  D = Duty cycle at a given discrete point;
--  A = Signal amplitude of the maximum duty cycle. We adopt 10,000 for integer
--      values and 1 for floating values.
--  pi/2 = 1/4 of the sine period
--  x = Step number
--  N = Number of points. We adopt 125.

With Ada.Text_IO;                       Use Ada.Text_IO;
With Ada.Integer_Text_IO;               Use Ada.Integer_Text_IO;
With Ada.Numerics.Elementary_Functions; Use Ada.Numerics.Elementary_Functions;
                                        Use Ada.Numerics;

Procedure Sine_Table is
   --  Declaration and description of variables
   D  : integer; --  Value of point
   E  : float; --  Value of point
   A  : integer := 10_000; --  Signal amplitude of the maximum duty cycle
   N  : integer := 125; --  Number of points for 1/4 of the sine period
   F  : File_Type; --  Output file

begin
   --  Creating the output file
   begin
      Create(file => F, mode => out_file, name => "sine_table.txt");
   exception
      when Name_Error =>
         Put_Line("Write error in the output file.");
         New_Line;
   end;

   --  Collecting input data
   Put_Line("Put an integer for the amplitude of the maximum duty cycle [10000]:");
   Get(A);
   Put_Line("Put the number of points for 1/4 sine period [125]:");
   Get(N);

   --  Header of the file
   Put(F, "This program calculates the sine value of a point");
   New_Line(F);
   Put(F, "considering" & Integer'Image(2*N) & " points for half sine period");
   New_Line(F);
   Put(F, "and with a maximum amplitude of" & Integer'Image(A));
   New_Line(F);
   New_Line(F);

   --  Calculation of integer point values
   for x in 1..N loop
      D := Integer(float(A) * sin(Pi/2.0 * float(x)/float(N)));
      Put(F, Integer'Image(D) & ",");
      if ((x rem 10) = 0) then --  10 values by line
         New_Line(F);
      end if;
   end loop;
   for x in reverse 0..N-1 loop
      D := Integer(float(A) * sin(Pi/2.0 * float(x)/float(N)));
      Put(F, Integer'Image(D) & ",");
      if ((x rem 10) = 0) then --  10 values by line
         New_Line(F);
      end if;
   end loop;

   --  Now for generation of floating numbers from 0 to 1.0 we make the maximum
   --  amplitude A equal to 1.

   New_Line(F);
   --  Header of the file
   Put(F, "This program calculates the sine value of a point");
   New_Line(F);
   Put(F, "considering" & Integer'Image(2*N) & " points for half sine period");
   New_Line(F);
   Put(F, "and with a maximum amplitude of 1.0");
   New_Line(F);
   New_Line(F);

   --  Calculation of floating point values
   for x in 1..N loop
      E := sin(Pi/2.0 * float(x)/float(N));
      Put(F, Float'Image(E) & ",");
      if ((x rem 5) = 0) then --  5 values by line
         New_Line(F);
      end if;
   end loop;
   for x in reverse 0..N-1 loop
      E := sin(Pi/2.0 * float(x)/float(N));
      Put(F, Float'Image(E) & ",");
      if ((x rem 5) = 0) then --  5 values by line
         New_Line(F);
      end if;
   end loop;

end Sine_Table;
