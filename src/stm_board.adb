
package body STM_Board is

   ---------------------
   -- Initialize_GPIO --
   ---------------------

   procedure Initialize_GPIO is
      Configuration : GPIO_Port_Configuration;
      All_PP_Outputs : constant GPIO_Points :=
         (Green_LED, Blue_LED, Red_LED, Buzzer);
   begin
      --  Output LEDs and buzzer pins
      Enable_Clock (All_PP_Outputs);
      Configuration := (Mode        => Mode_Out,
                        Output_Type => Push_Pull,
                        Speed       => Speed_100MHz,
                        Resistors   => Floating);
      Configure_IO (All_PP_Outputs, Configuration);

      --  Output gate driver pin. This depends on the electronic circuit.
      --  If the driver already has a pull-up resistor, then it is open-drain.
      Enable_Clock (PWM_Gate_Power);
      Configuration := (Mode        => Mode_Out,
                        Output_Type => Open_Drain,
                        Speed       => Speed_100MHz,
                        Resistors   => Floating);
      Configure_IO (PWM_Gate_Power, Configuration);

      --  Input frequency pin. This depends on the electronic circuit. If
      --  the pin already has pull-up resistor, then it is open-drain.
      Enable_Clock (AC_Frequency_Pin);
      Configuration := (Mode      => Mode_In,
                        Resistors => Pull_Up);
      Configure_IO (AC_Frequency_Pin, Configuration);

      Initialized := True;
   end Initialize_GPIO;

   -----------------------
   -- Read_AC_Frequency --
   -----------------------

   function Read_AC_Frequency return Boolean is
     (not AC_Frequency_Pin.Set);

   -------------
   -- Turn_On --
   -------------

   procedure Turn_On (This : in out GPIO_Point) is
   begin
      Set (This);
   end Turn_On;

   --------------
   -- Turn_Off --
   --------------

   procedure Turn_Off (This : in out GPIO_Point) is
   begin
      Clear (This);
   end Turn_Off;

   ----------------
   -- Set_Toggle --
   ----------------

   procedure Set_Toggle (This : in out GPIO_Point) is
   begin
      Toggle (This);
   end Set_Toggle;

   ------------------
   -- All_LEDs_Off --
   ------------------

   procedure All_LEDs_Off is
   begin
      Clear (All_LEDs);
   end All_LEDs_Off;

   -----------------
   -- All_LEDs_On --
   -----------------

   procedure All_LEDs_On is
   begin
      Set (All_LEDs);
   end All_LEDs_On;

   -----------------
   -- Toggle_LEDs --
   -----------------

   procedure Toggle_LEDs (These : in out GPIO_Points) is
   begin
      Toggle (These);
   end Toggle_LEDs;

   --------------------
   -- Is_Initialized --
   --------------------

   function Is_Initialized
      return Boolean is (Initialized);

end STM_Board;
