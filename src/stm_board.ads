with System;               use System;
with STM32.Device;         use STM32.Device;
with STM32.GPIO;           use STM32.GPIO;
with STM32.Timers;         use STM32.Timers;
with STM32.ADC;            use STM32.ADC;
with Ada.Interrupts.Names; use Ada.Interrupts.Names;

package STM_Board is

   ---------------
   -- Constants --
   ---------------

   subtype Frequency_Hz is Float;

   ---------------------
   -- PWM Full-bridge --
   ---------------------

   PWM_Timer        : Timer renames Timer_1;
   --  Timer for reading sine table values.
   PWM_Interrupt    : Ada.Interrupts.Interrupt_ID renames TIM1_UP_TIM10_Interrupt;
   PWM_ISR_Priority : constant Interrupt_Priority := Interrupt_Priority'Last - 3;

   PWM_A_Channel : Timer_Channel renames Channel_1;
   PWM_A_H_Pin   : GPIO_Point renames PE9;
   PWM_A_L_Pin   : GPIO_Point renames PE8;
   PWM_A_GPIO_AF : STM32.GPIO_Alternate_Function renames GPIO_AF_TIM1_1;

   PWM_B_Channel : Timer_Channel renames Channel_2;
   PWM_B_H_Pin   : GPIO_Point renames PE11;
   PWM_B_L_Pin   : GPIO_Point renames PE10;
   PWM_B_GPIO_AF : STM32.GPIO_Alternate_Function renames GPIO_AF_TIM1_1;

   PWM_Gate_Power : GPIO_Point renames PB2;
   --  Output for the FET/IGBT gate drivers.

   ------------------------------
   -- Voltage and Current ADCs --
   ------------------------------

   Sensor_ADC           : constant access Analog_To_Digital_Converter := ADC_3'Access;
   Sensor_Trigger_Event : External_Events_Regular_Group := Timer3_CC1_Event;
   Sensor_Interrupt     : Ada.Interrupts.Interrupt_ID renames ADC_Interrupt;
   Sensor_ISR_Priority  : constant Interrupt_Priority := Interrupt_Priority'Last - 2;

   ADC_Battery_V_Point : constant ADC_Point := (Sensor_ADC, Channel => 5);
   ADC_Battery_V_Pin   : GPIO_Point renames PF7;

   ADC_Battery_I_Point : constant ADC_Point := (Sensor_ADC, Channel => 6);
   ADC_Battery_I_Pin   : GPIO_Point renames PF8;

   ADC_Output_V_Point : constant ADC_Point := (Sensor_ADC, Channel => 7);
   ADC_Output_V_Pin   : GPIO_Point renames PF9;

   ----------------
   -- ADCs Timer --
   ----------------

   --  To syncronize A/D conversion and timers, the ADCs could be triggered
   --  by any of TIM1, TIM2, TIM3, TIM4, TIM5, or TIM8 timer.
   Sensor_Timer              : Timer renames Timer_3;
   Sensor_Timer_Interrupt    : Ada.Interrupts.Interrupt_ID renames TIM3_Interrupt;
   Sensor_Timer_ISR_Priority : constant Interrupt_Priority := Interrupt_Priority'Last - 2;

   --  Channel for reading analog inputs (5 kHz, 200 us)
   Sensor_Timer_Channel : Timer_Channel renames Channel_1;
   Sensor_Timer_AF      : STM32.GPIO_Alternate_Function renames GPIO_AF_TIM3_2;
   Sensor_Timer_Point   : GPIO_Point renames PA6;
   --  Point not used because this timer only start an interrupt.

   -------------------------
   -- Other GPIO Channels --
   -------------------------

   AC_Frequency_Pin : GPIO_Point renames PG0;
   --  Input for AC frequency select jumper.

   Green_LED : GPIO_Point renames PB0; -- LD1
   --  Output for OK indication in the nucleo board.

   Blue_LED  : GPIO_Point renames PB7; -- LD2
   --  Output for problem indication in the nucleo board.

   Red_LED   : GPIO_Point renames PB14; -- LD3
   --  Output for problem indication in the nucleo board.
   LCH_LED   : GPIO_Point renames Red_LED;
   --  Last chance handler led.

   All_LEDs  : GPIO_Points := Blue_LED & Green_LED & Red_LED;

   Buzzer    : GPIO_Point renames PG1;
   --  Output for buzzer alarm.

   ------------------------------
   -- Procedures and functions --
   ------------------------------

   procedure Initialize_GPIO;
   --  Initialize GPIO inputs and outputs.

   function Read_AC_Frequency return Boolean
   with
      Pre => Is_Initialized;
   --  Read AC frequency selection into global variable.

   procedure Turn_On (This : in out GPIO_Point)
   with
      Pre => Is_Initialized and (This /= PWM_Gate_Power);
   --  Turns ON the specified output.

   procedure Turn_Off (This : in out GPIO_Point)
   with
      Pre => Is_Initialized and (This /= PWM_Gate_Power);
   --  Turns OFF the specified output.

   procedure Set_Toggle (This : in out GPIO_Point)
   with
      Pre => Is_Initialized and (This /= PWM_Gate_Power);
   --  Toggle the specified output.

   procedure All_LEDs_Off
   with
      Pre => Is_Initialized;
   --  Turns OFF all LEDs.

   procedure All_LEDs_On
   with
      Pre => Is_Initialized;
   --  Turns ON all LEDs.

   procedure Toggle_LEDs (These : in out GPIO_Points)
   with
      Pre => Is_Initialized;
   --  Toggle the specified LEDs.

   function Is_Initialized return Boolean;
   --  Returns True if the board specifics are initialized.

private

   Initialized : Boolean := False;

end STM_Board;
