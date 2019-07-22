
//
//  Программа отображения диалога _Dlg_
//

(prog nil

      (try (dlgDestroy '_Dlg_) except Nil)


      (dlgCreate '_Dlg_ 1200 400 "" &H8000000F)

      (dlgAddControl '_Dlg_ '_BUT_1 _BUTTON 902 23 273 128 '("Tahoma" 20,25 0 0 0) "Выход")
      (dlgPutPicture '_BUT_1 36)

      (dlgAddControl '_Dlg_ '_TXT_1 _TEXT 12 50 550 112 '("Tahoma" 14 0 0 0) "" 0 &H80000008 &H80000005)

      (dlgAddControl '_Dlg_ '_BUT_2 _BUTTON 592 23 273 128 '("Tahoma" 21 0 0 0) "Выполнить")
      (dlgPutPicture '_BUT_2 7)

      (dlgAddControl '_Dlg_ '_LBL_1 _LABEL 12 10 396 30 '("Tahoma" 15 0 0 0) "Введите признаки животного" 0 &H80000012 &H8000000F)

      (dlgAddControl '_Dlg_ '_LBL_2 _LABEL 12 170 394 30 '("Tahoma" 15 0 0 0) "Результат:" 0 &H80000012 &H8000000F)

      (dlgAddControl '_Dlg_ '_LST_1 _LIST 13 201 1150 128 '("Tahoma" 10 0 0 0) &H80000008 &H80000005)

      //
      // Пролог загрузки диалога _Dlg_
      //

      (Prog () 

      )


      //
      // Обработчик события CLICK для кнопки _BUT_1
      //

      (defun _BUT_1_Click  Nil 
      	(dlghide '_dlg_)
      	(dlgdestroy '_dlg_)
      )


      //
      //   Назначение процедуры-события _BUT_1_Click  контролу _BUT_1
      //

      (dlgSetEvent '_BUT_1 '_BUT_1_Click )

      //
      // Обработчик события KEYPRESSED для области ввода _TXT_1
      //

      (defun _TXT_1_KeyPress  (KeyAscii) 
      	T

      )


      //
      //   Назначение процедуры-события _TXT_1_KeyPress  контролу _TXT_1
      //

      (dlgSetEvent '_TXT_1 '_TXT_1_KeyPress )

      //
      // Обработчик события CLICK для кнопки _BUT_2
      //

      (defun _BUT_2_Click  Nil 
	    (dlgClearList '_LST_1) 
      	(mapcar (lambda(a) (dlgadditem '_LST_1 (output a))) (outputAnimals (define_ (dlggettext '_TXT_1))))

      )


      //
      //   Назначение процедуры-события _BUT_2_Click  контролу _BUT_2
      //

      (dlgSetEvent '_BUT_2 '_BUT_2_Click )


      //
      //   Отображение диалога _Dlg_
      //

      (dlgShow '_Dlg_)
)
