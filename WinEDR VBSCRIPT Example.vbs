Set W = CreateObject("WinEDR.AUTO")
'W.test
'W.newfile "c:\data\testxx1.edr"
W.StimulusProtocol = "wave (sine)"
'W.StartStimulus
'W.RecordDuration = 20.0 
'W.startrecording
'W.HoldingVoltage = -0.05
'W.Numtriggersweeps = 3

'W.DACChannel = 1 
'W.HoldingVoltage = -0.06
W.startsealtest
W.sealtestpulseduration = 0.025
W.sealtestpulseamplitude = -0.015
W.holdingvoltage = 0.01
msgbox("status= " & W.Status)
W.stopsealtest
msgbox("status= " & W.Status)
msgbox("status= " & W.Status)
msgbox("status= " & W.Status)
msgbox("status= " & W.Status)

W.holdingvoltage = -0.01

