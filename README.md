**ICH AC'97 MOD Player**

AC'97 MOD Player for DOS (no DPMI, no HIMEM.SYS, no AUDIO DRV, direct play)
((NASM Source Code))

; Valid ICH device IDs
valid_ids:
dd	(ICH_DID << 16) + INTEL_VID  	 ; 8086h:2415h
dd	(ICH0_DID << 16) + INTEL_VID 	 ; 8086h:2425h
dd	(ICH2_DID << 16) + INTEL_VID 	 ; 8086h:2445h
dd	(ICH3_DID << 16) + INTEL_VID 	 ; 8086h:2485h
dd	(ICH4_DID << 16) + INTEL_VID 	 ; 8086h:24C5h
dd	(ICH5_DID << 16) + INTEL_VID 	 ; 8086h:24D5h
dd	(ICH6_DID << 16) + INTEL_VID 	 ; 8086h:266Eh
dd	(ESB6300_DID << 16) + INTEL_VID  ; 8086h:25A6h
dd	(ESB631X_DID << 16) + INTEL_VID  ; 8086h:2698h
dd	(ICH7_DID << 16) + INTEL_VID 	 ; 8086h:27DEh
dd	(MX82440_DID << 16) + INTEL_VID  ; 8086h:7195h
dd	(SI7012_DID << 16)  + SIS_VID	 ; 1039h:7012h
dd 	(NFORCE_DID << 16)  + NVIDIA_VID ; 10DEh:01B1h
dd 	(NFORCE2_DID << 16) + NVIDIA_VID ; 10DEh:006Ah
dd 	(AMD8111_DID << 16) + AMD_VID 	 ; 1022h:746Dh
dd 	(AMD768_DID << 16)  + AMD_VID 	 ; 1022h:7445h
dd 	(CK804_DID << 16) + NVIDIA_VID	 ; 10DEh:0059h
dd 	(MCP04_DID << 16) + NVIDIA_VID	 ; 10DEh:003Ah
dd 	(CK8_DID << 16) + NVIDIA_VID	 ; 1022h:008Ah
dd 	(NFORCE3_DID << 16) + NVIDIA_VID ; 10DEh:00DAh
dd 	(CK8S_DID << 16) + NVIDIA_VID	 ; 10DEh:00EAh
valid_id_count:	equ ($ - valid_ids)>>2
