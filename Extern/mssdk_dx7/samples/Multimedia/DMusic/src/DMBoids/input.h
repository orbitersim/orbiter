/*
**----------------------------------------------------------------------------
**
**  File:       input.h
**  Purpose:    DirectInput.
**  Notes:
**
**	Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
**----------------------------------------------------------------------------
*/

extern void WINAPI	DI_Term(void); 
extern BOOL WINAPI	DI_Init(void); 
extern void WINAPI	ProcessKBInput(void);

typedef struct t_keys {
	int	separation;
	int	alignment;
	int	cohesion;
	int	migratory;
	int	obstacle;
} Keys;

extern Keys keys;


