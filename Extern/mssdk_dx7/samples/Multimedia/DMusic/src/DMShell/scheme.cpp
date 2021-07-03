// Scheme.cpp : implementation file
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
/////////////////////////////////////////////////////////////////////////////

#include "main.h"



CTemplateIndex::CTemplateIndex(HWND hWnd, WORD wTemplate)
{
	m_hWnd = hWnd;
	m_wTemplate = wTemplate;
	m_pNextIndex = NULL;
}

CTemplateIndex::~CTemplateIndex()
{
}

CTemplateMapper::CTemplateMapper()
{
	m_pFirstIndex = NULL;
}

CTemplateMapper::~CTemplateMapper()
{
	DeleteAll();
}

WORD CTemplateMapper::GetTemplate(HWND hWnd)
{
	// Scan through list looking for hWnd
	CTemplateIndex** ppTI = &m_pFirstIndex;
	while (*ppTI != NULL)
	{
		if ( (*ppTI)->m_hWnd == hWnd )
		{
			return (*ppTI)->m_wTemplate;
		}
		ppTI = &( (*ppTI)->m_pNextIndex );
	}

	//hWnd is new, generate optimal template number and create a new index entry
	WORD wTemplate = PickNewTemplate();
	CTemplateIndex* pTI = new CTemplateIndex(hWnd, wTemplate);
	*ppTI = pTI;
	return wTemplate;
}

WORD CTemplateMapper::PickNewTemplate()
{
	WORD wTemplHistogram[NUM_TEMPLATES];
	for (WORD x = 0; x < NUM_TEMPLATES; x++)
		wTemplHistogram[x] = 0;

	//Fill the histogram array with the number of open apps using each template
	CTemplateIndex* pTI = m_pFirstIndex;
	while (pTI != NULL)
	{
		if ( pTI->m_wTemplate < NUM_TEMPLATES )
			wTemplHistogram[pTI->m_wTemplate]++;

		pTI = pTI->m_pNextIndex;
	}

	//Determine the lowest level in the histogram
	WORD wLowestReferenceCount = 0xFFFF;
	for (x = 0; x < NUM_TEMPLATES; x++)
	{
		if ( wTemplHistogram[x] < wLowestReferenceCount)
			wLowestReferenceCount = wTemplHistogram[x];
	}

	//Fill an array with the template numbers having the lowest number of app references
	WORD wNumTemplsWithLowestRefCount = 0;
	WORD wOptimalTemplSelections[NUM_TEMPLATES];
	for (x = 0; x < NUM_TEMPLATES; x++)
	{
		if ( wTemplHistogram[x] == wLowestReferenceCount)
		{
			wOptimalTemplSelections[wNumTemplsWithLowestRefCount] = x;
			wNumTemplsWithLowestRefCount++;
		}
	}

	//error conditions
	if (wNumTemplsWithLowestRefCount == 0 || wNumTemplsWithLowestRefCount > NUM_TEMPLATES)
		wNumTemplsWithLowestRefCount = NUM_TEMPLATES;

	
	//Randomly select between the templates with the least number of references
	WORD wTemplateNum;
	if (wNumTemplsWithLowestRefCount == 1)
	{
		//Choice is obvious
		wTemplateNum = wOptimalTemplSelections[0];
	}
	else
	{
		SYSTEMTIME sysTime;
		GetSystemTime( &sysTime);
		srand ( sysTime.wMilliseconds);
		WORD wTemplateIndex = rand() % wNumTemplsWithLowestRefCount ;
		wTemplateNum = wOptimalTemplSelections[wTemplateIndex];
	}
	
	//Disaster protection
	if (wTemplateNum >= NUM_TEMPLATES)
		wTemplateNum = 0;
	
	return wTemplateNum;
}

void CTemplateMapper::DeleteAll()
{
	CTemplateIndex* pTICurrent = m_pFirstIndex;
	CTemplateIndex* pTINext = NULL;
	while (pTICurrent != NULL)
	{
		pTINext = pTICurrent->m_pNextIndex;
		delete pTICurrent;
		pTICurrent = pTINext;
	}
}

void CTemplateMapper::DeleteWin(HWND hWnd)
{
	return;
	CTemplateIndex* pTIPrevious = NULL;
	CTemplateIndex* pTICurrent = m_pFirstIndex;
	CTemplateIndex* pTINext = NULL;
	while (pTICurrent != NULL)
	{
		pTINext = pTICurrent->m_pNextIndex;
		if (pTICurrent->m_hWnd == hWnd)
		{
			if (pTIPrevious == NULL)
			{
				delete pTICurrent;
				m_pFirstIndex = pTINext;
			}
			else
			{
				delete pTICurrent;
				pTIPrevious->m_pNextIndex = pTINext;
			}
		}
	}
}
